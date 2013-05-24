%% Copyright (c) 2013 Heroku <mononcqc@ferd.ca>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(logplex_tcpsyslog_drain).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).
-define(RECONNECT_MSG, reconnect).
-define(TARGET_SEND_SIZE, 4096).
-define(SEND_TIMEOUT_MSG, send_timeout).
-define(SEND_TIMEOUT, timer:seconds(4)).

-include("logplex.hrl").
-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                host :: string() | inet:ip_address() | binary(),
                port :: inet:port_number(),
                sock = undefined :: 'undefined' | inet:socket(),
                %% Buffer for messages while disconnected
                buf :: pid(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% TCP failures since last_good_time
                failures = 0 :: non_neg_integer(),
                %% Reconnect timer reference
                reconnect_tref = undefined :: 'undefined' | reference(),
                %% Send timer reference
                send_tref = undefined :: 'undefined' | reference(),
                %% Time of last successful connection
                connect_time :: 'undefined' | erlang:timestamp(),
                %% Buffered items to send
                out_q = queue:new() :: queue(),
                drop_info :: {erlang:timestamp(), pos_integer()}
               }).

-record(frame, {frame :: iolist(),
                msg_count = 0 :: non_neg_integer(),
                loss_count = 0 :: non_neg_integer(),
                tries = 0 :: non_neg_integer()
               }).

-type pstate() :: 'disconnected' | 'ready_to_send' | 'sending'.


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/5
         ,resize_msg_buffer/2
         ,set_target_send_size/2
        ]).

-export([valid_uri/1
         ,uri/2
         ,start_link/4
        ]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([disconnected/2,
         connected/2
         ]).

-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ChannelID, DrainID, DrainTok,
           #ex_uri{scheme="syslog",
                   authority=#ex_uri_authority{host=Host, port=Port}}) ->
    start_link(ChannelID, DrainID, DrainTok, Host, Port).

start_link(ChannelID, DrainID, DrainTok, Host, Port) ->
    gen_fsm:start_link(?MODULE,
                       [#state{drain_id=DrainID,
                               drain_tok=DrainTok,
                               channel_id=ChannelID,
                               host=Host,
                               port=Port}],
                       []).

valid_uri(#ex_uri{scheme="syslog",
                  authority=#ex_uri_authority{host=Host, port=Port}} = Uri)
  when is_list(Host), is_integer(Port),
       0 < Port andalso Port =< 65535 ->
    {valid, tcpsyslog, Uri};
valid_uri(#ex_uri{scheme="syslog",
                  authority=A=#ex_uri_authority{host=Host,
                                                port=undefined}} = Uri)
  when is_list(Host) ->
    {valid, tcpsyslog,
     Uri#ex_uri{authority=A#ex_uri_authority{port=601}}};
valid_uri(_) ->
    {error, invalid_tcpsyslog_uri}.

-spec uri(Host, Port) ->
                 #ex_uri{}
                     when Host :: iolist(),
                          Port :: 'undefined' | non_neg_integer().
uri(Host, undefined) ->
    uri(Host, 514);
uri(Host, Port) when is_binary(Host), is_integer(Port) ->
    uri(binary_to_list(Host), Port);
uri(Host, Port) when is_list(Host), is_integer(Port) ->
    #ex_uri{scheme="syslog",
            authority=#ex_uri_authority{host=Host, port=Port}}.

resize_msg_buffer(Pid, NewSize)
  when is_integer(NewSize), NewSize > 0 ->
    gen_fsm:sync_send_all_state_event(Pid, {resize_msg_buffer, NewSize}).

set_target_send_size(Pid, NewSize)
  when is_integer(NewSize), NewSize > 0 ->
    gen_fsm:sync_send_all_state_event(Pid, {set_target_send_size, NewSize}).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

%% @private
init([State0=#state{sock = undefined, host=H, port=P,
                    drain_id=DrainId}]) when H =/= undefined, is_integer(P) ->
    process_flag(trap_exit, true),
    try
        logplex_drain:register(DrainId, tcpsyslog, {H,P}),
        State = start_drain_buffer(State0),
        ?INFO("drain_id=~p channel_id=~p dest=~s at=spawn",
              log_info(State, [])),
        {ok, disconnected,
         State, hibernate}
    catch
        error:badarg -> ignore
    end.

%% @doc Disconnected state. We wait here for the reconnect timer to
%% fire before initiating the reconnect sequence.
disconnected({timeout, TRef, ?RECONNECT_MSG},
             State = #state{reconnect_tref = TRef, sock = undefined}) ->
    do_reconnect(State#state{reconnect_tref=undefined});
disconnected({timeout, Received, ?RECONNECT_MSG},
             State = #state{reconnect_tref = Expected}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_reconnect "
          "expected=~p received=~p state=disconnected",
          log_info(State, [Expected, Received])),
    reconnect(State);
disconnected({logplex_drain_buffer, Buf, new_data}, State=#state{buf=Buf}) ->
    reconnect(State);
disconnected({logplex_drain_buffer, Buf, {frame, Frame, MsgCount, Lost}},
             State=#state{buf=Buf}) ->
    reconnect(push_frame(Frame, MsgCount, Lost, State));
disconnected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~1000p state=disconnected",
          log_info(State, [Msg])),
    {next_state, disconnected, State}.

connected({logplex_drain_buffer, Buf, new_data}, State = #state{buf=Buf}) ->
    send(State);
connected({logplex_drain_buffer, Buf, {frame, Frame, MsgCount, Lost}},
               State = #state{buf=Buf}) ->
    send(push_frame(Frame, MsgCount, Lost, State));
connected(Msg, State = #state{sock = Sock})
  when is_port(Sock) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=ready_to_send",
          log_info(State, [Msg])),
    {next_state, connected, State}.


%% @private
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
handle_sync_event({set_target_send_size, Size}, _From, StateName,
                  State = #state{})
  when is_integer(Size), Size > 0 ->
    put(target_send_size, Size),
    {reply, {ok, Size}, StateName, State};
handle_sync_event({resize_msg_buffer, NewSize}, _From, StateName,
                  State = #state{buf = Buf})
  when is_integer(NewSize), NewSize > 0 ->
    logplex_drain_buffer:resize_msg_buffer(Buf, NewSize),
    {reply, ok, StateName, State};
handle_sync_event(Event, _From, StateName, State) ->
    ?WARN("[state ~p] Unexpected event ~p",
          [StateName, Event]),
    {next_state, StateName, State}.

%% @private
handle_info({tcp, Sock, Data}, StateName,
            State = #state{sock = Sock}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s state=~p "
          "err=unexpected_peer_data data=~p",
          log_info(State, [StateName, Data])),
    {next_state, StateName, State};
handle_info({tcp_error, Sock, Reason}, StateName,
            State = #state{sock = Sock}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
         "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(State, [StateName, Reason, Sock, duration(State)])),
    reconnect(tcp_bad(State));
handle_info({inet_reply, Sock, {error, Reason}}, StateName,
            State = #state{sock = Sock}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
         "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(State, [StateName, Reason, Sock, duration(State)])),
    reconnect(tcp_bad(State));
handle_info({tcp_closed, Sock}, StateName,
            State = #state{sock = Sock}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s state=~p "
          "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(State, [StateName, closed, Sock, duration(State)])),
    reconnect(tcp_bad(State));
handle_info(shutdown, StateName, State = #state{sock = Sock})
  when is_port(Sock) ->
    catch gen_tcp:close(Sock),
    ?INFO("drain_id=~p channel_id=~p dest=~s state=~p "
          "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(State, [StateName, shutdown, Sock, duration(State)])),
    {stop, {shutdown,call}, State#state{sock = undefined}};
handle_info(shutdown, _StateName, State) ->
    {stop, {shutdown,call}, State};
handle_info({'EXIT', Buf, _Reason}, StateName, State = #state{buf=Buf}) ->
    {next_state, StateName, start_drain_buffer(State)};
handle_info(Info, StateName, State) ->
    ?MODULE:StateName(Info, State).

%% @private
code_change(v67, OldStateName, OldState, _Extra) ->
    13 = tuple_size(OldState),
    StateL = tuple_to_list(OldState),
    State0 = list_to_tuple(StateL ++ [queue:new(),undefined]),
    %% Prepare to have the new buffer checked
    process_flag(trap_exit, true),
    %% Unregister the channel and let the new buffer handle it
    %% remain registered as a drain
    Chan = {channel, State0#state.channel_id},
    logplex_channel:unregister(Chan),
    %% Convert messages to the new format and send them back
    State = #state{buf=Pid} = start_drain_buffer(State0#state{buf=undefined}),
    [logplex_drain_buffer:post(Pid, Msg)
     || Msg <- logplex_msg_buffer:to_list(State0#state.buf)],
    %% We'll have lost a few messages to updating -- they're probably in the
    %% mailbox right now, but oh well.
    StateName = case OldStateName of
        disconnected -> disconnected;
        sending -> connected;
        ready_to_send -> connected
    end,
    {ok, StateName, State};

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% @private
terminate(_Reason, _StateName, _State) ->
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @private
%% @doc Time has finally come to reconnect. Attempt the reconnection,
%% send buffered messages on success, schedule a delayed reconnect if
%% not.
-spec do_reconnect(#state{}) ->
                          {next_state, pstate(), #state{}}.
do_reconnect(State = #state{sock = undefined,
                            reconnect_tref = undefined,
                            failures = Failures}) ->
    case connect(State) of
        {ok, Sock} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s "
                  "state=disconnected at=connect try=~p sock=~p",
                  log_info(State, [Failures + 1, Sock])),
            NewState = State#state{sock=Sock,
                                   reconnect_tref = undefined,
                                   send_tref = undefined,
                                   connect_time=os:timestamp()},
            send(NewState);
        {error, Reason} ->
            NewState = tcp_bad(State),
            case Failures of
                0 ->
                    %% Reduce log volume by skipping logging on
                    %% first failure.
                    ok;
                _ ->
                    ?ERR("drain_id=~p channel_id=~p dest=~s at=connect "
                         "err=gen_tcp data=~p try=~p last_success=~s "
                         "state=disconnected",
                         log_info(State, [Reason, NewState#state.failures,
                                          time_failed(NewState)]))
            end,
            reconnect(NewState)
    end.

%% @private
connect(#state{sock = undefined, host=Host, port=Port})
    when is_integer(Port), 0 < Port, Port =< 65535 ->
    SendTimeoutS = logplex_app:config(tcp_syslog_send_timeout_secs),
    HostS = case Host of
                B when is_binary(B) -> binary_to_list(B);
                L when is_list(L) -> L;
                T when is_tuple(T) -> T;
                A when is_atom(A) -> A
            end,
    Options = [binary
               %% We don't expect data, but why not.
               ,{active, true}
               ,{exit_on_close, true}
               ,{keepalive, true}
               ,{packet, raw}
               ,{reuseaddr, true}
               ,{linger, {true,1}}
              ],
    gen_tcp:connect(HostS, Port, Options,
                    timer:seconds(SendTimeoutS));
connect(#state{}) ->
    {error, bogus_port_number}.


-spec reconnect(#state{}) -> {next_state, pstate(), #state{}}.
%% @private
reconnect(State = #state{reconnect_tref = Ref}) when is_reference(Ref) ->
    %% Reconnect timer was set
    case erlang:read_timer(Ref) of
        false ->
            %% and has expired
            reconnect(State#state{reconnect_tref=undefined});
        _ ->
            %% and is still valid
            {next_state, disconnected, State}
    end;
reconnect(State = #state{failures = 0, last_good_time=undefined}) ->
    %% First reconnect ever
    %% Skip straight through to reconnection code.
    do_reconnect(State);
reconnect(State = #state{failures = 0, last_good_time=T})
  when is_tuple(T), tuple_size(T) =:= 3 ->
    Min = logplex_app:config(tcp_syslog_reconnect_min, 30),
    SecsSinceConnect = timer:now_diff(os:timestamp(), T) div 1000000,
    case SecsSinceConnect of
        TooFew when TooFew < Min ->
            {next_state, disconnected,
             reconnect_in(timer:seconds(Min), State)};
        _EnoughTime ->
            do_reconnect(State)
    end;
reconnect(State = #state{failures = F}) ->
    Max = logplex_app:config(tcp_syslog_backoff_max, 300),
    BackOff = case length(integer_to_list(Max, 2)) of
                  MaxExp when F > MaxExp -> Max;
                  _ -> 1 bsl F
              end,
    {next_state, disconnected,
     reconnect_in(timer:seconds(BackOff), State)}.

reconnect_in(MS, State = #state{}) ->
    Ref = erlang:start_timer(MS, self(), ?RECONNECT_MSG),
    State#state{reconnect_tref = Ref}.

%% @private
tcp_good(State = #state{}) ->
    State#state{last_good_time = os:timestamp(),
                failures = 0}.

%% @private
%% Caller must ensure sock is closed before calling this.
tcp_bad(State = #state{sock = Sock}) when is_port(Sock) ->
    catch gen_tcp:close(Sock),
    tcp_bad(State#state{sock = undefined});
tcp_bad(State = #state{sock = undefined,
                       failures = F}) ->
    State#state{failures = F + 1}.

-spec time_failed(#state{}) -> iolist().
%% @private
time_failed(State = #state{}) ->
    time_failed(os:timestamp(), State).
time_failed(Now, #state{last_good_time=T0})
  when is_tuple(T0) ->
    integer_to_list(timer:now_diff(Now, T0) div 1000000);
time_failed(_, #state{last_good_time=undefined}) ->
    "".

%% @private
log_info(#state{drain_id=DrainId, channel_id=ChannelId, host=H, port=P}, Rest)
  when is_list(Rest) ->
    [DrainId, ChannelId, logplex_logging:dest(H,P) | Rest].

-spec msg_stat('drain_dropped' | 'drain_buffered' | 'drain_delivered' |
               'requests_sent',
               non_neg_integer(), #state{}) -> any().
msg_stat(Key, N,
         #state{drain_id=DrainId, channel_id=ChannelId}) ->
    logplex_stats:incr(#drain_stat{drain_id=DrainId,
                                   channel_id=ChannelId,
                                   key=Key}, N).

-spec duration(#state{}) -> iolist().
duration(#state{connect_time=undefined}) ->
    "undefined";
duration(#state{connect_time=T0}) ->
    US = timer:now_diff(os:timestamp(), T0),
    io_lib:format("~f", [US / 1000000]).

-spec push_frame(iodata(), non_neg_integer(), non_neg_integer(), #state{}) -> #state{}.
push_frame(Data, MsgCount, Lost, State = #state{out_q=Q}) ->
    Retries = logplex_app:config(tcp_syslog_retries, 1),
    Tries = Retries + 1,
    Frame = #frame{frame=Data,
                   msg_count=MsgCount,
                   loss_count=Lost,
                   tries=Tries},
    NewQ = queue:in(Frame, Q),
    State#state{out_q = NewQ}.

-spec sent_frame(#frame{}, #state{}) -> #state{}.
sent_frame(#frame{msg_count=Count, loss_count=Lost}, State=#state{drop_info=Drop}) ->
    logplex_realtime:incr(drain_delivered, Count),
    msg_stat(drain_delivered, Count, State),
    case {Lost, Drop} of
        {0, undefined} ->
            State;
        {_, undefined} ->
            logplex_realtime:incr(drain_dropped, Lost),
            msg_stat(drain_dropped, Lost, State),
            State;
        {_, {_,Dropped}} ->
            logplex_realtime:incr(drain_dropped, Lost+Dropped),
            msg_stat(drain_dropped, Lost+Dropped, State),
            State#state{drop_info=undefined}
    end.

retry_frame(Frame = #frame{tries = N},
            State = #state{out_q = Q}) when N > 1 ->
    NewQ = queue:in_r(Frame#frame{tries = N - 1}, Q),
    State#state{out_q = NewQ};
retry_frame(Frame = #frame{tries = N}, State) when N =< 1 ->
    drop_frame(Frame, State).

%% @private
%% @doc Called on frames we've decided to drop. Records count of
%% messages dropped (not frame count).
drop_frame(#frame{msg_count=Msgs, loss_count=Lost}, State) ->
    lost_msgs(Msgs+Lost, State).

lost_msgs(0, State) -> State;
lost_msgs(Lost, S = #state{drop_info=undefined}) ->
    S#state{drop_info={os:timestamp(), Lost}};
lost_msgs(Lost, S = #state{drop_info={TS,Dropped}}) ->
    S#state{drop_info={TS,Dropped+Lost}}.


%% @private
%% @doc Send buffered messages.
-spec send(#state{}) -> {next_state, 'sending' | 'ready_to_send', #state{}}.
send(State = #state{buf = Buf, out_q = Q}) ->
    case queue:out(Q) of
        {empty,_} ->
            set_active(Buf, State),
            {next_state, connected, State};
        {{value,Frame}, NewQ} ->
            do_send(Frame, State#state{out_q=NewQ})
    end.

do_send(Frame = #frame{frame=Data0, tries=Tries, loss_count=Lost},
        State = #state{sock = Sock, drop_info=Drops, drain_tok=DrainTok})
        when Tries > 0 ->
    Data = case {Drops,Lost} of
        {undefined,0} -> Data0;
        {undefined,_} ->
            T0 = os:timestamp(),
            Msg = frame(logplex_syslog_utils:overflow_msg(Lost,T0), DrainTok),
            [Msg,Data0];
        {{T0,Dropped},_} ->
            Msg = frame(logplex_syslog_utils:overflow_msg(Dropped+Lost,T0), DrainTok),
            [Msg,Data0]
    end,
    try gen_tcp:send(Sock, Data) of
        ok ->
            send(tcp_good(sent_frame(Frame, State)));
        {error, closed} ->
            reconnect(tcp_bad(retry_frame(Frame, State)));
        {error, enotconn} ->
            reconnect(tcp_bad(retry_frame(Frame, State)));
        {error, _Reason} -> % may or may no have closed the connection
            send(retry_frame(Frame, State))
    catch
        error:badarg ->
            ?INFO("drain_id=~p channel_id=~p dest=~s state=~p "
                "err=gen_tcp data=~p sock=~p duration=~s",
                log_info(State, [send, closed, Sock,
                         duration(State)])),
            %% Re-use old state as we know the messages we
            %% just de-buffered are lost to tcp.
            reconnect(tcp_bad(retry_frame(Frame, State)))
    end;
do_send(Frame = #frame{tries=0, msg_count=C}, State=#state{}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s at=do_send result=tries_exceeded "
          "frame_tries=0 dropped_msgs=~p",
          log_info(State, [C])),
      send(drop_frame(Frame, State)).


set_active(Buf, #state{drain_tok=Token}) ->
    TargBytes = target_send_size(),
    logplex_drain_buffer:set_active(Buf, TargBytes, drain_buf_framing(Token)),
    ok.

target_send_size() ->
    case get(target_send_size) of
        Size when is_integer(Size),
                  Size > 0 ->
            Size;
        _ ->
            logplex_app:config(tcp_drain_target_bytes,
                               ?TARGET_SEND_SIZE)
    end.

frame(LogTuple, DrainToken) ->
    logplex_syslog_utils:frame(logplex_syslog_utils:to_msg(LogTuple, DrainToken)).

%% This function is local, so we need to replace it every time we update
%% this module!
drain_buf_framing(DrainToken) ->
    fun({loss_indication, _N, _When}) -> skip;
       ({msg, MData}) -> {frame, frame(MData, DrainToken)}
    end.

start_drain_buffer(State=#state{channel_id=ChannelId, buf=undefined}) ->
    DrainSize = logplex_app:config(tcp_drain_buffer_size),
    {ok, Buf} = logplex_drain_buffer:start_link(ChannelId, self(),
                                                notify, DrainSize),
    State#state{buf = Buf}.

