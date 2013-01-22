%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Syslog/tcp drain
%% @end
%%%-------------------------------------------------------------------

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
                buf = logplex_msg_buffer:new() :: logplex_msg_buffer:buf(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% TCP failures since last_good_time
                failures = 0 :: non_neg_integer(),
                %% Reconnect timer reference
                reconnect_tref = undefined :: 'undefined' | reference(),
                %% Send timer reference
                send_tref = undefined :: 'undefined' | reference(),
                %% Time of last successful connection
                connect_time :: 'undefined' | erlang:timestamp()
               }).

-type pstate() :: 'disconnected' | 'ready_to_send' | 'sending'.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/5]).

-export([valid_uri/1
         ,uri/2
         ,start_link/4
        ]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([disconnected/2,
         ready_to_send/2,
         sending/2
         ]).

-export([init/1,  handle_event/3, handle_sync_event/4,
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

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

%% @private
init([State0 = #state{sock = undefined, host=H, port=P,
                      drain_id=DrainId, channel_id=ChannelId}])
  when H =/= undefined, is_integer(P) ->
    try
        logplex_drain:register(DrainId, ChannelId, tcpsyslog,
                               {H,P}),
        DrainSize = logplex_app:config(tcp_drain_buffer_size),
        State = State0#state{buf = logplex_msg_buffer:new(DrainSize)},
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
disconnected({post, Msg}, State) ->
    reconnect(buffer(Msg, State));
disconnected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~1000p state=disconnected",
          log_info(State, [Msg])),
    {next_state, disconnected, State}.

%% @doc We have a socket open and messages to send. Collect up an
%% appropriate amount and flush them to the socket.
ready_to_send({timeout, _Ref, ?SEND_TIMEOUT_MSG},
              State = #state{sock = Sock})
  when is_port(Sock) ->
    %% Stale message.
    send(State);
ready_to_send({post, Msg}, State = #state{sock = Sock})
  when is_port(Sock) ->
    send(buffer(Msg, State));
ready_to_send({inet_reply, Sock, ok}, S = #state{sock = Sock})
  when is_port(Sock) ->
    %% Stale inet reply
    send(S);
ready_to_send(Msg, State = #state{sock = Sock})
  when is_port(Sock) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=ready_to_send",
          log_info(State, [Msg])),
    {next_state, ready_to_send, State}.


%% @doc We sent some data to the socket and are waiting the result of
%% the send operation.
sending({timeout, Ref, ?SEND_TIMEOUT_MSG},
        S = #state{send_tref=Ref}) ->
    reconnect(tcp_bad(S#state{send_tref=undefined}));
sending({post, Msg}, State) ->
    {next_state, sending, buffer(Msg, State)};
sending({inet_reply, Sock, ok}, S = #state{sock = Sock}) ->
    send(tcp_good(cancel_send_timeout(S)));
sending({inet_reply, Sock, {error, Reason}}, S = #state{sock = Sock}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
         "err=gen_tcp data=~p sock=~p duration=~s state=sending",
          log_info(S, [sending, Reason, Sock, duration(S)])),
    reconnect(tcp_bad(S));
sending(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=sending",
          log_info(State, [Msg])),
    {next_state, sending, State}.


%% @private
%% state_name(Event, _From, State) ->
%%     ?WARN("[state ~p] Unexpected event ~p",
%%           [state_name, Event]),
%%     {next_state, state_name, State}.

%% @private
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
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
    {stop, shutdown, State#state{sock = undefined}};
handle_info(shutdown, _StateName, State) ->
    {stop, shutdown, State};
handle_info(Info, StateName, State) ->
    ?MODULE:StateName(Info, State).

%% @private
terminate(_Reason, _StateName, _State) ->
    ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

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
                            reconnect_tref = undefined}) ->
    case connect(State) of
        {ok, Sock} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s "
                  "state=disconnected at=connect try=~p sock=~p",
                  log_info(State, [State#state.failures + 1, Sock])),
            NewState = State#state{sock=Sock,
                                   reconnect_tref = undefined,
                                   send_tref = undefined,
                                   connect_time=os:timestamp()},
            send(NewState);
        {error, Reason} ->
            NewState = tcp_bad(State),
            ?ERR("drain_id=~p channel_id=~p dest=~s at=connect "
                 "err=gen_tcp data=~p try=~p last_success=~s "
                 "state=disconnected",
                 log_info(State, [Reason, NewState#state.failures,
                                  time_failed(NewState)])),
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
    ?INFO("drain_id=~p channel_id=~p dest=~s at=reconnect_delay delay=~p "
          "ref=~p",
          log_info(State, [MS, Ref])),
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

-spec msg_stat('drain_dropped' | 'drain_buffered' | 'drain_delivered',
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

%% -spec buffer_status(#state{}) -> 'empty' | 'has_messages_to_send'.
%% %% @private
%% buffer_status(State = #state{buf = Buf}) ->
%%     case logplex_msg_buffer:len(Buf) of
%%         0 -> empty;
%%         _ -> has_messages_to_send
%%     end.

-spec buffer(any(), #state{}) -> #state{}.
%% @private
buffer(Msg, State = #state{buf = Buf}) ->
    {Result, NewBuf} = logplex_msg_buffer:push_ext(Msg, Buf),
    msg_stat(drain_buffered, 1, State),
    case Result of
        displace ->
            msg_stat(drain_dropped, 1, State);
        insert -> ok
    end,
    State#state{buf=NewBuf}.

%% @private
%% @doc Send buffered messages.
-spec send(#state{}) -> {next_state, 'sending' | 'ready_to_send', #state{}}.
send(State = #state{buf = Buf, sock = Sock,
                    drain_tok = DrainTok}) ->
    case logplex_msg_buffer:empty(Buf) of
        empty ->
            {next_state, ready_to_send, State};
        not_empty ->
            {Data, N, NewBuf} =
                buffer_to_pkts(Buf, ?TARGET_SEND_SIZE, DrainTok),
            Ref = erlang:start_timer(?SEND_TIMEOUT, self(), ?SEND_TIMEOUT_MSG),
            try
                erlang:port_command(Sock, Data, []),
                msg_stat(drain_delivered, N, State),
                logplex_realtime:incr(message_routed),
                {next_state, sending,
                 State#state{buf = NewBuf,
                             send_tref=Ref}}
            catch
                error:badarg ->
                    ?INFO("drain_id=~p channel_id=~p dest=~s state=~p "
                          "err=gen_tcp data=~p sock=~p duration=~s",
                          log_info(State, [send, closed, Sock,
                                           duration(State)])),
                    erlang:cancel_timer(Ref),
                    %% Re-use old state as we know the messages we
                    %% just de-buffered are lost to tcp.
                    reconnect(tcp_bad(State))
            end
    end.

-spec cancel_send_timeout(#state{}) -> #state{send_tref :: undefined}.
cancel_send_timeout(State = #state{send_tref = undefined}) -> State;
cancel_send_timeout(State = #state{send_tref = Ref})
  when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            %% Flush expired timer message
            receive
                {timeout, Ref, ?SEND_TIMEOUT_MSG} -> ok
            after 0 -> ok
            end;
        _Time ->
            %% Timer didn't fire, so no message to worry about
            ok
    end,
    State#state{send_tref=undefined}.

buffer_to_pkts(Buf, BytesRemaining, DrainTok) ->
    logplex_msg_buffer:to_pkts(Buf, BytesRemaining,
                               pkt_fmt(DrainTok)).

pkt_fmt(DrainTok) ->
    Frame = fun (Msg) ->
                    SyslogMsg = logplex_syslog_utils:to_msg(Msg, DrainTok),
                    logplex_syslog_utils:frame(SyslogMsg)
            end,
    fun ({loss_indication, N, When}) ->
            case logplex_app:config(tcp_syslog_send_loss_msg) of
                dont_send ->
                    skip;
                _ ->
                    {frame,
                     Frame(logplex_syslog_utils:overflow_msg(N, When))}
            end;
        ({msg, MData}) ->
            {frame, Frame(MData)}
    end.
