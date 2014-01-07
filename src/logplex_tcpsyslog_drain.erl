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
-define(HIBERNATE_TIMEOUT, 5000).
-define(SHRINK_TRIES, 10).
-define(SHRINK_BUF_SIZE, 10).
-define(IDLE_TIMEOUT_MSG, idle_timeout).

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
                buf = logplex_msg_buffer:new(default_buf_size()) :: logplex_msg_buffer:buf(),
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

-type pstate() :: 'disconnected' | 'ready_to_send' | 'sending' | 'disconnecting'.

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
         ready_to_send/2,
         sending/2,
         disconnecting/2
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
init([State0 = #state{sock = undefined, host=H, port=P,
                      drain_id=DrainId, channel_id=ChannelId}])
  when H =/= undefined, is_integer(P) ->
    try
        random:seed(os:timestamp()),
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
disconnected({timeout, _Ref, ?IDLE_TIMEOUT_MSG}, State) ->
    %% Already disconnected; nothing to do here
    {next_state, disconnected, State, hibernate};
disconnected(timeout, State) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, disconnected, State, hibernate};
disconnected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~1000p state=disconnected",
          log_info(State, [Msg])),
    {next_state, disconnected, State, ?HIBERNATE_TIMEOUT}.

%% @doc We have a socket open and messages to send. Collect up an
%% appropriate amount and flush them to the socket.
ready_to_send({timeout, _Ref, ?SEND_TIMEOUT_MSG},
              State = #state{sock = Sock})
  when is_port(Sock) ->
    %% Stale message.
    send(State);
ready_to_send({timeout, _Ref, ?IDLE_TIMEOUT_MSG}, S) ->
    case close_if_idle(S) of
        closed ->
            {next_state, disconnected, S#state{sock = undefined}, hibernate};
        ok ->
            {next_state, ready_to_send, S}
    end;
ready_to_send({post, Msg}, State = #state{sock = Sock})
  when is_port(Sock) ->
    send(buffer(Msg, State));
ready_to_send({inet_reply, Sock, ok}, S = #state{sock = Sock})
  when is_port(Sock) ->
    %% Stale inet reply
    send(S);
ready_to_send(timeout, S = #state{}) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, ready_to_send, S, hibernate};
ready_to_send(Msg, State = #state{sock = Sock})
  when is_port(Sock) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=ready_to_send",
          log_info(State, [Msg])),
    {next_state, ready_to_send, State, ?HIBERNATE_TIMEOUT}.


%% @doc We sent some data to the socket and are waiting the result of
%% the send operation.
sending({timeout, Ref, ?SEND_TIMEOUT_MSG},
        S = #state{send_tref=Ref}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s err=send_timeout "
          "state=sending",
          log_info(S, [])),
    reconnect(tcp_bad(S#state{send_tref=undefined}));
sending({post, Msg}, State) ->
    {next_state, sending, buffer(Msg, State), ?HIBERNATE_TIMEOUT};
sending({inet_reply, Sock, ok}, S = #state{sock = Sock, send_tref = TRef}) ->
    send(tcp_good(S#state{send_tref=cancel_timeout(TRef, ?SEND_TIMEOUT_MSG)}));
sending({inet_reply, Sock, {error, Reason}}, S = #state{sock = Sock}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
         "err=gen_tcp data=~p sock=~p duration=~s state=sending",
          log_info(S, [sending, Reason, Sock, duration(S)])),
    reconnect(tcp_bad(S));
sending({timeout, _, ?IDLE_TIMEOUT_MSG}, State) ->
    case close_if_idle(State) of
        closed ->
            {next_state, disconnecting, State#state{sock = undefined}};
        ok ->
            {next_state, sending, State}
    end;
sending(timeout, S = #state{}) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, sending, S, hibernate};
sending(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=sending",
          log_info(State, [Msg])),
    {next_state, sending, State, ?HIBERNATE_TIMEOUT}.

%% @doc We got an idle timeout while in the sending state but haven't
%% gotten an inet_reply yet.
disconnecting({timeout, _TRef, ?SEND_TIMEOUT_MSG}, S) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s err=send_timeout "
          "state=disconnecting", log_info(S, [])),
    {next_state, disconnected, tcp_bad(S#state{send_tref=undefined}), hibernate};
disconnecting({inet_reply, Sock, Status}, S = #state{sock = Sock,
                                                     send_tref = SendTRef}) ->
    case Status of
        {error, Reason} ->
            ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
                "err=gen_tcp data=~p sock=~p duration=~s state=disconnecting",
                log_info(S, [disconnecting, Reason, Sock, duration(S)]));
        _ -> ok
    end,
    NewState = S#state{sock = undefined,
                       send_tref = cancel_timeout(SendTRef, ?SEND_TIMEOUT_MSG)},
    {next_state, disconnected, NewState, hibernate};
disconnecting({post, Msg}, State) ->
    {next_state, sending, buffer(Msg, State), ?HIBERNATE_TIMEOUT};
disconnecting({timeout, _, ?IDLE_TIMEOUT_MSG}, State) ->
    %% Shouldn't see this since entering this state means the timer wasn't reset
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_idle_timeout "
          "data=~p state=disconnecting", log_info(State, [])),
    {next_state, disconnecting, State};
disconnecting(timeout, S = #state{}) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, disconnecting, S, hibernate};
disconnecting(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=disconnecting", log_info(State, [Msg])),
    {next_state, disconnecting, State, ?HIBERNATE_TIMEOUT}.


%% @private
%% state_name(Event, _From, State) ->
%%     ?WARN("[state ~p] Unexpected event ~p",
%%           [state_name, Event]),
%%     {next_state, state_name, State}.

%% @private
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State, ?HIBERNATE_TIMEOUT}.

%% @private
handle_sync_event({set_target_send_size, Size}, _From, StateName,
                  State = #state{})
  when is_integer(Size), Size > 0 ->
    put(target_send_size, Size),
    {reply, {ok, Size}, StateName, State, ?HIBERNATE_TIMEOUT};
handle_sync_event({resize_msg_buffer, NewSize}, _From, StateName,
                  State = #state{buf = Buf})
  when is_integer(NewSize), NewSize > 0 ->
    NewBuf = logplex_msg_buffer:resize(NewSize, Buf),
    {reply, ok, StateName, State#state{buf = NewBuf}, ?HIBERNATE_TIMEOUT};

handle_sync_event(Event, _From, StateName, State) ->
    ?WARN("[state ~p] Unexpected event ~p",
          [StateName, Event]),
    {next_state, StateName, State, ?HIBERNATE_TIMEOUT}.

%% @private
handle_info({tcp, Sock, Data}, StateName,
            State = #state{sock = Sock}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s state=~p "
          "err=unexpected_peer_data data=~p",
          log_info(State, [StateName, Data])),
    {next_state, StateName, State, ?HIBERNATE_TIMEOUT};
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
handle_info(timeout, StateName, State) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, StateName, State, hibernate};
handle_info(Info, StateName, State) ->
    ?MODULE:StateName(Info, State).

%% @private
terminate(Reason, StateName, State) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s state=~p "
          "at=terminate reason=~p",
          log_info(State, [StateName, Reason])),
    ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State, ?HIBERNATE_TIMEOUT}.

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
                            buf=Buf,
                            failures = Failures}) ->
    case connect(State) of
        {ok, Sock} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s "
                  "state=disconnected at=connect try=~p sock=~p",
                  log_info(State, [Failures + 1, Sock])),
            NewState = State#state{sock=Sock,
                                   reconnect_tref = undefined,
                                   send_tref = undefined,
                                   buf = maybe_resize(Buf),
                                   connect_time=os:timestamp()},
            start_idle_timer(),
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
            {next_state, disconnected, State, ?HIBERNATE_TIMEOUT}
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
            %% We hibernate only when we need to reconnect with a timer.  The
            %% timer acts as a rate limiter! If you remove the timer, you must
            %% re-think the hibernation.
            {next_state, disconnected,
             reconnect_in(timer:seconds(Min), State), hibernate};
        _EnoughTime ->
            do_reconnect(State)
    end;
reconnect(State = #state{failures = F, buf=Buf}) ->
    Max = logplex_app:config(tcp_syslog_backoff_max, 300),
    BackOff = case length(integer_to_list(Max, 2)) of
                  MaxExp when F > MaxExp -> Max;
                  _ -> 1 bsl F
              end,
    NewBuf = maybe_shrink(Buf, F),
    %% We hibernate only when we need to reconnect with a timer. The timer
    %% acts as a rate limiter! If you remove the timer, you must re-think
    %% the hibernation.
    {next_state, disconnected,
     reconnect_in(timer:seconds(BackOff), State#state{buf=NewBuf}),
     hibernate}.

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
            msg_stat(drain_dropped, 1, State),
            %% If socket is online, note drain dropped.
            case State#state.sock of
                undefined -> ok;
                _ -> logplex_realtime:incr(drain_dropped)
            end;
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
            PktSize = target_send_size(),
            {Data, N, NewBuf} =
                buffer_to_pkts(Buf, PktSize, DrainTok),
            Ref = erlang:start_timer(?SEND_TIMEOUT, self(), ?SEND_TIMEOUT_MSG),
            try
                erlang:port_command(Sock, Data, []),
                msg_stat(drain_delivered, N, State),
                logplex_realtime:incr(drain_delivered, N),
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

cancel_timeout(undefined, _Msg) -> undefined;
cancel_timeout(Ref, Msg)
  when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            %% Flush expired timer message
            receive
                {timeout, Ref, Msg} -> undefined
            after 0 -> undefined
            end;
        _Time ->
            %% Timer didn't fire, so no message to worry about
            undefined
      end.


now_ms({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000 + (MicroSecs / 1000).

start_idle_timer() ->
    MaxIdle = logplex_app:config(tcp_syslog_idle_timeout, timer:minutes(5)),
    Fuzz = random:uniform(logplex_app:config(tcp_syslog_idle_fuzz, 15000)),
    erlang:start_timer(MaxIdle + Fuzz, self(), ?IDLE_TIMEOUT_MSG).

close_if_idle(State = #state{sock = Sock, last_good_time = LastGood}) ->
    MaxIdle = logplex_app:config(tcp_syslog_idle_timeout, timer:minutes(5)),
    SinceLastGood = now_ms(os:timestamp()) - now_ms(LastGood),
    case SinceLastGood > MaxIdle of
        true ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=idle_timeout",
                  log_info(State, [])),
            gen_tcp:close(Sock),
            closed;
        _ ->
            start_idle_timer(),
            ok
    end.

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

target_send_size() ->
    case get(target_send_size) of
        Size when is_integer(Size),
                  Size > 0 ->
            Size;
        _ ->
            logplex_app:config(tcp_drain_target_bytes,
                               ?TARGET_SEND_SIZE)
    end.

maybe_resize(Buf) ->
    Default = default_buf_size(),
    case logplex_msg_buffer:max_size(Buf) < Default of
        true -> logplex_msg_buffer:resize(Default, Buf);
        false -> Buf
    end.

maybe_shrink(Buf, Tries) ->
    Max = logplex_msg_buffer:max_size(Buf),
    case Max =:= ?SHRINK_BUF_SIZE of
        true ->
            Buf;
        false ->
            %% Shrink if we have never connected before or the last update time
            %% is more than ?SHRINK_TRIES old, and if the buffer is
            %% currently full and dropping data
            case full =:= logplex_msg_buffer:full(Buf) andalso
                 logplex_msg_buffer:lost(Buf) > 0 andalso
                 Tries > ?SHRINK_TRIES of
                true ->
                    logplex_msg_buffer:resize(?SHRINK_BUF_SIZE, Buf);
                false ->
                    Buf
            end
    end.

default_buf_size() -> logplex_app:config(tcp_drain_buffer_size, 1024).
