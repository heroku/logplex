%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Syslog/tcp drain
%% @end
%%%-------------------------------------------------------------------

-module(logplex_tcpsyslog_drain2).
-behaviour(gen_fsm).
-define(SERVER, ?MODULE).
-define(RECONNECT_MSG, reconnect).
-define(TARGET_SEND_SIZE, 4096).

-include("logplex.hrl").
-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                host :: string() | inet:ip_address() | binary(),
                port :: inet:port_number(),
                sock = undefined :: 'undefined' | inet:socket(),
                %% Buffer for messages while disconnected
                buf = logplex_drain_buffer:new() :: logplex_drain_buffer:buf(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% TCP failures since last_good_time
                failures = 0 :: non_neg_integer(),
                %% Reconnect timer reference
                reconnect_tref = undefined :: 'undefined' | reference(),
                %% Send timer reference
                send_tref = undefined :: 'undefined' | reference(),
                connect_time :: 'undefined' | erlang:timestamp()
               }).

%% -type pstate() :: 'disconnected' | 'ready_to_send' | 'sending'.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/5]).

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

start_link(ChannelID, DrainID, DrainTok, Host, Port) ->
    gen_fsm:start_link(?MODULE,
                       [#state{drain_id=DrainID,
                               drain_tok=DrainTok,
                               channel_id=ChannelID,
                               host=Host,
                               port=Port}],
                       []).

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

%% @private
init([State0 = #state{sock = undefined,
                      host=H, port=P}])
  when H =/= undefined, is_integer(P) ->
    try
        register_with_gproc(State0),
        DrainSize = logplex_app:config(tcp_drain_buffer_size),
        State = State0#state{buf = logplex_drain_buffer:new(DrainSize)},
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
    case connect(State) of
        {ok, Sock} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=connect try=~p sock=~p",
                  log_info(State, [State#state.failures + 1, Sock])),
            NewState = tcp_good(State#state{sock=Sock,
                                            connect_time=os:timestamp()}),
            send(NewState);
        {error, Reason} ->
            NewState = tcp_bad(State),
            ?ERR("drain_id=~p channel_id=~p dest=~s at=connect "
                 "err=gen_tcp data=~p try=~p last_success=~s",
                 log_info(State, [Reason, NewState#state.failures,
                                  time_failed(NewState)])),
            {next_state, disconnected, reconnect(NewState)}
    end;
disconnected({post, Msg}, State) ->
    {next_state, disconnected,
     reconnect(buffer(Msg, State))};
disconnected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info data=~p",
          log_info(State, [Msg])),
    {next_state, disconnected, State}.

%% @doc We have a socket open and messages to send. Collect up an
%% appropriate amount and flush them to the socket.
ready_to_send({post, Msg}, State) ->
    send(buffer(Msg, State));
ready_to_send({inet_reply, Sock, ok}, S = #state{sock = Sock}) ->
    %% Stale inet reply
    send(S);
ready_to_send(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info data=~p",
          log_info(State, [Msg])),
    {next_state, disconnected, State}.


%% @doc We sent some data to the socket and are waiting the result of
%% the send operation.
sending({post, Msg}, State) ->
    {next_state, sending, buffer(Msg, State)};
sending({inet_reply, Sock, ok}, S = #state{sock = Sock}) ->
    send(S);
sending({inet_reply, Sock, {error, Reason}}, S = #state{sock = Sock}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
         "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(S, [sending, Reason, Sock, duration(S)])),
    {next_state, disconnected, reconnect(tcp_bad(S))};
sending(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info data=~p",
          log_info(State, [Msg])),
    {next_state, disconnected, State}.


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
handle_info({tcp, Sock, Data}, StateName, State = #state{sock = Sock}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s state=~p"
          "err=unexpected_peer_data data=~p",
          log_info(State, [StateName, Data])),
    {next_state, StateName, State};
handle_info({tcp_error, Sock, Reason}, StateName, State) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
         "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(State, [StateName, Reason, Sock, duration(State)])),
    {next_state, disconnected, reconnect(tcp_bad(State))};
handle_info({inet_reply, Sock, {error, Reason}}, StateName,
            State = #state{sock = Sock}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s state=~p "
         "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(State, [StateName, Reason, Sock, duration(State)])),
    {next_state, disconnected, reconnect(tcp_bad(State))};
handle_info({tcp_closed, Sock}, StateName, State) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s state=~p "
          "err=gen_tcp data=~p sock=~p duration=~s",
          log_info(State, [StateName, closed, Sock, duration(State)])),
    {next_state, disconnected, reconnect(tcp_bad(State))};
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

-spec reconnect(#state{}) -> #state{}.
%% @private
reconnect(State = #state{reconnect_tref = Ref}) when is_reference(Ref) ->
    %% Reconnect timer was set
    case erlang:read_timer(Ref) of
        false ->
            %% and has expired
            reconnect(State#state{reconnect_tref=undefined});
        _ ->
            %% and is still valid
            State
    end;
reconnect(State = #state{failures = 0, last_good_time=undefined}) ->
    %% First reconnect ever
    reconnect_in(1, State);
reconnect(State = #state{failures = 0, last_good_time=T})
  when is_tuple(T), tuple_size(T) =:= 3 ->
    Min = logplex_app:config(tcp_syslog_reconnect_min, 30),
    SecsSinceConnect = timer:now_diff(os:timestamp(), T) div 1000000,
    case SecsSinceConnect of
        TooFew when TooFew < Min ->
            reconnect_in(Min, State);
        _EnoughTime ->
            reconnect_in(1, State)
    end;
reconnect(State = #state{failures = F}) ->
    Max = logplex_app:config(tcp_syslog_backoff_max, 300),
    BackOff = case length(integer_to_list(Max, 2)) of
                  MaxExp when F > MaxExp -> Max;
                  _ -> 1 bsl F
              end,
    reconnect_in(BackOff, State).

reconnect_in(Seconds, State = #state{}) ->
    Ref = erlang:start_timer(timer:seconds(Seconds), self(), ?RECONNECT_MSG),
    State#state{reconnect_tref = Ref}.

%% cancel_timer(S = #state{reconnect_tref = undefined}) -> S;
%% cancel_timer(S = #state{reconnect_tref = Ref}) when is_reference(Ref) ->
%%     erlang:cancel_timer(Ref),
%%     S#state{reconnect_tref = undefined}.

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
               pos_integer(), #state{}) -> any().
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
%%     case logplex_drain_buffer:len(Buf) of
%%         0 -> empty;
%%         _ -> has_messages_to_send
%%     end.

-spec buffer(any(), #state{}) -> #state{}.
%% @private
buffer(Msg, State = #state{buf = Buf}) ->
    {Result, NewBuf} = logplex_drain_buffer:push_ext(Msg, Buf),
    msg_stat(drain_buffered, 1, State),
    case Result of
        displace ->
            msg_stat(drain_dropped, 1, State);
        insert -> ok
    end,
    State#state{buf=NewBuf}.

%% @private
register_with_gproc(#state{drain_id=DrainId,
                           channel_id=ChannelId,
                           host=H, port=P})
  when H =/= undefined, is_integer(P) ->
    gproc:reg({n, l, {drain, DrainId}}, undefined),
    %% This is ugly, but there's no other obvious way to do it.
    gproc:mreg(p, l, [{{channel, ChannelId}, true},
                      {drain_dest, {H, P}},
                      {drain_type, tcpsyslog}]),
    ok.

%% @private
%% unregister_from_gproc(#state{drain_id=DrainId,
%%                              channel_id=ChannelId}) ->
%%     gproc:unreg({n, l, {drain, DrainId}}),
%%     gproc:munreg(p, l, [{channel, ChannelId},
%%                         drain_dest,
%%                         drain_type]).

%% @private
%% @doc Send buffered messages.
send(State = #state{buf = Buf, sock = Sock,
                    drain_tok = DrainTok}) ->
    case logplex_drain_buffer:empty(Buf) of
        empty ->
            {next_state, ready_to_send, State};
        not_empty ->
            {Data, NewBuf} = buffer_to_pkts(Buf, ?TARGET_SEND_SIZE, DrainTok),
            erlang:port_command(Sock, Data, []),
            {next_state, sending, State#state{buf = NewBuf}}
    end.

buffer_to_pkts(Buf, BytesRemaining, DrainTok) when BytesRemaining > 0 ->
    {Item, NewBuf} = logplex_drain_buffer:pop(Buf),
    Msg = case Item of
              empty ->
                  finished;
              {loss_indication, N, When} ->
                  case logplex_app:config(tcp_syslog_send_loss_msg) of
                      dont_send ->
                          skip;
                      _ ->
                          {msg,
                           logplex_syslog_utils:overflow_msg(N, When)}
                  end;
              {msg, M} ->
                  {msg, M}
          end,
    case Msg of
        finished ->
            {[], NewBuf};
        skip ->
            buffer_to_pkts(NewBuf, BytesRemaining, DrainTok);
        {msg, MData} ->
            SyslogMsg = logplex_syslog_utils:to_msg(MData, DrainTok),
            Data = logplex_syslog_utils:frame([SyslogMsg, $\n]),
            DataSize = iolist_size(Data),
            case BytesRemaining - DataSize of
                Remaining when Remaining > 0 ->
                    {Rest, FinalBuf} = buffer_to_pkts(NewBuf,
                                                      Remaining,
                                                      DrainTok),
                    {[Data, Rest], FinalBuf};
                _ when DataSize > ?TARGET_SEND_SIZE ->
                    %% We will exceed bytes remaining, but this
                    %% message is a pig, so send it anyway.
                    {Data, NewBuf};
                _ ->
                    %% Would have exceeded BytesRemaining, pretend we
                    %% didn't pop it.
                    {[], Buf}
            end
    end.
