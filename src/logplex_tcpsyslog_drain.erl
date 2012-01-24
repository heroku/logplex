%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc
%% @end
%%%-------------------------------------------------------------------
-module(logplex_tcpsyslog_drain).

-behaviour(gen_server).

-include("logplex.hrl").
-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/5
         ,shutdown/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
                tref = undefined :: 'undefined' | reference()
               }).

-define(RECONNECT_MSG, reconnect).

%%====================================================================
%% API
%%====================================================================

start_link(ChannelID, DrainID, DrainTok, Host, Port) ->
    gen_server:start_link(?MODULE,
                          [#state{drain_id=DrainID,
                                  drain_tok=DrainTok,
                                  channel_id=ChannelID,
                                  host=Host,
                                  port=Port}],
                          []).

shutdown(Pid) ->
    gen_server:cast(Pid, shutdown),
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([State0 = #state{drain_id=DrainId,
                      channel_id=ChannelId,
                      host=H, port=P}])
  when H =/= undefined, is_integer(P) ->
    try
        gproc:add_local_name({drain, DrainId}),
        %% This is ugly, but there's no other obvious way to do it.
        gproc:add_local_property({channel, ChannelId}, true),
        gproc:add_local_property(drain_dest, {H, P}),
        gproc:add_local_property(drain_type, tcpsyslog),
        DrainSize = logplex_app:config(tcp_drain_buffer_size),
        State = State0#state{buf = logplex_drain_buffer:new(DrainSize)},
        ?INFO("drain_id=~p channel_id=~p dest=~s at=spawn",
              log_info(State, [])),
        {ok, State, hibernate}
    catch
        error:badarg -> ignore
    end.

%% @private
handle_call(Call, _From, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_call data=~p",
          log_info(State, [Call])),
    {noreply, State}.

%% @private
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_cast data=~p",
          log_info(State, [Msg])),
    {noreply, State}.

%% @private
handle_info({post, Msg}, State = #state{sock = undefined,
                                        buf = Buf})
  when is_tuple(Msg) ->
    msg_stat(drain_buffered, 1, State),
    NewBuf = logplex_drain_buffer:push(Msg, Buf),
    NewState = State#state{buf=NewBuf},
    {noreply, maybe_reconnect(NewState)};

handle_info({post, Msg}, State = #state{drain_tok = DrainTok,
                                        sock = S})
  when is_tuple(Msg) ->
    case post(Msg, S, DrainTok) of
        ok ->
            msg_stat(drain_delivered, 1, State),
            logplex_realtime:incr(message_routed),
            {noreply, tcp_good(State)};
        {error, Reason} ->
            msg_stat(drain_buffered, 1, State),
            NewBuf = logplex_drain_buffer:push(Msg, State#state.buf),
            NewState = tcp_bad(State#state{buf=NewBuf}),
            ?ERR("drain_id=~p channel_id=~p dest=~s at=post err=gen_tcp data=~p",
                 log_info(NewState, [Reason])),
            {noreply, reconnect(tcp_error, NewState)}
    end;

handle_info({timeout, TRef, ?RECONNECT_MSG},
            State = #state{tref = TRef, sock = undefined}) ->
    case connect(State) of
        {ok, Sock} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=connect try=~p sock=~p",
                  log_info(State, [State#state.failures + 1, Sock])),
            NewState = tcp_good(State#state{sock=Sock}),
            {noreply, post_buffer(NewState)};
        {error, Reason} ->
            NewState = tcp_bad(State),
            ?ERR("drain_id=~p channel_id=~p dest=~s at=connect "
                 "err=gen_tcp data=~p try=~p last_success=~s",
                 log_info(State, [Reason, NewState#state.failures,
                                  time_failed(NewState)])),
            {noreply, reconnect(tcp_error, NewState)}
    end;

handle_info({timeout, OldRef, ?RECONNECT_MSG}, State = #state{tref = NewRef})
  when OldRef =/= NewRef ->
    ?WARN("drain_id=~p channel_id=~p dest=~s at=reconnect "
          "err=ignoring_old_timer data=\"~p\" current_timer=~p",
          log_info(State, [OldRef,
                           case NewRef of
                               undefined -> not_set;
                               _ -> set
                           end])),
    {noreply, State};

handle_info({tcp_closed, S}, State = #state{sock = S}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s at=close sock=~p",
          log_info(State, [S])),
    {noreply, reconnect(tcp_closed, State#state{sock=undefined})};

handle_info({tcp_closed, S}, State = #state{}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s "
          "err=old_sock_close data=~p sock=~p",
          log_info(State, [S, S])),
    {noreply, State};

handle_info({tcp_error, S, Reason}, State = #state{sock = S}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s at=idle "
         "err=gen_tcp data=~p sock=~p",
          log_info(State, [Reason, S])),
    {noreply, reconnect(tcp_error, State#state{sock=undefined})};

handle_info({tcp, S, Data}, State = #state{sock = S}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_peer_data data=~p",
          log_info(State, [Data])),
    {noreply, State};

handle_info(Info, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info data=~p",
          log_info(State, [Info])),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec post(logplex_syslog_utils:syslog_msg(), inet:socket(),
           logplex_drain:token()) ->
                  'ok' |
                  {'error', term()}.
%% @private
post(Msg, Sock, DrainTok) when is_tuple(Msg) ->
    SyslogMsg = logplex_syslog_utils:to_msg(Msg, DrainTok),
    Packet = logplex_syslog_utils:frame([SyslogMsg, $\n]),
    gen_tcp:send(Sock, Packet).

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
    gen_tcp:connect(HostS, Port, [binary
                                 %% We don't expect data, but why not.
                                 ,{active, true}
                                 ,{exit_on_close, true}
                                 ,{keepalive, true}
                                 ,{packet, raw}
                                 ,{reuseaddr, true}
                                 ,{send_timeout,
                                   timer:seconds(SendTimeoutS)}
                                 ,{send_timeout_close, true}
                                 ]);
connect(#state{}) ->
    {error, bogus_port_number}.


maybe_reconnect(State = #state{tref = undefined}) ->
    reconnect(idle, State);
maybe_reconnect(State = #state{tref = Ref}) when is_reference(Ref) ->
    case erlang:read_timer(Ref) of
        false ->
            reconnect(idle, State);
        _ ->
            State
    end.


-spec reconnect('idle' | 'tcp_error' | 'tcp_closed', #state{}) -> #state{}.
%% @private
reconnect(_Reason, State = #state{failures = 0, last_good_time=undefined}) ->
    reconnect_in(logplex_app:config(tcp_syslog_reconnect_min, 30), State);
reconnect(_Reason, State = #state{failures = 0, last_good_time=T})
  when is_tuple(T), tuple_size(T) =:= 3 ->
    Min = logplex_app:config(tcp_syslog_reconnect_min, 30),
    SecsSinceConnect = timer:now_diff(os:timestamp(), T) div 1000000,
    case SecsSinceConnect of
        TooFew when TooFew < Min ->
            reconnect_in(Min, State);
        _EnoughTime ->
            reconnect_in(1, State)
    end;
reconnect(_Reason, State = #state{failures = F}) ->
    Max = logplex_app:config(tcp_syslog_backoff_max, 300),
    BackOff = case length(integer_to_list(Max, 2)) of
                  MaxExp when F > MaxExp -> Max;
                  _ -> 1 bsl F
              end,
    reconnect_in(BackOff, State).

reconnect_in(Seconds, State = #state{}) ->
    Ref = erlang:start_timer(timer:seconds(Seconds), self(), ?RECONNECT_MSG),
    (cancel_timer(State))#state{tref = Ref}.

cancel_timer(S = #state{tref = undefined}) -> S;

cancel_timer(S = #state{tref = Ref}) when is_reference(Ref) ->
    erlang:cancel_timer(Ref),
    S#state{tref = undefined}.

%% @private
tcp_good(State = #state{}) ->
    State#state{last_good_time = os:timestamp(),
                failures = 0}.

%% @private
%% Caller must ensure sock is closed before calling this.
tcp_bad(State = #state{sock = S}) when is_port(S) ->
    catch gen_tcp:close(S),
    State#state{sock = undefined};
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

post_buffer(#state{drain_tok = DrainTok,
                   buf = Buf,
                   sock = S} = State) ->
    case logplex_drain_buffer:pop(Buf) of
        {empty, NewBuf} ->
            State#state{buf=NewBuf};
        {{loss_indication, N, When}, NewBuf} ->
            msg_stat(drain_dropped, N, State),
            ?INFO("drain_id=~p channel_id=~p dest=~s at=loss"
                  " dropped=~p since=~s",
                  log_info(State, [N, logplex_syslog_utils:datetime(When)])),
            case logplex_app:config(tcp_syslog_send_loss_msg) of
                dont_send ->
                    post_buffer(State#state{buf=NewBuf});
                _ ->
                    case post(overflow_msg(N, When), S, DrainTok) of
                        ok ->
                            post_buffer(tcp_good(State#state{buf=NewBuf}));
                        {error, Reason} ->
                            ?ERR("drain_id=~p channel_id=~p dest=~s at=post "
                                 "err=gen_tcp data=~p",
                                 log_info(State, [Reason])),
                            reconnect(tcp_error,
                                      tcp_bad(State#state{sock=undefined}))
                    end
            end;
        {{msg, Msg}, NewBuf} ->
            case post(Msg, S, DrainTok) of
                ok ->
                    msg_stat(drain_delivered, 1, State),
                    post_buffer(tcp_good(State#state{buf=NewBuf}));
                {error, Reason} ->
                    %% Don't use NewBuf - we want to keep the old
                    %% buffer state so we don't drop the msg.
                    ?ERR("drain_id=~p channel_id=~p dest=~s at=post "
                         "err=gen_tcp data=~p",
                         log_info(State, [Reason])),
                    reconnect(tcp_error, tcp_bad(State#state{sock=undefined}))
            end
    end.

%% @private
overflow_msg(N, When) ->
    logplex_syslog_utils:fmt(local5,
                             warning,
                             now,
                             "logplex",
                             "logplex",
                             "Logplex drain buffer overflowed."
                             " ~p messages lost since ~s.",
                             [N,
                              logplex_syslog_utils:datetime(When)]
                            ).

%% @private
host_str({A,B,C,D}) ->
    Quads = [integer_to_list(Quad) || Quad <- [A,B,C,D]],
    string:join(Quads,".");
host_str(H)
  when is_list(H); is_binary(H) ->
    H.

%% @private
log_info(#state{drain_id=DrainId, channel_id=ChannelId, host=H, port=P}, Rest)
  when is_list(Rest) ->
    [DrainId, ChannelId, io_lib:format("~s:~p", [host_str(H), P]) | Rest].

-spec msg_stat('drain_dropped' | 'drain_buffered' | 'drain_delivered',
               pos_integer(), #state{}) -> any().
msg_stat(Key, N,
         #state{drain_id=DrainId, channel_id=ChannelId}) ->
    logplex_stats:incr(#drain_stat{drain_id=DrainId,
                                   channel_id=ChannelId,
                                   key=Key}, N).
