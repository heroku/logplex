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
-export([start_link/4
         ,shutdown/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id :: logplex_drain:id(),
                channel :: logplex_channel:id(),
                host :: string() | inet:ip_address(),
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

start_link(ChannelID, DrainID, Host, Port) ->
    gen_server:start_link(?MODULE,
                          [#state{id=DrainID,
                                  channel=ChannelID,
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
init([State0 = #state{id=ID, channel=Chan}]) ->
    try
        gproc:add_local_name({drain, ID}),
        %% This is ugly, but there's no other obvious way to do it.
        gproc:add_local_property({channel, Chan}, true),
        DrainSize = logplex_app:config(tcp_drain_buffer_size),
        State = State0#state{buf = logplex_drain_buffer:new(DrainSize),
        ?INFO("drain_id=~p channel_id=~p dest=~s at=spawned",
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
                                        id = Token,
                                        buf = Buf})
  when is_tuple(Msg) ->
    logplex_stats:incr({drain_buffered, Token}),
    NewBuf = logplex_drain_buffer:push(Msg, Buf),
    {noreply, State#state{buf=NewBuf}};

handle_info({post, Msg}, State = #state{id = Token,
                                        sock = S})
  when is_tuple(Msg) ->
    case post(Msg, S, Token) of
        ok ->
            logplex_stats:incr({drain_delivered, Token}),
            {noreply, tcp_good(State)};
        {error, Reason} ->
            ?ERR("drain_id=~p channel_id=~p dest=~s at=post err=gen_tcp data=~p",
                 log_info(State, [Reason])),
            {noreply, reconnect(tcp_error, State)}
    end;

handle_info(?RECONNECT_MSG, State = #state{sock = undefined}) ->
    State1 = State#state{tref = undefined},
    case connect(State1) of
        {ok, Sock} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=connected try=~p",
                  log_info(State1, [State1#state.failures + 1])),
            NewState = tcp_good(State1#state{sock=Sock}),
            {noreply, post_buffer(NewState)};
        {error, Reason} ->
            NewState = tcp_bad(State1),
            ?ERR("drain_id=~p channel_id=~p dest=~s at=connect "
                 "err=gen_tcp data=~p try=~p last_success=~s",
                 log_info(State, [Reason, NewState#state.failures,
                                  time_failed(NewState)])),
            {noreply, reconnect(tcp_error, NewState)}
    end;

handle_info({tcp_closed, S}, State = #state{sock = S}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s at=close",
          log_info(State, [])),
    {noreply, reconnect(tcp_closed, State#state{sock=undefined})};

handle_info({tcp_error, S, Reason}, State = #state{sock = S}) ->
    ?ERR("drain_id=~p channel_id=~p dest=~s at=idle "
         "err=gen_tcp data=~p",
          log_info(State, [Reason])),
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
           logplex_drain:id()) ->
                  'ok' |
                  {'error', term()}.
%% @private
post(Msg, Sock, Token) when is_tuple(Msg) ->
    SyslogMsg = logplex_syslog_utils:to_msg(Msg, Token),
    Packet = logplex_syslog_utils:frame(SyslogMsg),
    gen_tcp:send(Sock, Packet).

%% @private
connect(#state{sock = undefined, host=Host, port=Port}) ->
    SendTimeoutS = logplex_app:config(tcp_syslog_send_timeout_secs),
    gen_tcp:connect(Host, Port, [binary
                                 %% We don't expect data, but why not.
                                 ,{active, true}
                                 ,{exit_on_close, true}
                                 ,{keepalive, true}
                                 ,{packet, raw}
                                 ,{reuseaddr, true}
                                 ,{send_timeout,
                                   timer:seconds(SendTimeoutS)}
                                 ,{send_timeout_close, true}
                                 ]).

-spec reconnect('tcp_error' | 'tcp_closed', #state{}) -> #state{}.
%% @private
reconnect(_Reason, State = #state{failures = F}) ->
    BackOff = erlang:min(logplex_app:config(tcp_syslog_backoff_max),
                         1 bsl F),
    Ref = erlang:send_after(timer:seconds(BackOff), self(), ?RECONNECT_MSG),
    State#state{tref=Ref}.

%% @private
tcp_good(State = #state{}) ->
    State#state{last_good_time = os:timestamp(),
                failures = 0}.

%% @private
%% Caller must ensure sock is closed before calling this.
tcp_bad(State = #state{failures = F}) ->
    State#state{failures = F + 1,
                sock = undefined}.

-spec time_failed(#state{}) -> iolist().
%% @private
time_failed(State = #state{}) ->
    time_failed(os:timestamp(), State).
time_failed(Now, #state{last_good_time=T0})
  when is_tuple(T0) ->
    io_lib:format("~fs ago", [timer:now_diff(Now, T0) / 1000000]);
time_failed(_, #state{last_good_time=undefined}) ->
    "never".

post_buffer(State = #state{id = ID, buf = Buf, sock = S}) ->
    case logplex_drain_buffer:pop(Buf) of
        {empty, NewBuf} ->
            State#state{buf=NewBuf};
        {Item, NewBuf} ->
            Msg = case Item of
                      {msg, M} -> M;
                      {loss_indication, N, When} ->
                          logplex_stats:incr({drain_dropped, ID}),
                          ?INFO("drain_id=~p channel_id=~p dest=~s at=loss"
                                " dropped=~p since=~s",
                                log_info(State, [N, logplex_syslog_utils:datetime(When)])),
                          overflow_msg(N, When)
                  end,
            case post(Msg, S, ID) of
                ok ->
                    logplex_stats:incr({drain_delivered, ID}),
                    post_buffer(tcp_good(State#state{buf=NewBuf}));
                {error, Reason} ->
                    ?ERR("drain_id=~p channel_id=~p dest=~s at=post err=gen_tcp data=~p",
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
log_info(#state{id=ID, channel=Channel, host=H, port=P}, Rest)
  when is_list(Rest) ->
    [ID, Channel, io_lib:format("~s:~p", [host_str(H), P]) | Rest].
