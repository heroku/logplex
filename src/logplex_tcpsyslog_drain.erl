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
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {id :: binary(),
                channel :: binary(),
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
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link(ChannelID, DrainID, Host, Port) ->
    gen_server:start_link(?MODULE,
                          [#state{id=DrainID,
                                  channel=ChannelID,
                                  host=Host,
                                  port=Port}],
                          []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([State0 = #state{id=ID, channel=Chan}]) ->
    gproc:add_local_name({drain, ID}),
    %% This is ugly, but there's no other obvious way to do it.
    gproc:add_local_property({channel, Chan}, true),
    DrainSize = logplex_app:config(tcp_drain_buffer_size),
    {ok, State0#state{buf = logplex_drain_buffer:new(DrainSize)}, hibernate}.

%% @private
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%% @private
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info({post, Msg}, State = #state{sock=undefined,
                                        buf=Buf})
  when is_tuple(Msg) ->
    NewBuf = logplex_drain_buffer:push(Msg, Buf),
    {noreply, State#state{buf=NewBuf}};

handle_info({post, Msg}, State = #state{id = Token,
                                        sock = S})
  when is_tuple(Msg) ->
    case post(Msg, S, Token) of
        ok ->
            {noreply, tcp_good(State)};
        {error, Reason} ->
            ?ERR("[~p] (~p:~p) Couldn't write syslog message: ~p",
                 [State#state.id, State#state.host, State#state.port,
                  Reason]),
            {noreply, reconnect(tcp_error, State)}
    end;

handle_info(?RECONNECT_MSG, State = #state{sock = undefined}) ->
    State1 = State#state{tref = undefined},
    case connect(State1) of
        {ok, Sock} ->
            ?INFO("[~s] connected to ~p:~p on try ~p.",
                  [State1#state.id, State1#state.host, State1#state.port,
                   State1#state.failures + 1]),
            NewState = tcp_good(State1#state{sock=Sock}),
            {noreply, post_buffer(NewState)};
        {error, Reason} ->
            NewState = tcp_bad(State1),
            ?ERR("[~s] Couldn't connect to ~p:~p; ~p"
                 " (try ~p, last success: ~s)",
                 [NewState#state.id, NewState#state.host, NewState#state.port,
                  Reason, NewState#state.failures, time_failed(NewState)]),
            {noreply, reconnect(tcp_error, NewState)}
    end;

handle_info({tcp_closed, S}, State = #state{sock = S}) ->
    {noreply, reconnect(tcp_closed, State#state{sock=undefined})};

handle_info({tcp_error, S, _Reason}, State = #state{sock = S}) ->
    {noreply, reconnect(tcp_error, State#state{sock=undefined})};

handle_info({tcp, S, _Data}, State = #state{sock = S}) ->
    {stop, not_implemented, State};

handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
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
           iolist() | binary()) ->
                  'ok' |
                  {'error', term()}.
post(Msg, Sock, Token) when is_tuple(Msg) ->
    SyslogMsg = logplex_syslog_utils:to_msg(Msg, Token),
    Packet = logplex_syslog_utils:frame(SyslogMsg),
    gen_tcp:send(Sock, Packet).

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
reconnect(_Reason, State = #state{failures = F}) ->
    BackOff = erlang:min(logplex_app:config(tcp_syslog_backoff_max),
                         1 bsl F),
    Ref = erlang:send_after(timer:seconds(BackOff), self(), ?RECONNECT_MSG),
    State#state{tref=Ref}.

tcp_good(State = #state{}) ->
    State#state{last_good_time = os:timestamp(),
                failures = 0}.

%% Caller must ensure sock is closed before calling this.
tcp_bad(State = #state{failures = F}) ->
    State#state{failures = F + 1,
                sock = undefined}.

-spec time_failed(#state{}) -> iolist().
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
                          ?INFO("Drain ~s dropped ~p messages since ~s.",
                                [ID, N, logplex_syslog_utils:datetime(When)]),
                          overflow_msg(N, When)
                  end,
            case post(Msg, S, ID) of
                ok ->
                    post_buffer(tcp_good(State#state{buf=NewBuf}));
                {error, Reason} ->
                    ?ERR("[~p] (~p:~p) Couldn't write syslog message: ~p",
                         [State#state.id, State#state.host, State#state.port,
                          Reason]),
                    reconnect(tcp_error, tcp_bad(State#state{sock=undefined}))
            end
    end.

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
