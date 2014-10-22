%%%-------------------------------------------------------------------
%%% @author Alex Arnell <alex.arnell@gmail.com>
%%% @copyright 2014 Heroku
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(logplex_ehmon).

-behaviour(gen_server).

%% API
-export([start_link/2,
        send_report/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("logplex_logging.hrl").

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECONNECT_DELAY, 15000).

-record(state, {
          host = '127.0.0.1',
          port = 8000,
          connect_timeout = 5000,
          reconnect_delay = undefined,
          socket = undefined
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

send_report(Report) ->
    gen_server:call(?MODULE, {send_report, Report}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Host, Port]) ->
    State = #state{host=Host,
                   port=Port},
    State1 = reconnect_after_delay(State),
    {ok, State1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send_report, Report}, _From, State0) ->
    {Reply, State} = handle_send_report(Report, State0),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp_error, _Socket, Reason}, State) ->
    % will be followed by a close
    ?INFO("at=handle_info error=tcp_error reason=~p", [Reason]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State0) ->
    State = reconnect_after_delay(State0),
    ?INFO("at=handle_info error=tcp_closed reconnect_in=~pms", [State#state.reconnect_delay]),
    {noreply, State#state{ socket=undefined }};
handle_info(reconnect, State0) ->
    case connect(State0) of
        {ok, State} ->
            {noreply, State#state{ reconnect_delay=undefined }};
        {error, {connection_error, Reason}} ->
            error_logger:warning_msg("Shh connection error: ~p~n", [Reason]),
            State = reconnect_after_delay(State0),
            {noreply, State}
    end;
handle_info(Info, State) ->
    ?INFO("at=handle_info error=unexpected_info info=~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(State) ->
    case gen_tcp:connect(State#state.host, State#state.port,
                         ?SOCKET_OPTS, State#state.connect_timeout) of
        {ok, Socket} ->
            {ok, State#state{ socket=Socket }};
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

reconnect_after_delay(State0) ->
    State = next_reconnect_delay(State0),
    erlang:send_after(State#state.reconnect_delay, self(), reconnect), 
    State.

next_reconnect_delay(State=#state{ reconnect_delay=undefined }) ->
    State#state{ reconnect_delay=0 };
next_reconnect_delay(State=#state{ reconnect_delay=0}) ->
    State#state{ reconnect_delay=?RECONNECT_DELAY };
next_reconnect_delay(State) ->
    State.

handle_send_report(Report, State) ->
    Reply = tcp_send_report(State#state.socket, format_report(Report)),
    {Reply, State}.

format_report(Report) ->
    format_report(Report, []).

format_report([], Out) ->
    iolist_to_binary(Out);
format_report([Metric | Rest], Out) ->
    format_report(Rest, [format_metric(Metric) | Out]).

%% format_value(rq, Amount) ->
%%     Amount;
%% format_value(memtot, Amount) ->
%%     [Amount, "g", "b"];
%% format_value(memproc, Amount) ->
%%     [Amount, "g", "b"];
%% format_value(memets, Amount) ->
%%     [Amount, "g", "b"];
%% format_value(membin, Amount) ->
%%     format_metric({membin, Val, gauge, bytes}).
%% format_value(memcode, Amount) ->
%%     format_metric({memcode, Val, gauge, bytes}).
format_metric({Name, Val, Type, Unit}) ->
    io_lib:format("~w ~w ~w ~w ~s~n", [unixtime(), Name, Val, format_type(Type), format_unit(Unit)]).

format_type(gauge) ->
    g;
format_type(counter) ->
    c.

format_unit(undefined) ->
    "";
format_unit(percent) ->
    "Percent,%";
format_unit(bytes) ->
    "Bytes,b";
format_unit(seconds) ->
    "Seconds,s";
format_unit(milliSeconds) ->
    "MilliSeconds,ms";
format_unit(nanoSeconds) ->
    "NanoSeconds,ns";
format_unit(requests) ->
    "Requests,reqs";
format_unit(errors) ->
    "Errors,errs";
format_unit(packets) ->
    "Packets,pkts";
format_unit(iNodes) ->
    "INodes,inodes";
format_unit(files) ->
    "Files,files";
format_unit(processes) ->
    "Processes,procs";
format_unit(connections) ->
    "Connections,conns";
format_unit(sockets) ->
    "Sockets,socks";
format_unit(avg) ->
    "Avg,avg";
format_unit(objects) ->
    "Objects,objs";
format_unit(routines) ->
    "Routines,routines".

tcp_send_report(undefined, _Report) ->
    {error, not_connected};
tcp_send_report(Socket, Report) ->
    case gen_tcp:send(Socket, Report) of
        ok -> ok;
        Err={error, _Reason} ->
            Err
    end.

unixtime() ->
    unixtime(os:timestamp()).

unixtime({Mega, Sec, Micro}) ->
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.
