-module(logplex_logger).
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([log/1, log/2, log/3]).

-define(FACILITY, 5). %% facility 5 = user

log(Msg) when is_list(Msg) ->
    log(info, Msg, []).

log(Msg, Args) when is_list(Msg), is_list(Args) ->
    log(info, Msg, Args);

log(Level, Msg) when is_atom(Level), is_list(Msg) ->
    log(Level, Msg, []).
    
log(Level, Msg, Args) when is_atom(Level), is_list(Msg), is_list(Args) ->
    NewMsg =
        case Args of
            [] -> Msg;
            _ -> lists:flatten(io_lib:format(Msg, Args))
        end,
    {{Year,Month,Day},{Hour,Minutes,Seconds}} = erlang:localtime(),
    io:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B ~s~n", [Year, Month, Day, Hour, Minutes, Seconds, NewMsg]),
    syslog:send(NewMsg, [{facility, ?FACILITY}, {level, Level}, {ident, logplex}]).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_event
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          Other
%% @hidden
%%----------------------------------------------------------------------
init(_) ->
	{ok, []}.

%%----------------------------------------------------------------------
%% Func: handle_event/2
%% Returns: {ok, State}                                |
%%          {swap_handler, Args1, State1, Mod2, Args2} |
%%          remove_handler
%% @hidden
%%----------------------------------------------------------------------
handle_event({error, _Gleader, {_Pid, Format, Data}}, State) ->
    syslog:send(io_lib:format(Format, Data), [{facility, ?FACILITY}, {level, error}, {ident, logplex}]),
    {ok, State};

handle_event({error_report, _Gleader, {_Pid, std_error, Report}}, State) ->
    syslog:send(io_lib:format("~p", [Report]), [{facility, ?FACILITY}, {level, error}, {ident, logplex}]),
    {ok, State};

handle_event({error_report, _Gleader, {_Pid, Type, Report}}, State) ->
    syslog:send(io_lib:format("[~p] ~p", [Type, Report]), [{facility, ?FACILITY}, {level, error}, {ident, logplex}]),
    {ok, State};

handle_event({warning_msg, _Gleader, {_Pid, Format, Data}}, State) ->
    syslog:send(io_lib:format(Format, Data), [{facility, ?FACILITY}, {level, warning}, {ident, logplex}]),
    {ok, State};

handle_event({warning_report, _Gleader, {_Pid, std_warning, Report}}, State) ->
    syslog:send(io_lib:format("~p", [Report]), [{facility, ?FACILITY}, {level, warning}, {ident, logplex}]),
    {ok, State};

handle_event({warning_report, _Gleader, {_Pid, Type, Report}}, State) ->
    syslog:send(io_lib:format("[~p] ~p", [Type, Report]), [{facility, ?FACILITY}, {level, warning}, {ident, logplex}]),
    {ok, State};

handle_event({info_msg, _Gleader, {_Pid, Format, Data}}, State) ->
    syslog:send(io_lib:format(Format, Data), [{facility, ?FACILITY}, {level, info}, {ident, logplex}]),
    {ok, State};

handle_event({info_report, _Gleader, {_Pid, std_info, Report}}, State) ->
    syslog:send(io_lib:format("~p", [Report]), [{facility, ?FACILITY}, {level, info}, {ident, logplex}]),
    {ok, State};

handle_event({info_report, _Gleader, {_Pid, Type, Report}}, State) ->
    syslog:send(io_lib:format("[~p] ~p", [Type, Report]), [{facility, ?FACILITY}, {level, info}, {ident, logplex}]),
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_call/2
%% Returns: {ok, Reply, State}                                |
%%          {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%          {remove_handler, Reply}
%% @hidden
%%----------------------------------------------------------------------
handle_call(_Request, State) ->
    {ok, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {ok, State}
%% @hidden
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any
%% @hidden
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------