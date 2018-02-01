%% Copyright (c) 2018 Salesforce.com, Inc
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

%% @doc
%% Implements a process which polls the state of ranch servers and
%% emits metrics to folsom.
%% @end
-module(logplex_ranch_metrics).

-include("logplex_logging.hrl").

-behavior(gen_server).

%% API
-export([start_link/0]).
-export([report_status/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the metrics server.
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Report status of each metric to folsom.
-spec report_status() -> ok.
report_status() ->
    gen_server:cast(?MODULE, report_status).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    refresh_cycle(),
    {ok, no_state}.

%% @private
handle_call(Msg, _From, State) ->
    ?WARN("type=unexpected_call msg='~p'", [Msg]),
    Error = {unexpected_call, Msg},
    {reply, Error, State}.

%% @private
handle_cast(report_status, State) ->
    [notify_metric(Server) || Server <- ranch_servers()],
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARN("type=unexpected_cast msg='~p'", [Msg]),
    {noreply, State}.

%% @private
handle_info(cycle_timeout, State) ->
    report_status(),
    refresh_cycle(),
    {noreply, State};
handle_info(Msg, State) ->
    ?WARN("type=unexpected_info msg='~p'", [Msg]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% internal functions
%%%===================================================================

refresh_cycle() ->
    Timeout = timer:seconds(60),
    erlang:send_after(Timeout, self(), cycle_timeout),
    ok.

ranch_servers() ->
    [
     logplex_logs_rest,
     logplex_api_v3
    ].

notify_metric(Server) ->
    Metric = metric(Server),
    Value = value(Server),
    folsom_metrics:notify(Metric, Value, gauge).

metric(Server) ->
    list_to_binary([<<"logplex.ranch_server.">>, atom_to_list(Server), <<".connections">>]).

value(Server) ->
    ranch_server:count_connections(Server).
