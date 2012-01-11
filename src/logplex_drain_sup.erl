%%%-------------------------------------------------------------------
%% @copyright You, 2036
%% @author You <erlanghacker@example.com>
%% @version {@vsn}, {@date} {@time}
%% @doc drain supervisor
%% @end
%%%-------------------------------------------------------------------
-module(logplex_drain_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 20, 10}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
