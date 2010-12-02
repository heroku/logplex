-module(logplex_drain_sup).
-behavior(supervisor).

-export([start_link/0, init/1, start_child/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 100, 1}, [
        {logplex_drain_writer, {logplex_drain_writer, start_link, []}, transient, 2000, worker, [logplex_drain_writer]}
    ]}}.