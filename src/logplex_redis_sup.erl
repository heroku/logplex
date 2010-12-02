-module(logplex_redis_sup).
-behavior(supervisor).

-export([start_link/0, init/1, start_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(RedisOpts) ->
    supervisor:start_child(?MODULE, [RedisOpts]).

init([]) ->
    {ok, {{simple_one_for_one, 100, 1}, [
        {logplex_redis_writer, {logplex_redis_writer, start_link, []}, transient, 2000, worker, [logplex_redis_writer]}
    ]}}.