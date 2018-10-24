-module(logplex_redis_reader_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/1,
         remove_child/1,
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Spec) ->
    supervisor:start_child(?MODULE, Spec).

remove_child(Pid) ->
    [begin
         supervisor:terminate_child(?MODULE, Id),
         supervisor:delete_child(?MODULE, Id)
     end || {Id, ChildPid, _, _} <- supervisor:which_children(?MODULE), ChildPid == Pid].

init([]) ->
    {ok, {{one_for_one, 1000, 300}, []}}.
