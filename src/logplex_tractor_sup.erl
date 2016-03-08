-module(logplex_tractor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(ch_sup, []).

init(_Args) ->
    Name = {local, tractor},
    SupFlags = #{strategy => rest_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => tractor_client,
                    start => {sdiff_client, start_link, logplex_tractor_cb:sdiff_opts(Name)},
                    restart => permanent,
                    shutdown => 2000,
                    type => worker,
                    modules => [sdiff_client]},
                 #{id => tractor_sync,
                   start => {logplex_tractor_sync, start_link, [Name]},
                   restart => permanent,
                   shutdown => 2000,
                   type => worker,
                   modules => [logplex_tractor_sync]}],
    {ok, {SupFlags, ChildSpecs}}.
