-module(tractor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(ch_sup, []).

init(_Args) ->
    Name = {local, tractor},
    SupFlags = #{strategy => one_for_all, intensity => 1, period => 5},
    ChildSpecs = [#{id => tractor_client,
                    start => {sdiff_client, start_link, tractor_sync:opts(Name)},
                    restart => permanent,
                    shutdown => 2000,
                    type => worker,
                    modules => [sdiff_client]},
                 #{id => tractor_sync,
                   start => {tractor_sync, start_link, [Name]}}],
    {ok, {SupFlags, ChildSpecs}}.
