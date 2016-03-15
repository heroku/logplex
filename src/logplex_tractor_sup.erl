-module(logplex_tractor_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    logplex_tractor_sync:init_tab(),

    {ok, InitOpts} = logplex_tractor_cb:sdiff_opts(),

    ChildSpecs = [#{id => Name,
                     start => {logplex_tractor_sync, start_link, [Opts]},
                     restart => permanent,
                     shutdown => 2000,
                     type => worker,
                     modules => [logplex_tractor_sync]} || [Name | _]=Opts <- InitOpts],

    {ok, {SupFlags, ChildSpecs}}.
