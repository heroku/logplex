%%%-------------------------------------------------------------------
%% @copyright 2012 Heroku
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc syslog (tcp_proxy) supervisor
%% @end
%%%-------------------------------------------------------------------
-module(logplex_syslog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         child_spec/0]).

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

child_spec() ->
    {?MODULE,
     {?MODULE, start_link, []},
     permanent, 2000, supervisor, [?MODULE]}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok,
     { {one_for_all, 0, 1},
       [{tcp_proxy_sup, {tcp_proxy_sup, start_link, []},
                permanent, 2000, worker, [tcp_proxy_sup]},
        tcp_acceptor:child_spec()]} }.

%%====================================================================
%% Internal functions
%%====================================================================
