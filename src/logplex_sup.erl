%%%-------------------------------------------------------------------
%% @copyright Heroku, 2012
%% @author Geoff Cant <geoff@heroku.com>
%% @version {@vsn}, {@date} {@time}
%% @doc Logplex supervisor
%% @end
%%%-------------------------------------------------------------------
-module(logplex_sup).

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
    {ok,
     {{one_for_one, 5, 10},
      [{logplex_db, {logplex_db, start_link, []},
        permanent, 2000, worker, [logplex_db]}
       ,{config_redis,
         {redo, start_link, [config, redo_config(config_redis_url)]},
         permanent, 2000, worker, [redo]}
       ,{config_redis_block,
         {redo_block, start_link, [config_block, redo_config(config_redis_url)]},
         permanent, 2000, worker, [redo_block]}
       ,{logplex_drain_sup,
         {logplex_drain_sup, start_link, []},
         permanent, 2000, supervisor, [logplex_drain_sup]}
       ,{nsync, {nsync, start_link, [logplex_app:nsync_opts()]},
         permanent, 2000, worker, [nsync]}
       ,{redgrid, {redgrid, start_link, []},
         permanent, 2000, worker, [redgrid]}
       ,{logplex_realtime, {logplex_realtime, start_link,
                            [redo_config(stats_redis_url)]},
         permanent, 2000, worker, [logplex_realtime]}
       ,{logplex_stats, {logplex_stats, start_link, []},
         permanent, 2000, worker, [logplex_stats]}

       ,{logplex_tail, {logplex_tail, start_link, []},
         permanent, 2000, worker, [logplex_tail]}

       ,{logplex_redis_writer_sup,
         {logplex_worker_sup, start_link,
          [logplex_redis_writer_sup, logplex_redis_writer]},
         permanent, 2000, worker, [logplex_redis_writer_sup]}
       ,{logplex_read_queue_sup,
         {logplex_queue_sup, start_link,
          [logplex_read_queue_sup, logplex_read_queue]},
         permanent, 2000, worker, [logplex_read_queue_sup]}
       ,{logplex_reader_sup,
         {logplex_worker_sup, start_link,
          [logplex_reader_sup, logplex_reader]},
         permanent, 2000, worker, [logplex_reader_sup]}

       ,{logplex_shard, {logplex_shard, start_link, []},
         permanent, 2000, worker, [logplex_shard]}

       ,{logplex_monitor, {logplex_mon_sup, start_link, []},
         permanent, 8000, supervisor, [logplex_mon_sup]}

       %% All tcp listen processes start from the 'listen' start phase
       %% in logplex_app

      ]
    }}.

%%====================================================================
%% Internal functions
%%====================================================================

redo_config(ConfigVar) ->
    redo_uri:parse(logplex_app:config(ConfigVar)).
