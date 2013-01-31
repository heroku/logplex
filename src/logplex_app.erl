%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
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
-module(logplex_app).
-behaviour(application).

-define(APP, logplex).


%% Application callbacks
-export([start/2, start_phase/3, stop/1]).

-export([logplex_work_queue_args/0
         ,nsync_opts/0
         ,config/0
         ,config/1
         ,config/2
         ,start/0
         ,a_start/2
        ]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

%%%===================================================================
%%% Convenience Functions
%%%===================================================================

start() ->
    a_start(?APP, permanent).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ?INFO("at=start", []),
    cache_os_envvars(),
    setup_crashdumps(),
    set_cookie(),
    read_git_branch(),
    read_availability_zone(),
    boot_pagerduty(),
    setup_redgrid_vals(),
    setup_redis_shards(),
    application:start(nsync),
    logplex_sup:start_link().

stop(_State) ->
    ?INFO("at=stop", []),
    ok.

start_phase(listen, normal, _Args) ->
    {ok, _} = supervisor:start_child(logplex_sup,
                                     logplex_api:child_spec()),
    {ok, _} = supervisor:start_child(logplex_sup,
                                     logplex_syslog_sup:child_spec()),
    {ok, _} = supervisor:start_child(logplex_sup,
                                     logplex_logs_rest:child_spec()),
    ok.

cache_os_envvars() ->
    %% cache {key, EnvVarPrefList}
    %% Last env var wins.
    cache_os_envvars([
                      %% availability_zone cached by read_availability_zone()
                      {auth_key, ["LOGPLEX_AUTH_KEY"]}
                      ,{cloud_name, ["LOGPLEX_CLOUD_NAME", "HEROKU_DOMAIN"]}
                      %% core_userpass is deprecated
                      ,{config_redis_url, ["LOGPLEX_CONFIG_REDIS_URL"]}
                      ,{cookie, ["LOGPLEX_COOKIE"]}
                      %% ERL_CRASH_DUMP read by setup_crashdumps
                      %% git_branch cached by read_git_branch()
                      ,{instance_name, ["INSTANCE_NAME"]}
                      ,{local_ip, ["LOCAL_IP"]}
                      ,{logplex_shard_urls, ["LOGPLEX_SHARD_URLS"]}
                      ,{pagerduty, ["PAGERDUTY"],
                        optional}
                      ,{pagerduty_key, ["ROUTING_PAGERDUTY_SERVICE_KEY"],
                        optional}
                      ,{redgrid_redis_url, ["LOGPLEX_CONFIG_REDIS_URL",
                                            "LOGPLEX_STATS_REDIS_URL"]}
                     ]),
    ok.

cache_os_envvars(Vars) ->
    [ cache_os_envvar(Var)
      || Var <- Vars ],
    ok.

cache_os_envvar({Var, Keys}) ->
    cache_os_envvar(Var, Keys),
    config(Var);
cache_os_envvar({Var, Keys, optional}) ->
    cache_os_envvar(Var, Keys).

cache_os_envvar(_Var, []) -> ok;
cache_os_envvar(Var, [Key | Keys]) ->
    case os:getenv(Key) of
        false -> ok;
        Value -> set_config(Var, Value)
    end,
    cache_os_envvar(Var, Keys).

set_config(KeyS, Value) when is_list(KeyS) ->
    set_config(list_to_atom(KeyS), Value);
set_config(Key, Value) when is_atom(Key) ->
    application:set_env(?APP, Key, Value).

config() ->
    application:get_all_env(logplex).

config(Key) when is_atom(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.

config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

set_cookie() ->
    case config(cookie) of
        "" -> ok;
        Cookie -> erlang:set_cookie(node(), list_to_atom(Cookie))
    end.

read_git_branch() ->
    GitOutput = hd(string:tokens(os:cmd("git status"), "\n")),
    case re:run(GitOutput, "\# On branch (\\S+)", [{capture, all_but_first, list}]) of
        {match,[Branch]} ->
            set_config(git_branch, Branch);
        _ ->
            set_config(git_branch, "unknown")
    end.

read_availability_zone() ->
    Url = "http://169.254.169.254/latest/meta-data/placement/availability-zone",
    case httpc:request(get, {Url, []}, [{timeout, 2000}, {connect_timeout, 1000}], []) of
        {ok,{{_,200,_}, _Headers, Zone}} ->
            application:set_env(logplex, availability_zone, Zone);
        _ ->
            ok
    end.

boot_pagerduty() ->
    case config(cloud_name) of
        "heroku.com" ->
            case config(pagerduty) of
                "0" -> ok;
                _ ->
                    ok = application:load(pagerduty),
                    application:set_env(pagerduty, service_key, config(pagerduty_key)),
                    a_start(pagerduty, temporary),
                    ok = error_logger:add_report_handler(logplex_report_handler)
            end;
        _ ->
            ok
    end.

setup_redgrid_vals() ->
    application:load(redgrid),
    application:set_env(redgrid, local_ip, config(local_ip)),
    application:set_env(redgrid, redis_url, config(redgrid_redis_url)),
    application:set_env(redgrid, domain, config(cloud_name)),
    ok.

setup_redis_shards() ->
    URLs = case config(logplex_shard_urls) of
               UrlString when is_list(UrlString), UrlString =/= [] ->
                   string:tokens(UrlString, ",");
               _ ->
                   [config(config_redis_url)]
           end,
    application:set_env(logplex, logplex_shard_urls,
                        logplex_shard:redis_sort(URLs)).

setup_crashdumps() ->
    case dumpdir() of
        undefined -> ok;
        Dir ->
            File = string:join([config(instance_name),
                                "boot",
                                logplex_syslog_utils:datetime(now)], "_"),
            DumpFile = filename:join(Dir, File),
            ?INFO("at=setup_crashdumps dumpfile=~p",
                  [DumpFile]),
            os:putenv("ERL_CRASH_DUMP", DumpFile)
    end.

dumpdir() ->
    case config(crashdump_dir, undefined) of
        undefined ->
            case os:getenv("ERL_CRASH_DUMP") of
                false -> undefined;
                File -> filename:dirname(File)
            end;
        Dir -> Dir
    end.

logplex_work_queue_args() ->
    MaxLength = logplex_utils:to_int(config(queue_length)),
    NumWorkers = logplex_utils:to_int(config(workers)),
    [{name, "logplex_work_queue"},
     {max_length, MaxLength},
     {num_workers, NumWorkers},
     {worker_sup, logplex_worker_sup},
     {worker_args, []}].

nsync_opts() ->
    RedisUrl = config(config_redis_url),
    RedisOpts = logplex_utils:parse_redis_url(RedisUrl),
    Ip = case proplists:get_value(ip, RedisOpts) of
             {_,_,_,_}=L ->
                 string:join([integer_to_list(I)
                              || I <- tuple_to_list(L)], ".");
             Other -> Other
         end,
    RedisOpts1 = proplists:delete(ip, RedisOpts),
    RedisOpts2 = [{host, Ip} | RedisOpts1],
    [{callback, {nsync_callback, handle, []}} | RedisOpts2].

a_start(App, Type) ->
    start_ok(App, Type, application:start(App, Type)).

start_ok(_App, _Type, ok) -> ok;
start_ok(_App, _Type, {error, {already_started, _App}}) -> ok;
start_ok(App, Type, {error, {not_started, Dep}}) ->
    ok = a_start(Dep, Type),
    a_start(App, Type);
start_ok(App, _Type, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
