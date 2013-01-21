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
    set_cookie(),
    redo_opts(),
    read_git_branch(),
    read_availability_zone(),
    read_environment(),
    setup_crashdumps(),
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

set_cookie() ->
    case os:getenv("LOGPLEX_COOKIE") of
        false -> ok;
        Cookie -> erlang:set_cookie(node(), list_to_atom(Cookie))
    end.

read_git_branch() ->
    GitOutput = hd(string:tokens(os:cmd("git status"), "\n")),
    case re:run(GitOutput, "\# On branch (\\S+)", [{capture, all_but_first, list}]) of
        {match,[Branch]} ->
            application:set_env(logplex, git_branch, Branch);
        _ ->
            ok
    end.

read_availability_zone() ->
    case httpc:request("http://169.254.169.254/latest/meta-data/placement/availability-zone") of
        {ok,{{_,200,_}, _Headers, Zone}} ->
            application:set_env(logplex, availability_zone, Zone);
        _ ->
            ok
    end.

read_environment() ->
    [case os:getenv(SK) of
         false -> ok;
         Val ->
             application:set_env(?APP, K, Val)
     end
     || {K, SK} <- [ {instance_name, "INSTANCE_NAME"} ]].

boot_pagerduty() ->
    case os:getenv("HEROKU_DOMAIN") of
        "heroku.com" ->
            case os:getenv("PAGERDUTY") of
                "0" -> ok;
                _ ->
                    ok = application:load(pagerduty),
                    application:set_env(pagerduty, service_key, os:getenv("ROUTING_PAGERDUTY_SERVICE_KEY")),
                    a_start(pagerduty, temporary),
                    ok = error_logger:add_report_handler(logplex_report_handler)
            end;
        _ ->
            ok
    end.

setup_redgrid_vals() ->
    application:load(redgrid),
    application:set_env(redgrid, local_ip, os:getenv("LOCAL_IP")),
    application:set_env(redgrid, redis_url, os:getenv("LOGPLEX_STATS_REDIS_URL")),
    application:set_env(redgrid, domain, os:getenv("HEROKU_DOMAIN")),
    ok.

setup_redis_shards() ->
    URLs = case os:getenv("LOGPLEX_SHARD_URLS") of
               false ->
                   erlang:error({fatal_config_error,
                                 missing_logplex_shard_urls});
               [] ->
                   case os:getenv("LOGPLEX_CONFIG_REDIS_URL") of
                       false -> ["redis://127.0.0.1:6379/"];
                       Url -> [Url]
                   end;
               UrlString when is_list(UrlString) ->
                   string:tokens(UrlString, ",")
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
                false ->
                    undefined;
                File ->
                    filename:dirname(File)
            end;
        Dir -> Dir
    end.

logplex_work_queue_args() ->
    MaxLength =
        case os:getenv("LOGPLEX_QUEUE_LENGTH") of
            false -> ?DEFAULT_LOGPLEX_QUEUE_LENGTH;
            StrNum1 -> list_to_integer(StrNum1)
        end,
    NumWorkers =
        case os:getenv("LOGPLEX_WORKERS") of
            false -> ?DEFAULT_LOGPLEX_WORKERS;
            StrNum2 -> list_to_integer(StrNum2)
        end,
    [{name, "logplex_work_queue"},
     {max_length, MaxLength},
     {num_workers, NumWorkers},
     {worker_sup, logplex_worker_sup},
     {worker_args, []}].

nsync_opts() ->
    RedisOpts = logplex_utils:redis_opts("LOGPLEX_CONFIG_REDIS_URL"),
    Ip = case proplists:get_value(ip, RedisOpts) of
        {_,_,_,_}=L -> string:join([integer_to_list(I) || I <- tuple_to_list(L)], ".");
        Other -> Other
    end,
    RedisOpts1 = proplists:delete(ip, RedisOpts),
    RedisOpts2 = [{host, Ip} | RedisOpts1],
    [{callback, {nsync_callback, handle, []}} | RedisOpts2].


config(Key, Default) ->
    case application:get_env(logplex, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(redis_stats_uri) ->
    redo_uri:parse(os:getenv("LOGPLEX_STATS_REDIS_URL"));
config(Key) ->
    case application:get_env(logplex, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.

config() ->
    application:get_all_env(logplex).

a_start(App, Type) ->
    start_ok(App, Type, application:start(App, Type)).

start_ok(_App, _Type, ok) -> ok;
start_ok(_App, _Type, {error, {already_started, _App}}) -> ok;
start_ok(App, Type, {error, {not_started, Dep}}) ->
    ok = a_start(Dep, Type),
    a_start(App, Type);
start_ok(App, _Type, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

redo_opts() ->
    case os:getenv("LOGPLEX_CONFIG_REDIS_URL") of
        false -> [];
        Url ->
            ParsedUrl = redo_uri:parse(Url),
            application:set_env(?APP, config_redis_url, ParsedUrl)
    end.
