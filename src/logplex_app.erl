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

-export([nsync_opts/0
         ,config/0
         ,config/1
         ,config/2
         ,set_config/2
         ,start/0
         ,a_start/2
        ]).

-export([cache_os_envvars/0]). % for tests

-export([elb_healthcheck/0]).

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
    heroku_crashdumps_app:start(),
    set_cookie(),
    read_git_branch(),
    read_availability_zone(),
    boot_pagerduty(),
    logplex_realtime:setup_metrics(),
    setup_redgrid_vals(),
    setup_redis_shards(),
    application:start(nsync),
    logplex_sup:start_link().

stop(_State) ->
    ?INFO("at=stop", []),
    ok.

start_phase(listen, normal, _Args) ->
    setup_firehose(),
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
                      %% git_branch cached by read_git_branch()
                      ,{instance_name, ["INSTANCE_NAME"]}
                      ,{metrics_channel_id, ["METRICS_CHANNEL_ID"],
                        optional,
                        integer}
                      ,{firehose_channel_ids, ["FIREHOSE_CHANNEL_IDS"],
                        optional}
                      ,{firehose_filter_tokens, ["FIREHOSE_FILTER_TOKENS"],
                        optional}
                      ,{local_ip, ["LOCAL_IP"]}
                      ,{metrics_namespace, ["METRICS_NAMESPACE"],
                        optional}
                      ,{logplex_shard_urls, ["LOGPLEX_SHARD_URLS"]}
                      ,{pagerduty, ["PAGERDUTY"],
                        optional}
                      ,{pagerduty_key, ["ROUTING_PAGERDUTY_SERVICE_KEY"],
                        optional}
                      ,{redgrid_redis_url, ["LOGPLEX_REDGRID_REDIS_URL"]}
                      ,{force_gc_memory, ["LOGPLEX_FORCE_GC_MEMORY"],
                        optional, %% in bytes
                        integer}
                      ,{api_endpoint_url, ["LOGPLEX_API_ENDPOINT_URL"],
                       optional}
                     ]),
    ok.

cache_os_envvars(Vars) ->
    [ cache_os_envvar(Var)
      || Var <- Vars ],
    ok.

cache_os_envvar({Var, Keys}) ->
    cache_os_envvar(Var, Keys, string),
    config(Var);
cache_os_envvar({Var, Keys, optional}) ->
    cache_os_envvar(Var, Keys, string);
cache_os_envvar({Var, Keys, optional, integer}) ->
    cache_os_envvar(Var, Keys, integer).

%% Read os environment for Key and write to var if set.
%% Keys later in the list overwrite earlier values allowing multiple
%% env keys to be the source for a single app environment variable
%% with priority.
cache_os_envvar(Var, Keys, Type) ->
    [ case os:getenv(Key) of
          false -> ok;
          Value -> set_config(Var, set_config_value(Value, Type))
      end || Key <- Keys ].

set_config(KeyS, Value) when is_list(KeyS) ->
    set_config(list_to_atom(KeyS), Value);
set_config(Key, Value) when is_atom(Key) ->
    ?INFO("at=update_running_config key=~p value=~1000p",
          [Key, Value]),
    application:set_env(?APP, Key, Value).

set_config_value(Value, string) -> Value;
set_config_value(Value, integer) -> list_to_integer(Value).

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
    case re:run(GitOutput, "On branch (\\S+)", [{capture, all_but_first, list}]) of
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

setup_firehose() ->
    logplex_firehose:enable().

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

elb_healthcheck() ->
    case config(nsync_loaded, false) of
        false ->
            unhealthy;
        true ->
            healthy
    end.
