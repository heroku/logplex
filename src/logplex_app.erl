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

%% Application callbacks
-export([start/2, stop/1, init/1]).

-include_lib("logplex.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    set_cookie(),
    read_git_branch(),
    read_availability_zone(),
    boot_pagerduty(),
    application:start(redis),
    setup_redgrid_vals(),
    application:start(nsync),
    application:start(cowboy),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    io:format("stopping...~n"),
    logplex_db:dump(),
    ok.

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {logplex_db, {logplex_db, start_link, []}, permanent, 2000, worker, [logplex_db]},
        {nsync, {nsync, start_link, [nsync_opts()]}, permanent, 2000, worker, [nsync]},
        {redgrid, {redgrid, start_link, []}, permanent, 2000, worker, [redgrid]},
        {logplex_realtime, {logplex_realtime, start_link, [logplex_utils:redis_opts("LOGPLEX_CONFIG_REDIS_URL")]}, permanent, 2000, worker, [logplex_realtime]},
        {logplex_stats, {logplex_stats, start_link, []}, permanent, 2000, worker, [logplex_stats]},

        {logplex_token, {logplex_token, refresh_dns, []}, permanent, 2000, worker, [logplex_token]},
        {logplex_tail, {logplex_tail, start_link, []}, permanent, 2000, worker, [logplex_tail]},

        {logplex_redis_writer_sup, {logplex_worker_sup, start_link, [logplex_redis_writer_sup, logplex_redis_writer]}, permanent, 2000, worker, [logplex_redis_writer_sup]},
        {logplex_redis_buffer_sup, {logplex_queue_sup, start_link, [logplex_redis_buffer_sup, logplex_redis_buffer]}, permanent, 2000, worker, [logplex_redis_buffer_sup]},
        {logplex_read_queue_sup, {logplex_queue_sup, start_link, [logplex_read_queue_sup, logplex_read_queue]}, permanent, 2000, worker, [logplex_read_queue_sup]},
        {logplex_reader_sup, {logplex_worker_sup, start_link, [logplex_reader_sup, logplex_reader]}, permanent, 2000, worker, [logplex_reader_sup]},
        {logplex_worker_sup, {logplex_worker_sup, start_link, [logplex_worker_sup, logplex_worker]}, permanent, 2000, worker, [logplex_worker_sup]},
        {logplex_drain_sup, {logplex_worker_sup, start_link, [logplex_drain_sup, logplex_drain_writer]}, permanent, 2000, worker, [logplex_drain_sup]},
        {logplex_drain_worker_sup, {logplex_worker_sup, start_link, [logplex_drain_worker_sup, logplex_drain_worker]}, permanent, 2000, worker, [logplex_drain_worker_sup]},

        {logplex_shard, {logplex_shard, start_link, []}, permanent, 2000, worker, [logplex_shard]},

        {logplex_work_queue, {logplex_queue, start_link, [logplex_work_queue, logplex_work_queue_args()]}, permanent, 2000, worker, [logplex_work_queue]},
        {logplex_drain_buffer, {logplex_queue, start_link, [logplex_drain_buffer, logplex_drain_buffer_args()]}, permanent, 2000, worker, [logplex_drain_buffer]},

        {tcp_proxy_sup, {tcp_proxy_sup, start_link, []}, permanent, 2000, worker, [tcp_proxy_sup]},

        {logplex_api, {logplex_api, start_link, []}, permanent, 2000, worker, [logplex_api]},
        {cowboy_listener_sup, {cowboy_listener_sup, start_link, http_handler:opts()}, permanent, 2000, supervisor, [cowboy_listener_sup]},
        {tcp_acceptor, {tcp_acceptor, start_link, [?TCP_PORT]}, permanent, 2000, worker, [tcp_acceptor]}]
    }}.

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

boot_pagerduty() ->
    case os:getenv("HEROKU_DOMAIN") of
        "heroku.com" ->
            case os:getenv("PAGERDUTY") of
                "0" -> ok;
                _ ->
                    ok = application:load(pagerduty),
                    application:set_env(pagerduty, service_key, os:getenv("ROUTING_PAGERDUTY_SERVICE_KEY")),
                    ok = application:start(pagerduty, temporary),
                    ok = error_logger:add_report_handler(logplex_report_handler)
            end;
        _ ->
            ok
    end.

setup_redgrid_vals() ->
    application:load(redgrid),
    application:set_env(redgrid, local_ip, os:getenv("LOCAL_IP")),
    application:set_env(redgrid, redis_url, os:getenv("LOGPLEX_CONFIG_REDIS_URL")),
    application:set_env(redgrid, domain, os:getenv("HEROKU_DOMAIN")),
    ok.

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

logplex_drain_buffer_args() ->
    MaxLength =
        case os:getenv("LOGPLEX_DRAIN_BUFFER_LENGTH") of
            false -> ?DEFAULT_LOGPLEX_DRAIN_BUFFER_LENGTH;
            StrNum1 -> list_to_integer(StrNum1)
        end,
    NumWorkers =
        case os:getenv("LOGPLEX_DRAIN_WRITERS") of
            false -> ?DEFAULT_LOGPLEX_DRAIN_WRITERS;
            StrNum2 -> list_to_integer(StrNum2)
        end,
    Dict = dict:from_list([
        {producer_callback, fun(_Pid, Action) ->
            [begin
                Pid ! {logplex_drain_buffer, Action}
            end ||{_,Pid,_,_} <- supervisor:which_children(logplex_worker_sup)]
        end}
    ]),
    [{name, "logplex_drain_buffer"},
     {max_length, MaxLength},
     {num_workers, NumWorkers},
     {worker_sup, logplex_drain_sup},
     {worker_args, []},
     {dict, Dict}].

nsync_opts() ->
    RedisOpts = logplex_utils:redis_opts("LOGPLEX_CONFIG_REDIS_URL"),
    Ip = case proplists:get_value(ip, RedisOpts) of
        {_,_,_,_}=L -> string:join([integer_to_list(I) || I <- tuple_to_list(L)], ".");
        Other -> Other
    end,
    RedisOpts1 = proplists:delete(ip, RedisOpts),
    RedisOpts2 = [{host, Ip} | RedisOpts1],
    [{callback, {nsync_callback, handle, []}} | RedisOpts2].
