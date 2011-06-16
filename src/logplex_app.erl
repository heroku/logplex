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
    boot_pagerduty(),
    boot_redis(),
    setup_redgrid_vals(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    io:format("stopping...~n"),
    logplex_db:dump(),
    ok.

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {logplex_db, {logplex_db, start_link, []}, permanent, 2000, worker, [logplex_db]},
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

        {logplex_shard, {logplex_shard, start_link, []}, permanent, 2000, worker, [logplex_shard]},

        {logplex_work_queue, {logplex_queue, start_link, [logplex_work_queue, logplex_work_queue_args()]}, permanent, 2000, worker, [logplex_work_queue]},
        {logplex_drain_buffer, {logplex_queue, start_link, [logplex_drain_buffer, logplex_drain_buffer_args()]}, permanent, 2000, worker, [logplex_drain_buffer]},

        {logplex_api, {logplex_api, start_link, []}, permanent, 2000, worker, [logplex_api]},
        {udp_acceptor, {udp_acceptor, start_link, []}, permanent, 2000, worker, [udp_acceptor]}
    ]}}.

set_cookie() ->
    case os:getenv("LOGPLEX_COOKIE") of
        false -> ok;
        Cookie -> erlang:set_cookie(node(), list_to_atom(Cookie))
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

boot_redis() ->
    application:start(redis, temporary).
    
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
     {worker_args, []},
     {dict, dict:from_list([{producer_callback, fun(Self, Atom) -> whereis(udp_acceptor) ! {Self, Atom} end}])}].

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
    [{name, "logplex_drain_buffer"},
     {max_length, MaxLength},
     {num_workers, NumWorkers},
     {worker_sup, logplex_drain_sup},
     {worker_args, []}].
