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
    boot_redis(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
        {logplex_grid, {logplex_grid, start_link, []}, permanent, 2000, worker, [logplex_grid]},
        {logplex_rate_limit, {logplex_rate_limit, start_link, []}, permanent, 2000, worker, [logplex_rate_limit]},
        %{logplex_realtime, {logplex_realtime, start_link, []}, permanent, 2000, worker, [logplex_realtime]},
        {logplex_stats, {logplex_stats, start_link, []}, permanent, 2000, worker, [logplex_stats]},

        {logplex_channel, {logplex_channel, start_link, []}, permanent, 2000, worker, [logplex_channel]},
        {logplex_token, {logplex_token, start_link, []}, permanent, 2000, worker, [logplex_token]},
        {logplex_drain, {logplex_drain, start_link, []}, permanent, 2000, worker, [logplex_drain]},
        {logplex_session, {logplex_session, start_link, []}, permanent, 2000, worker, [logplex_session]},
        {logplex_tail, {logplex_tail, start_link, []}, permanent, 2000, worker, [logplex_tail]},

        {logplex_redis_writer_sup, {logplex_sup, start_link, [logplex_redis_writer_sup, logplex_redis_writer]}, permanent, 2000, worker, [logplex_redis_writer_sup]},
        {logplex_redis_buffer_sup, {logplex_sup, start_link, [logplex_redis_buffer_sup, logplex_redis_buffer]}, permanent, 2000, worker, [logplex_redis_buffer_sup]},
        {logplex_read_queue_sup, {logplex_sup, start_link, [logplex_read_queue_sup, logplex_read_queue]}, permanent, 2000, worker, [logplex_read_queue_sup]},
        {logplex_reader_sup, {logplex_sup, start_link, [logplex_reader_sup, logplex_reader]}, permanent, 2000, worker, [logplex_reader_sup]},
        {logplex_worker_sup, {logplex_sup, start_link, [logplex_worker_sup, logplex_worker]}, permanent, 2000, worker, [logplex_worker_sup]},
        {logplex_drain_sup, {logplex_sup, start_link, [logplex_drain_sup, logplex_drain_writer]}, permanent, 2000, worker, [logplex_drain_sup]},

        {logplex_shard, {logplex_shard, start_link, []}, permanent, 2000, worker, [logplex_shard]},

        {logplex_work_queue, {logplex_work_queue, start_link, []}, permanent, 2000, worker, [logplex_work_queue]},
        {logplex_drain_buffer, {logplex_drain_buffer, start_link, []}, permanent, 2000, worker, [logplex_drain_buffer]},
        %% logplex_redis_buffer supervised by logplex_redis_buffer_sup
        %% logplex_read_queue supervised by logplex_read_queue_sup

        {logplex_api, {logplex_api, start_link, []}, permanent, 2000, worker, [logplex_api]},
        {syslog_acceptor, {syslog_acceptor, start_link, []}, permanent, 2000, worker, [syslog_acceptor]}
    ]}}.

set_cookie() ->
    case os:getenv("LOGPLEX_COOKIE") of
        false -> ok;
        Cookie -> erlang:set_cookie(node(), list_to_atom(Cookie))
    end.

boot_redis() ->
    case application:start(redis, temporary) of
        ok ->
            Opts = redis_opts("LOGPLEX_CONFIG_REDIS_URL"),
            redis_sup:add_pool(config_pool, Opts, 25);
        Err ->
            exit(Err)
    end.

redis_opts(ConfigVar) when is_list(ConfigVar) ->
    case os:getenv(ConfigVar) of
        false ->
            [{ip, "127.0.0.1"}, {port, 6379}];
        Url ->
            logplex_utils:parse_redis_url(Url)
    end.