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
    ok = error_logger:add_report_handler(logplex_logger),
    RedisOpts = boot_redis(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [RedisOpts]).

stop(_State) ->
    ok.

init([RedisOpts]) ->
    {ok, {{one_for_one, 5, 10}, [
        {logplex_grid, {logplex_grid, start_link, []}, permanent, 2000, worker, [logplex_grid]},
        {logplex_realtime, {logplex_realtime, start_link, []}, permanent, 2000, worker, [logplex_realtime]},
        {logplex_stats, {logplex_stats, start_link, []}, permanent, 2000, worker, [logplex_stats]},
        {logplex_channel, {logplex_channel, start_link, []}, permanent, 2000, worker, [logplex_channel]},
        {logplex_token, {logplex_token, start_link, []}, permanent, 2000, worker, [logplex_token]},
        {logplex_drain, {logplex_drain, start_link, []}, permanent, 2000, worker, [logplex_drain]},
        {logplex_session, {logplex_session, start_link, []}, permanent, 2000, worker, [logplex_session]},
        {logplex_api, {logplex_api, start_link, []}, permanent, 2000, worker, [logplex_api]},
        {logplex_tail, {logplex_tail, start_link, []}, permanent, 2000, worker, [logplex_tail]},
        {logplex_queue, {logplex_queue, start_link, []}, permanent, 2000, worker, [logplex_queue]},
        {logplex_drain_buffer, {logplex_drain_buffer, start_link, []}, permanent, 2000, worker, [logplex_drain_buffer]},
        {logplex_redis_buffer, {logplex_redis_buffer, start_link, []}, permanent, 2000, worker, [logplex_redis_buffer]},
        {logplex_worker_mgr, {logplex_worker_mgr, start_link, [RedisOpts]}, permanent, 2000, worker, [logplex_worker_mgr]},
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
            Opts = 
                case os:getenv("LOGPLEX_REDIS_URL") of
                    false -> [{ip, "127.0.0.1"}, {port, 6379}];
                    Url ->
                        case redis_uri:parse(Url) of
                            {redis, UserInfo, Host, Port, _Path, _Query} ->
                                Pass = 
                                    case UserInfo of
                                        "" -> undefined;
                                        Val -> list_to_binary(Val)
                                    end,
                                {ok, Ip} = inet:getaddr(Host, inet),
                                [{ip, Ip}, {port, Port}, {pass, Pass}];
                            _ ->
                                [{ip, "127.0.0.1"}, {port, 6379}]
                        end
                end,
            redis_sup:add_pool(redis_pool, Opts, 100),
            Opts;
        Err ->
            exit(Err)
    end.