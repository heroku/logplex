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
-module(logplex_redis_writer).
-export([start_link/2, init/3, loop/2]).

-include_lib("logplex.hrl").

%% API functions
start_link(BufferPid, RedisOpts) ->
    io:format("logplex_redis_writer start_link ~p~n", [RedisOpts]),
    proc_lib:start_link(?MODULE, init, [self(), BufferPid, RedisOpts], 5000).

init(Parent, BufferPid, RedisOpts) ->
    io:format("init ~p~n", [?MODULE]),
    Socket = open_socket(RedisOpts),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(BufferPid, Socket).

loop(BufferPid, Socket) ->
    case logplex_queue:out(BufferPid, 100) of
        timeout -> ok;
        {NumItems, Logs} ->
            case gen_tcp:send(Socket, Logs) of
                ok ->
                    logplex_stats:incr(logplex_stats, message_processed, NumItems),
                    receive _X -> ok after 0 -> ok end,
                    inet:setopts(Socket, [{active, once}]);
                Err -> exit(Err)
            end
    end,
    ?MODULE:loop(BufferPid, Socket).

open_socket(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Pass = proplists:get_value(pass, Opts),
    case redis:connect(Ip, Port, Pass) of
        {ok, Socket} ->
            inet:setopts(Socket, [{active, once}, {nodelay, true}]),
            Socket;
        Err ->
            exit(Err)
    end.