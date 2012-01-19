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
-export([start_link/2, init/3, loop/3]).

-include_lib("logplex.hrl").

%% API functions
start_link(BufferPid, RedisOpts) ->
    io:format("logplex_redis_writer start_link ~p~n", [RedisOpts]),
    proc_lib:start_link(?MODULE, init, [self(), BufferPid, RedisOpts], 5000).

init(Parent, BufferPid, RedisOpts) ->
    io:format("init ~p~n", [?MODULE]),
    logplex_queue:register(BufferPid, self()),
    case open_socket(RedisOpts) of
        {error, Err} ->
            timer:sleep(500),
            exit({error, Err});
        {ok, Socket} when is_port(Socket) ->
            proc_lib:init_ack(Parent, {ok, self()}),
            loop(BufferPid, Socket, RedisOpts)
    end.

loop(BufferPid, Socket, RedisOpts) ->
    %% verify that writer still has an open
    %% connection to redis server
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, _} -> ok;
        {error, timeout} -> ok;
        {error, closed} ->
            error_logger:error_msg("~p event=recv result=closed", [?MODULE]),
            exit({error, closed})
    end,
    case catch logplex_queue:out(BufferPid, 100) of
        {'EXIT', {noproc, _}} ->
            exit(normal);
        timeout -> ok;
        {NumItems, Logs} ->
            case gen_tcp:send(Socket, Logs) of
                ok ->
                    logplex_stats:incr(message_processed, NumItems),
                    logplex_realtime:incr(message_processed, NumItems);
                {error, closed} ->
                    error_logger:error_msg("~p event=send result=closed", [?MODULE]),
                    timer:sleep(500),
                    exit(normal);
                Err ->
                    error_logger:error_msg("~p event=send result=~p", [?MODULE, Err]),
                    exit(normal)
            end
    end,
    receive stop -> exit(normal) after 0 -> ok end,
    ?MODULE:loop(BufferPid, Socket, RedisOpts).

open_socket(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Pass = proplists:get_value(pass, Opts),
    redis:connect(Ip, Port, Pass).
