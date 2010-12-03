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
-export([start_link/1, init/2, loop/1]).

-include_lib("logplex.hrl").

%% API functions
start_link(RedisOpts) ->
    proc_lib:start_link(?MODULE, init, [self(), RedisOpts], 5000).

init(Parent, RedisOpts) ->
    io:format("init ~p~n", [?MODULE]),
    Socket = open_socket(RedisOpts),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Socket).

loop(Socket) ->
    case logplex_redis_buffer:out(100) of
        undefined -> timer:sleep(10);
        {NumItems, Logs} ->
            case gen_tcp:send(Socket, Logs) of
                ok -> logplex_stats:incr(message_processed, NumItems);
                Err -> exit(Err)
            end
    end,
    loop(Socket).

open_socket(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Pass = proplists:get_value(pass, Opts),
    case redis:connect(Ip, Port, Pass) of
        {ok, Socket} -> Socket;
        Err -> exit(Err)
    end.