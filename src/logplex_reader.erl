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
-module(logplex_reader).
-export([start_link/2, init/3, loop/2]).

-include_lib("logplex.hrl").

%% API functions
start_link(QueuePid, RedisOpts) ->
    proc_lib:start_link(?MODULE, init, [self(), QueuePid, RedisOpts], 5000).

init(Parent, QueuePid, RedisOpts) ->
    io:format("init ~p~n", [?MODULE]),
    logplex_queue:register(QueuePid, self()),
    Socket = open_socket(RedisOpts),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(QueuePid, Socket).

loop(QueuePid, Socket) ->
    %% verify that reader still has an open
    %% connection to redis server
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, _} -> ok;
        {error, timeout} -> ok;
        {error, closed} -> exit(normal)
    end,
    case logplex_queue:out(QueuePid) of
        timeout -> ok;
        Out ->
            {1, [{ClientPid, Packet}]} = Out,
            case is_process_alive(ClientPid) of
                false -> ok;
                true ->
                    case redis:send_recv(Socket, 30000, Packet) of
                        Logs when is_list(Logs) ->
                            ClientPid ! {logs, Logs};
                        Err ->
                            exit(Err)
                    end
            end
    end,
    receive stop -> exit(normal) after 0 -> ok end,
    ?MODULE:loop(QueuePid, Socket).

open_socket(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Pass = proplists:get_value(pass, Opts),
    case redis:connect(Ip, Port, undefined) of
        {ok, Socket} ->
            gen_tcp:send(Socket, [<<"AUTH ">>, Pass, <<"\r\n">>]),
            case gen_tcp:recv(Socket, 0, 5000) of
                {ok,<<"+OK\r\n">>} -> ok;
                _ -> exit({error, auth_failed})
            end,
            inet:setopts(Socket, [{nodelay, true}]),
            Socket;
        Err ->
            exit(Err)
    end.
