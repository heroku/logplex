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
    Socket = open_socket(RedisOpts),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(QueuePid, Socket).

loop(QueuePid, Socket) ->
    case logplex_queue:out(QueuePid) of
        timeout -> ok;
        Out ->
             {1, [{ClientPid, Packet}]} = Out,
            case is_process_alive(ClientPid) of
                false -> ok;
                true ->
                    case gen_tcp:send(Socket, Packet) of
                        ok ->
                            Logs = recv_logs(Socket),
                            ClientPid ! {logs, Logs};
                        Err ->
                            exit(Err)
                    end
            end
    end,
    ?MODULE:loop(QueuePid, Socket).

open_socket(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Pass = proplists:get_value(pass, Opts),
    case redis:connect(Ip, Port, Pass) of
        {ok, Socket} ->
            inet:setopts(Socket, [{active, false}, {nodelay, true}]),
            Socket;
        Err ->
            exit(Err)
    end.

recv_logs(Socket) ->
    Num = num_lines(Socket),
    recv_lines(Socket, Num).

num_lines(Socket) ->
    inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"*", Rest/binary>>} ->
            Size = size(Rest) - 2,
            case Rest of
                <<Int:Size/binary, "\r\n">> ->
                    list_to_integer(binary_to_list(Int));
                _ ->
                    exit({parse_failure, ?MODULE, ?LINE})
            end;
        _ ->
            exit({parse_failure, ?MODULE, ?LINE})
    end.

recv_lines(Socket, Num) ->
    recv_lines(Socket, Num, []).

recv_lines(_Socket, 0, Acc) -> Acc;

recv_lines(Socket, Num, Acc) ->
    inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0) of
        {ok, <<"$", Rest/binary>>} ->
            Size = size(Rest) - 2,
            case Rest of
                <<Int:Size/binary, "\r\n">> ->
                    Length = list_to_integer(binary_to_list(Int)),
                    inet:setopts(Socket, [{packet, raw}]),
                    case gen_tcp:recv(Socket, Length+2) of
                        {ok, <<Msg:Length/binary, "\r\n">>} ->
                            recv_lines(Socket, Num-1, [Msg|Acc]);
                        _ ->
                            exit({parse_failure, ?MODULE, ?LINE})
                    end;
                _ ->
                    exit({parse_failure, ?MODULE, ?LINE})
            end;
        _ ->
            exit({parse_failure, ?MODULE, ?LINE})
    end.