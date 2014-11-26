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

-include("logplex.hrl").
-include("logplex_logging.hrl").

%% API functions
start_link(BufferPid, RedisOpts) ->
    ?INFO("at=start_link redis_opts=~p~n", [RedisOpts]),
    proc_lib:start_link(?MODULE, init, [self(), BufferPid, RedisOpts], 5000).

init(Parent, BufferPid, RedisOpts) ->
    ?INFO("at=init buffer_pid=~p parent=~p~n", [BufferPid, Parent]),
    logplex_queue:register(BufferPid, self()),
    proc_lib:init_ack(Parent, {ok, self()}),
    case open_socket(RedisOpts) of
        {error, Err} ->
            throttled_restart(Err);
        {ok, Socket} when is_port(Socket) ->
            loop(BufferPid, Socket, RedisOpts)
    end.

loop(BufferPid, Socket, RedisOpts) when is_port(Socket) ->
    verify_open_connection(Socket),
    write_queued_logs(BufferPid, Socket),
    check_for_stop_signal(),
    ?MODULE:loop(BufferPid, Socket, RedisOpts).

verify_open_connection(Socket) ->
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, _} -> ok;
        {error, timeout} -> ok;
        {error, closed} ->
            throttled_restart(closed)
    end.

write_queued_logs(BufferPid, Socket) ->
    case catch logplex_queue:out(BufferPid, 100) of
        {'EXIT', {noproc, _}} ->
            exit(normal);
        timeout -> ok;
        {NumItems, Logs} when is_list(Logs), is_integer(NumItems) ->
            case gen_tcp:send(Socket, Logs) of
                ok ->
                    logplex_stats:incr(message_processed, NumItems),
                    logplex_realtime:incr('message.processed', NumItems);
                {error, closed} ->
                    throttled_restart(closed);
                Err ->
                    ?INFO("event=send result=~p", [Err]),
                    exit({error, Err})
            end;
        Else ->
            ?WARN("event=queue_out result=~p", [Else]),
            exit(normal)
    end.

check_for_stop_signal() ->
    receive stop -> exit(normal) after 0 -> ok end.

throttled_restart(Reason) ->
    throttled_restart(Reason, 5000).

throttled_restart(Reason, Delay) ->
    ?INFO("event=send result=~p exit_in=~pms", [Reason, Delay]),
    timer:sleep(Delay),
    exit({error, closed}).

open_socket(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Pass = proplists:get_value(pass, Opts),
    redis:connect(Ip, Port, Pass).
