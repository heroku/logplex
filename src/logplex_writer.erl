-module(logplex_writer).
-export([start_link/1, init/2, loop/1]).

-include_lib("logplex.hrl").

%% API functions
start_link(RedisOpts) ->
    proc_lib:start_link(?MODULE, init, [self(), RedisOpts], 5000).

init(Parent, RedisOpts) ->
    Socket = open_socket(RedisOpts),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Socket).

loop(Socket) ->
    case logplex_buffer:out(100) of
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