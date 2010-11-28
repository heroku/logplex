-module(logplex_reader).
-export([start_link/0, init/1, loop/1]).

-include_lib("logplex.hrl").

-record(state, {socket, regexp, command}).

%% API functions
start_link() ->
    proc_lib:spawn_link(?MODULE, init, [self()]).

init(Parent) ->
    {ok, RE} = re:compile("^<\\d+>\\S+ \\S+ \\S+ (t[.]\\S+) "),
    RedisCommand = redis:build_request([<<"BRPOP">>, <<"logs">>, <<"0">>]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 6379, [binary, {active, false}, {keepalive, true}]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{socket=Socket, regexp=RE, command=RedisCommand}).

loop(#state{socket=Socket, command=RedisCommand}=State) ->
    case gen_tcp:send(Socket, RedisCommand) of
        ok ->
            inet:setopts(Socket, [{packet, line}]),
            case gen_tcp:recv(Socket, 0) of
                {ok, Line} -> io:format("line ~p~n", [Line]);
                Err1 -> io:format("recv err ~p~n", [Err1])
            end;
        Err -> io:format("send err ~p~n", [Err])
    end,
    loop(State).