-module(logplex_grid).
-export([start_link/0, init/1, publish/2, loop/0]).

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    loop().

publish(RegName, Msg) when is_atom(RegName), is_tuple(Msg) ->
    [erlang:send({RegName, Node}, Msg) || Node <- [node()|nodes()]],
    ok.

loop() ->
    redis_helper:set_node_ex(atom_to_binary(node(), utf8), local_ip()),
    [connect(Key) || {ok, Key} <- redis_helper:get_nodes()],
    receive
        {nodedown, _Node} ->
            ok
    after 0 ->
        ok
    end,
    timer:sleep(5 * 1000),
    ?MODULE:loop().

local_ip() ->
    case os:getenv("LOCAL_IP") of
        false -> <<"127.0.0.1">>;
        LocalIp -> list_to_binary(LocalIp)
    end.

connect(<<"node:", BinNode/binary>> = Key) ->
    StrNode = binary_to_list(BinNode),
    Node = list_to_atom(StrNode),
    case node() == Node of
        true ->
            undefined;
        false ->
            case net_adm:ping(Node) of
                pong -> ok;
                pang ->
                    case redis_helper:get_node(Key) of
                        {ok, Ip} ->
                            {ok, Addr} = inet:getaddr(binary_to_list(Ip), inet),
                            case re:run(StrNode, ".*@(.*)$", [{capture, all_but_first, list}]) of
                                {match, [Host]} ->
                                    io:format("add host ~p -> ~p~n", [Host, Addr]),
                                    inet_db:add_host(Addr, [Host]),
                                    case net_adm:ping(Node) of
                                        pong ->
                                            erlang:monitor_node(Node, true);
                                        pang ->
                                            {error, {ping_failed, Node}}
                                    end;
                                _ ->
                                    {error, {failed_to_parse_host, StrNode}}
                            end;
                        _ ->
                            undefined
                    end
            end
    end.
