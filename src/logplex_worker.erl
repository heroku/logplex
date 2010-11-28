-module(logplex_worker).
-export([start_link/1, init/2, loop/1]).

-include_lib("logplex.hrl").

-record(state, {socket, regexp}).

%% API functions
start_link(RedisOpts) ->
    proc_lib:start_link(?MODULE, init, [self(), RedisOpts], 5000).

init(Parent, RedisOpts) ->
    {ok, RE} = re:compile("^<\\d+>\\S+ \\S+ \\S+ (t[.]\\S+) "),
    Socket = open_socket(RedisOpts),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{socket=Socket, regexp=RE}).

loop(#state{socket=Socket, regexp=RE}=State) ->
    case logplex_queue:out() of
        undefined -> timer:sleep(10);
        Msg ->
            case re:run(Msg, RE, [{capture, all_but_first, list}]) of
                {match, [Token]} ->
                    route(Socket, list_to_binary(Token), Msg);
                _ ->
                    ok
            end
    end,
    loop(State).

open_socket(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Pass = proplists:get_value(pass, Opts),
    case redis:connect(Ip, Port, Pass) of
        {ok, Socket} -> Socket;
        Err -> exit(Err)
    end.

route(Socket, Token, Msg) when is_binary(Token), is_binary(Msg) ->
    case logplex_token:lookup(Token) of
        #token{channel_id=ChannelId, name=TokenName, addon=Addon} ->
            Count = logplex_stats:incr(ChannelId),
            case exceeded_threshold(Count, Addon) of
                true ->
                    ok;
                notify ->
                    {{Year,Month,Day},{Hour,Min,Sec}} = Local = erlang:localtime(),
                    UTC = erlang:universaltime(),
                    {_, {Offset, _, _}} = calendar:time_difference(Local, UTC),
                    Msg1 = iolist_to_binary(io_lib:format("<40>1 ~w-~w-~wT~w:~w:~w-0~w:00 - heroku logplex - - You have exceeded ~w logs/min. Please upgrade your logging addon for higher throughput.", [Year, Month, Day, Hour, Min, Sec, Offset, throughput(Addon)])),
                    process(Socket, ChannelId, Msg1);
                false ->
                    Msg1 = re:replace(Msg, Token, TokenName),
                    Msg2 = iolist_to_binary(Msg1),
                    process(Socket, ChannelId, Msg2)
            end;
        _ ->
            ok
    end.

process(Socket, ChannelId, Msg) ->
    logplex_tail:route(ChannelId, Msg),
    [logplex_drain_pool:route(Host, Port, Msg) || #drain{host=Host, port=Port} <- logplex_channel:drains(ChannelId)],
    case gen_tcp:send(Socket, redis_helper:build_push_msg(ChannelId, Msg)) of
        ok -> logplex_stats:incr(message_processed);
        Err -> exit(Err)
    end.

throughput(<<"basic">>) -> ?BASIC_THROUGHPUT;
throughput(<<"expanded">>) -> ?EXPANDED_THROUGHPUT.

exceeded_threshold(_, <<"advanced">>) -> false;
exceeded_threshold(Count, <<"expanded">>) when Count =< ?EXPANDED_THROUGHPUT -> false;
exceeded_threshold(Count, <<"expanded">>) when Count == (?EXPANDED_THROUGHPUT + 1) -> notify;
exceeded_threshold(Count, <<"basic">>) when Count =< ?BASIC_THROUGHPUT -> false;
exceeded_threshold(Count, <<"basic">>) when Count == (?BASIC_THROUGHPUT + 1) -> notify;
exceeded_threshold(_, _) -> true.