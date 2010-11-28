-module(logplex_reader).
-export([start_link/0, init/1, loop/1]).

-include_lib("logplex.hrl").

-record(state, {socket, regexp, command}).

%% API functions
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    {ok, RE} = re:compile("^<\\d+>\\S+ \\S+ \\S+ (t[.]\\S+) "),
    RedisCommand = redis:build_request([<<"BRPOP">>, <<"logs">>, <<"0">>]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 6379, [binary, {active, false}, {keepalive, true}]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{socket=Socket, regexp=RE, command=RedisCommand}).

loop(#state{socket=Socket, command=RedisCommand, regexp=RE}=State) ->
    case gen_tcp:send(Socket, RedisCommand) of
        ok ->
            case gen_tcp:recv(Socket, 0) of
                {ok, <<"*2\r\n$4\r\nlogs\r\n$", Line/binary>>} ->
                    {Len, Rest} = parse_len(Line), 
                    case Rest of
                        <<Msg:Len/binary, "\r\n">> -> process(Msg, RE);
                        _ ->
                            case gen_tcp:recv(Socket, Len - size(Rest)) of
                                {ok, Rest1} -> process(<<Rest/binary, Rest1/binary>>, RE);
                                Err2 -> error_logger:error_msg("recv err ~p~n", [Err2])
                            end
                    end;
                Err1 -> error_logger:error_msg("recv err ~p~n", [Err1])
            end;
        Err -> error_logger:error_msg("send err ~p~n", [Err])
    end,
    loop(State).

parse_len(Line) ->
	parse_len(Line, []).

parse_len(<<"\r\n", Rest/binary>>, Acc) ->
	{list_to_integer(lists:flatten(lists:reverse(Acc))), Rest};

parse_len(<<I:8, Rest/binary>>, Acc) ->
	parse_len(Rest, [[I]|Acc]).

process(Msg, RE) when is_binary(Msg) ->
    case re:run(Msg, RE, [{capture, all_but_first, list}]) of
        {match, [Token]} ->
            case logplex_token:lookup(list_to_binary(Token)) of
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
                            process(ChannelId, Msg1, Addon);
                        false ->
                            Msg1 = re:replace(Msg, Token, TokenName),
                            Msg2 = iolist_to_binary(Msg1),
                            process(ChannelId, Msg2, Addon)
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    logplex_stats:incr(message_received).

process(ChannelId, Msg, Addon) ->
    logplex_stats:incr(message_processed),
    %logplex_tail:route(ChannelId, Msg),
    %[logplex_drain_pool:route(Host, Port, Msg) || #drain{host=Host, port=Port} <- logplex_channel:drains(ChannelId)],
    %redis_helper:build_push_msg(ChannelId, Msg).
    ok.

throughput(<<"basic">>) -> ?BASIC_THROUGHPUT;
throughput(<<"expanded">>) -> ?EXPANDED_THROUGHPUT.

exceeded_threshold(_, <<"advanced">>) -> false;
exceeded_threshold(Count, <<"expanded">>) when Count =< ?EXPANDED_THROUGHPUT -> false;
exceeded_threshold(Count, <<"expanded">>) when Count == (?EXPANDED_THROUGHPUT + 1) -> notify;
exceeded_threshold(Count, <<"basic">>) when Count =< ?BASIC_THROUGHPUT -> false;
exceeded_threshold(Count, <<"basic">>) when Count == (?BASIC_THROUGHPUT + 1) -> notify;
exceeded_threshold(_, _) -> true.

spool_length(<<"advanced">>) -> ?ADVANCED_LOG_HISTORY;
spool_length(_) -> ?DEFAULT_LOG_HISTORY.