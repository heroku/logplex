-module(logplex_channel).
-export([create/1, push/2, logs/2]).

create(ChannelName) when is_list(ChannelName) ->
    case redis:q([<<"INCR">>, <<"channel_index">>]) of
        {ok, ChannelId} -> ChannelId;
        Error -> Error
    end.

push(ChannelId, Msg) when is_binary(ChannelId), is_binary(Msg) ->
    io:format("LPUSH ~p~n", [iolist_to_binary(["ch:", ChannelId, ":spool"])]),
    redis:q([<<"LPUSH">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), Msg]).

logs(ChannelId, Num) when is_list(ChannelId), is_integer(Num) ->
    io:format("logs ~p ~p~n", [ChannelId, Num]),
    redis:q([<<"LRANGE">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).