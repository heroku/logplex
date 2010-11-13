-module(logplex_channel).
-export([create/1, delete/1, push/2, logs/2, tokens/1, drains/1, info/1]).

create(ChannelName) when is_binary(ChannelName) ->
    case redis:q([<<"INCR">>, <<"channel_index">>]) of
        {ok, ChannelId} ->
            redis:q([<<"SET">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId)]), ChannelName]),
            ChannelId;
        Error -> Error
    end.

delete(ChannelId) when is_binary(ChannelId) ->
    redis:q([<<"DEL">>, iolist_to_binary([<<"ch:">>, ChannelId])]).

push(ChannelId, Msg) when is_binary(ChannelId), is_binary(Msg) ->
    io:format("LPUSH ~p~n", [iolist_to_binary(["ch:", ChannelId, ":spool"])]),
    redis:q([<<"LPUSH">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), Msg]).

logs(ChannelId, Num) when is_binary(ChannelId), is_integer(Num) ->
    io:format("logs ~p ~p~n", [ChannelId, Num]),
    redis:q([<<"LRANGE">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

tokens(ChannelId) when is_binary(ChannelId) ->
    [Token || {ok, Token} <- redis:q([<<"SMEMBERS">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":tokens">>])])].

drains(ChannelId) when is_binary(ChannelId) ->
    [logplex_drain:lookup(DrainId) || {ok, DrainId} <- redis:q([<<"SMEMBERS">>, iolist_to_binary([<<"channel:">>, ChannelId, <<":drains">>])])].

info(ChannelId) when is_binary(ChannelId) ->
    {ok, ChannelName} = redis:q([<<"GET">>, iolist_to_binary([<<"ch:">>, ChannelId])]),
    Tokens = tokens(ChannelId),
    [{channel_id, ChannelId},
     {channel_name, ChannelName},
     {tokens, Tokens}].
    
