-module(logplex_token).
-export([create/2, lookup/1, delete/1]).

create(ChannelId, TokenName) when is_binary(ChannelId), is_binary(TokenName) ->
    Token = list_to_binary("t." ++ string:strip(os:cmd("uuidgen"), right, $\n)),
    redis:q([<<"HMSET">>, Token, <<"channel_id">>, ChannelId, <<"name">>, TokenName]),
    redis:q([<<"SADD">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":tokens">>]), Token]),
    Token.

lookup(Token) when is_binary(Token) ->
    case redis:q([<<"HGETALL">>, Token]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            [{channel_id, logplex_utils:field_val(<<"channel_id">>, Fields)},
             {token_name, logplex_utils:field_val(<<"name">>, Fields)}];
        _ ->
            []
    end.

delete(Token) when is_binary(Token) ->
    case lookup(Token) of
        [{channel_id, ChannelId},_] ->
            redis:q([<<"DEL">>, Token]),
            redis:q([<<"SREM">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":tokens">>]), Token]);
        _ ->
            ok
    end.
