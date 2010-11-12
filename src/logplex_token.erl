-module(logplex_token).
-export([create/2, lookup/1]).

create(ChannelId, TokenName) when is_list(ChannelId), is_binary(TokenName) ->
    Token = "t." ++ string:strip(os:cmd("uuidgen"), right, $\n),
    redis:q([<<"HMSET">>, Token, <<"channel_id">>, ChannelId, <<"name">>, TokenName]),
    Token.

lookup(Token) when is_list(Token) ->
    case redis:q([<<"HGETALL">>, Token]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            [{channel_id, field_val(<<"channel_id">>, Fields)},
             {token_name, field_val(<<"name">>, Fields)}];
        _ ->
            []
    end.

field_val(Key, Fields) ->
    field_val(Key, Fields, undefined).

field_val(Key, [{ok, Key}, {ok, Val} | _Tail], _Default) ->
    Val;

field_val(Key, [_, _ | Tail], Default) ->
    field_val(Key, Tail, Default);

field_val(_Key, _, Default) ->
    Default.
