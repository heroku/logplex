-module(redis_helper).
-compile(export_all).

-include_lib("logplex.hrl").

%%====================================================================
%% CHANNEL
%%====================================================================
lookup_drains() ->
    lists:flatten(lists:foldl(
        fun({ok, Key}, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["channel", ChannelId, "drains"] ->
                    [lookup_drains(list_to_binary(ChannelId))|Acc];
                _ -> Acc
            end
        end, [], redis:q([<<"KEYS">>, <<"channel:*:drains">>]))).

lookup_drains(ChannelId) when is_binary(ChannelId) ->
    [logplex_drain:lookup(DrainId) || {ok, DrainId} <- redis:q([<<"SMEMBERS">>, iolist_to_binary([<<"channel:">>, ChannelId, <<":drains">>])])].

%%====================================================================
%% TOKEN
%%====================================================================
create_token(ChannelId, TokenId, TokenName) when is_binary(ChannelId), is_binary(TokenId), is_binary(TokenName) ->
    redis:q([<<"HMSET">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>]), <<"ch">>, ChannelId, <<"name">>, TokenName]),
    redis:q([<<"SADD">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":tokens">>]), TokenId]),
    ok.

delete_token(ChannelId, TokenId) when is_binary(ChannelId), is_binary(TokenId) ->
    redis:q([<<"DEL">>, TokenId]),
    redis:q([<<"SREM">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":tokens">>]), TokenId]),
    ok.

lookup_token(TokenId) when is_binary(TokenId) ->
    case redis:q([<<"HGETALL">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #token{id = TokenId,
                   channel_id = logplex_utils:field_val(<<"ch">>, Fields),
                   name = logplex_utils:field_val(<<"name">>, Fields)
            };
        _ ->
            undefined
    end.

lookup_tokens() ->
    lists:flatten(lists:foldl(
        fun({ok, Key}, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["tok", TokenId, "data"] ->
                    case lookup_token(list_to_binary(TokenId)) of
                        undefined -> Acc;
                        Token -> [Token|Acc]
                    end;
                _ ->
                    Acc
            end
        end, [], redis:q([<<"KEYS">>, <<"tok:*:data">>]))).

%%====================================================================
%% DRAIN
%%====================================================================
drain_index() ->
    case redis:q([<<"INCR">>, <<"drain_index">>]) of
        {ok, DrainId} -> DrainId;
        Error -> Error
    end.

create_drain(DrainId, ChannelId, Host, Port) when is_binary(DrainId), is_binary(ChannelId), is_binary(Host), is_integer(Port) ->
    redis:q([<<"HMSET">>, iolist_to_binary([<<"drain:">>, integer_to_list(DrainId)]),
        <<"channel_id">>, ChannelId,
        <<"host">>, Host] ++ lists:flatten([[<<"port">>, integer_to_list(Port)] || is_integer(Port)])),
    redis:q([<<"SADD">>, iolist_to_binary([<<"channel:">>, ChannelId, <<":drains">>]), integer_to_list(DrainId)]),
    ok.

delete_drain(DrainId, ChannelId) when is_binary(DrainId), is_binary(ChannelId) ->
    redis:q([<<"SREM">>, iolist_to_binary([<<"channel:">>, ChannelId, <<":drains">>])]),
    case redis:q([<<"DEL">>, iolist_to_binary([<<"drain:">>, DrainId])]) of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end,
    ok.