-module(redis_helper).
-compile(export_all).

-include_lib("logplex.hrl").

%%====================================================================
%% SESSION
%%====================================================================
create_session(Session, Body) when is_binary(Session), is_binary(Body) ->
    redis:q([<<"SETEX">>, Session, <<"360">>, Body]).

lookup_session(Session) when is_binary(Session) ->
    case redis:q([<<"GET">>, Session]) of
        {ok, Data} -> Data;
        Error ->
            exit(Error)
    end.

%%====================================================================
%% CHANNEL
%%====================================================================
channel_index() ->
    case redis:q([<<"INCR">>, <<"channel_index">>]) of
        {ok, ChannelId} ->
            ChannelId;
        Error ->
            exit(Error)
    end.

create_channel(ChannelName) when is_binary(ChannelName) ->
    ChannelId = channel_index(),
    redis:q([<<"SET">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId)]), ChannelName]),
    ChannelId.

delete_channel(ChannelId) when is_binary(ChannelId) ->
    redis:q([<<"DEL">>, iolist_to_binary([<<"ch:">>, ChannelId])]).

push_msg(Pool, ChannelId, Msg) when is_atom(Pool), is_binary(ChannelId), is_binary(Msg) ->
    redis:q(Pool, [<<"LPUSH">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), Msg]).

fetch_logs(ChannelId, Num) when is_binary(ChannelId), is_integer(Num) ->
    redis:q([<<"LRANGE">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

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

lookup_channel_name(ChannelId) when is_binary(ChannelId) ->
    case redis:q([<<"GET">>, iolist_to_binary([<<"ch:">>, ChannelId])]) of
        {ok, ChannelName} -> ChannelName;
        _ -> undefined
    end.

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

lookup_drain(DrainId) when is_binary(DrainId) ->
    case redis:q([<<"HGETALL">>, iolist_to_binary([<<"drain:">>, DrainId])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #drain{
                channel_id = logplex_utils:field_val(<<"channel_id">>, Fields),
                host = logplex_utils:field_val(<<"host">>, Fields),
                port =
                 case logplex_utils:field_val(<<"port">>, Fields) of
                     <<"">> -> undefined;
                     Val -> list_to_integer(binary_to_list(Val))
                 end
            };
        _ ->
            undefined
    end.
    
%%====================================================================
%% GRID
%%====================================================================
set_node_ex(Node, Ip) when is_binary(Node), is_binary(Ip) ->
    redis:q([<<"SETEX">>, iolist_to_binary([<<"node:">>, Node]), <<"15">>, Ip]).

get_nodes() ->
    redis:q([<<"KEYS">>, <<"node:*">>]).

get_node(Node) when is_binary(Node) ->
    redis:q([<<"GET">>, Node]).

%%====================================================================
%% HEALTHCHECK
%%====================================================================
healthcheck() ->
    case redis:q([<<"INCR">>, <<"healthcheck">>]) of
        {ok, Count} -> Count;
        Error -> exit(Error)
    end.