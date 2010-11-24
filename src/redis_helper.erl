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
        Err -> Err
    end.

%%====================================================================
%% CHANNEL
%%====================================================================
channel_index() ->
    case redis:q([<<"INCR">>, <<"channel_index">>]) of
        {ok, ChannelId} -> ChannelId;
        Err -> Err
    end.

create_channel(ChannelName) when is_binary(ChannelName) ->
    ChannelId = channel_index(),
    case redis:q([<<"SET">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":name">>]), ChannelName]) of
        {ok, <<"OK">>} -> ChannelId;
        Err -> Err
    end.

delete_channel(ChannelId) when is_binary(ChannelId) ->
    case redis:q([<<"DEL">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":name">>])]) of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end.

push_msg(ChannelId, Msg, Length) when is_binary(ChannelId), is_binary(Msg), is_integer(Length) ->
    Part1 = redis:build_request([<<"LPUSH">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), Msg]),
    Part2 = redis:build_request([<<"LTRIM">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), <<"0">>, list_to_binary(integer_to_list(Length - 1))]),
    case redis:q(spool, iolist_to_binary([Part1, Part2])) of
        {ok, _} -> ok;
        Err -> Err
    end.

fetch_logs(ChannelId, Num) when is_binary(ChannelId), is_integer(Num) ->
    redis:q([<<"LRANGE">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

lookup_channel_ids() ->
    lists:flatten(lists:foldl(
        fun({ok, Key}, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["ch", ChannelId, "data"] ->
                    [list_to_binary(ChannelId)|Acc];
                _ -> Acc
            end
        end, [], redis:q([<<"KEYS">>, <<"ch:*:data">>]))).

lookup_channel_name(ChannelId) when is_binary(ChannelId) ->
    case redis:q([<<"GET">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":name">>])]) of
        {ok, ChannelName} -> ChannelName;
        _ -> undefined
    end.

%%====================================================================
%% TOKEN
%%====================================================================
create_token(ChannelId, TokenId, TokenName, Addon) when is_binary(ChannelId), is_binary(TokenId), is_binary(TokenName), is_binary(Addon) ->
    Res = redis:q([<<"HMSET">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>]), <<"ch">>, ChannelId, <<"name">>, TokenName, <<"addon">>, Addon]),
    case Res of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end.

delete_token(ChannelId, TokenId) when is_binary(ChannelId), is_binary(TokenId) ->
    case redis:q([<<"DEL">>, TokenId]) of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end.

lookup_token(TokenId) when is_binary(TokenId) ->
    case redis:q([<<"HGETALL">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #token{id = TokenId,
                   channel_id = logplex_utils:field_val(<<"ch">>, Fields),
                   name = logplex_utils:field_val(<<"name">>, Fields),
                   addon = logplex_utils:field_val(<<"addon">>, Fields)
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
        Err -> Err
    end.

create_drain(DrainId, ChannelId, Host, Port) when is_binary(DrainId), is_binary(ChannelId), is_binary(Host), is_integer(Port) ->
    Key = iolist_to_binary([<<"drain:">>, DrainId, <<":data">>]),
    Res = redis:q([<<"HMSET">>, Key,
        <<"ch">>, ChannelId,
        <<"host">>, Host] ++
        [<<"port">> || is_integer(Port)] ++
        [integer_to_list(Port) || is_integer(Port)]),
    case Res of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end.

delete_drain(DrainId, ChannelId) when is_binary(DrainId), is_binary(ChannelId) ->
    case redis:q([<<"DEL">>, iolist_to_binary([<<"drain:">>, DrainId, <<":data">>])]) of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end,
    ok.

lookup_drains() ->
    lists:foldl(
        fun({ok, Key}, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["drain", DrainId, "data"] ->
                    [lookup_drain(list_to_binary(DrainId))|Acc];
                _ -> Acc
            end
        end, [], redis:q([<<"KEYS">>, <<"drain:*:data">>])).

lookup_drain(DrainId) when is_binary(DrainId) ->
    case redis:q([<<"HGETALL">>, iolist_to_binary([<<"drain:">>, DrainId, <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #drain{
                id = DrainId,
                channel_id = logplex_utils:field_val(<<"ch">>, Fields),
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