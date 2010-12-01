%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(redis_helper).
-compile(export_all).

-include_lib("logplex.hrl").

%%====================================================================
%% SESSION
%%====================================================================
create_session(Session, Body) when is_binary(Session), is_binary(Body) ->
    redis:q(config_pool, [<<"SETEX">>, Session, <<"360">>, Body]).

lookup_session(Session) when is_binary(Session) ->
    case redis:q(config_pool, [<<"GET">>, Session]) of
        {ok, Data} -> Data;
        Err -> Err
    end.

%%====================================================================
%% CHANNEL
%%====================================================================
channel_index() ->
    case redis:q(config_pool, [<<"INCR">>, <<"channel_index">>]) of
        {ok, ChannelId} -> ChannelId;
        Err -> Err
    end.

create_channel(ChannelName, AppId) when is_binary(ChannelName) ->
    ChannelId = channel_index(),
    case redis:q(config_pool, [<<"HMSET">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]),
            <<"name">>, ChannelName] ++
            [<<"app_id">> || is_integer(AppId)] ++
            [integer_to_list(AppId) || is_integer(AppId)]) of
        {ok, _} -> ChannelId;
        Err -> Err
    end.

delete_channel(ChannelId) when is_binary(ChannelId) ->
    case redis:q(config_pool, [<<"DEL">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":data">>])]) of
        {ok, 1} -> ok;
        Err -> Err
    end.

build_push_msg(ChannelId, Msg) when is_binary(ChannelId), is_binary(Msg) ->
    redis:build_request([<<"LPUSH">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), Msg]).

fetch_logs(ChannelId, Num) when is_binary(ChannelId), is_integer(Num) ->
    redis:q(log_pool, [<<"LRANGE">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

truncate_logs(Packet) when is_binary(Packet) ->
    redis:q(log_pool, iolist_to_binary(Packet), 10000).

lookup_channels() ->
    lists:flatten(lists:foldl(
        fun ({ok, Key}, Acc) when is_binary(Key) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["ch", ChannelId, "data"] ->
                    case lookup_channel(list_to_binary(ChannelId)) of
                        undefined -> Acc;
                        Channel -> [Channel|Acc]
                    end;
                _ ->
                    Acc
            end;
            (_, Acc) ->
                Acc
        end, [], redis:q(config_pool, [<<"KEYS">>, <<"ch:*:data">>]))).
 
lookup_channel_ids() ->
    lists:flatten(lists:foldl(
        fun({ok, Key}, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["ch", ChannelId, "data"] ->
                    [list_to_binary(ChannelId)|Acc];
                _ -> Acc
            end
        end, [], redis:q(config_pool, [<<"KEYS">>, <<"ch:*:data">>]))).

lookup_channel(ChannelId) when is_binary(ChannelId) ->
    case redis:q(config_pool, [<<"HGETALL">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #channel{
                id = ChannelId,
                name = logplex_utils:field_val(<<"name">>, Fields),
                app_id =
                 case logplex_utils:field_val(<<"app_id">>, Fields) of
                     Val when is_binary(Val), size(Val) > 0 ->
                         list_to_integer(binary_to_list(Val));
                     _ -> undefined
                 end
            };
        _ ->
            undefined
    end.

%%====================================================================
%% TOKEN
%%====================================================================
create_token(ChannelId, TokenId, TokenName, Addon) when is_binary(ChannelId), is_binary(TokenId), is_binary(TokenName), is_binary(Addon) ->
    Res = redis:q(config_pool, [<<"HMSET">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>]), <<"ch">>, ChannelId, <<"name">>, TokenName, <<"addon">>, Addon]),
    case Res of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end.

delete_token(ChannelId, TokenId) when is_binary(ChannelId), is_binary(TokenId) ->
    case redis:q(config_pool, [<<"DEL">>, TokenId]) of
        {ok, 1} -> ok;
        Err -> Err
    end.

lookup_token(TokenId) when is_binary(TokenId) ->
    case redis:q(config_pool, [<<"HGETALL">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>])]) of
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
        end, [], redis:q(config_pool, [<<"KEYS">>, <<"tok:*:data">>]))).

%%====================================================================
%% DRAIN
%%====================================================================
drain_index() ->
    case redis:q(config_pool, [<<"INCR">>, <<"drain_index">>]) of
        {ok, DrainId} -> DrainId;
        Err -> Err
    end.

create_drain(DrainId, ChannelId, Host, Port) when is_binary(DrainId), is_binary(ChannelId), is_binary(Host) ->
    Key = iolist_to_binary([<<"drain:">>, DrainId, <<":data">>]),
    Res = redis:q(config_pool, [<<"HMSET">>, Key,
        <<"ch">>, ChannelId,
        <<"host">>, Host] ++
        [<<"port">> || is_integer(Port)] ++
        [integer_to_list(Port) || is_integer(Port)]),
    case Res of
        {ok, <<"OK">>} -> ok;
        Err -> Err
    end.

delete_drain(DrainId) when is_binary(DrainId) ->
    case redis:q(config_pool, [<<"DEL">>, iolist_to_binary([<<"drain:">>, DrainId, <<":data">>])]) of
        {ok, 1} -> ok;
        Err -> Err
    end.

lookup_drains() ->
    lists:foldl(
        fun({ok, Key}, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["drain", DrainId, "data"] ->
                    [lookup_drain(list_to_binary(DrainId))|Acc];
                _ -> Acc
            end
        end, [], redis:q(config_pool, [<<"KEYS">>, <<"drain:*:data">>])).

lookup_drain(DrainId) when is_binary(DrainId) ->
    case redis:q(config_pool, [<<"HGETALL">>, iolist_to_binary([<<"drain:">>, DrainId, <<":data">>])]) of
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
set_node_ex(Node, Ip, Domain) when is_binary(Node), is_binary(Ip), is_binary(Domain) ->
    redis:q(config_pool, [<<"SETEX">>, iolist_to_binary([<<"node:">>, Domain, <<":">>, Node]), <<"60">>, Ip]).

get_nodes(Domain) when is_binary(Domain) ->
    redis:q(config_pool, [<<"KEYS">>, iolist_to_binary([<<"node:">>, Domain, <<":*">>])]).

get_node(Node) when is_binary(Node) ->
    redis:q(config_pool, [<<"GET">>, Node]).

%%====================================================================
%% HEALTHCHECK
%%====================================================================
healthcheck() ->
    case redis:q(config_pool, [<<"INCR">>, <<"healthcheck">>]) of
        {ok, Count} -> Count;
        Error -> exit(Error)
    end.

%%====================================================================
%% STATS
%%====================================================================
publish_stats(InstanceName, Json) when is_list(InstanceName), is_binary(Json) ->
    redis:q(config_pool, [<<"PUBLISH">>, iolist_to_binary([<<"stats.">>, InstanceName]), Json]).