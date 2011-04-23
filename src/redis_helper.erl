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
%% Change SETEX command unhandled by nsync to SET 
create_session(Session, Body) when is_binary(Session), is_binary(Body) ->
    redis_pool:q(config_pool, [<<"SET">>, Session, Body]).

delete_session(Session) when is_binary(Session) ->
    case redis_pool:q(config_pool, [<<"DEL">>, Session]) of
        1 -> ok;
        Err -> Err
    end.

lookup_session(Session) when is_binary(Session) ->
    case redis_pool:q(config_pool, [<<"GET">>, Session]) of
        {error, Err} -> {error, Err};
        Data when is_binary(Data) -> Data
    end.

%%====================================================================
%% CHANNEL
%%====================================================================
channel_index() ->
    case redis_pool:q(config_pool, [<<"INCR">>, <<"channel_index">>]) of
        {error, Err} -> {error, Err};
        ChannelId when is_integer(ChannelId) -> ChannelId
    end.

create_channel(ChannelName, AppId, Addon) when is_binary(ChannelName), is_integer(AppId), is_binary(Addon) ->
    ChannelId = channel_index(),
    case redis_pool:q(config_pool, [<<"HMSET">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]),
            <<"name">>, ChannelName,
            <<"app_id">>, integer_to_list(AppId),
            <<"addon">>, Addon]) of
        {error, Err} -> {error, Err};
        <<"OK">> -> ChannelId
    end.

delete_channel(ChannelId) when is_integer(ChannelId) ->
    case redis_pool:q(config_pool, [<<"DEL">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.

update_channel_addon(ChannelId, Addon) when is_integer(ChannelId), is_binary(Addon) ->
    case redis_pool:q(config_pool, [<<"HSET">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]), <<"addon">>, Addon]) of
        {error, Err} -> {error, Err};
        Int when is_integer(Int) -> ok
    end.

build_push_msg(ChannelId, Length, Msg) when is_integer(ChannelId), is_binary(Length), is_binary(Msg) ->
    Key = iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]),
    iolist_to_binary([
        redis_proto:build([<<"LPUSH">>, Key, Msg]),
        redis_proto:build([<<"LTRIM">>, Key, <<"0">>, Length])
    ]).

lookup_channels() ->
    lists:flatten(lists:foldl(
        fun (Key, Acc) when is_binary(Key) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["ch", ChannelId, "data"] ->
                    case lookup_channel(list_to_integer(ChannelId)) of
                        undefined -> Acc;
                        Channel -> [Channel|Acc]
                    end;
                _ ->
                    Acc
            end;
            (_, Acc) ->
                Acc
        end, [], redis_pool:q(config_pool, [<<"KEYS">>, <<"ch:*:data">>]))).
 
lookup_channel_ids() ->
    lists:flatten(lists:foldl(
        fun(Key, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["ch", ChannelId, "data"] ->
                    [list_to_integer(ChannelId)|Acc];
                _ -> Acc
            end
        end, [], redis_pool:q(config_pool, [<<"KEYS">>, <<"ch:*:data">>]))).

lookup_channel(ChannelId) when is_integer(ChannelId) ->
    case redis_pool:q(config_pool, [<<"HGETALL">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #channel{
                id = ChannelId,
                name = logplex_utils:field_val(<<"name">>, Fields),
                app_id = list_to_integer(binary_to_list(logplex_utils:field_val(<<"app_id">>))),
                addon = logplex_utils:field_val(<<"addon">>, Fields)
            };
        _ ->
            undefined
    end.

%%====================================================================
%% TOKEN
%%====================================================================
create_token(ChannelId, TokenId, TokenName, AppId, Addon) 
  when is_integer(ChannelId), is_binary(TokenId), is_binary(TokenName), is_integer(AppId), is_binary(Addon) ->
    Res = redis_pool:q(config_pool, [<<"HMSET">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>]), 
                                     <<"ch">>, integer_to_list(ChannelId), 
                                     <<"name">>, TokenName, 
                                     <<"app_id">>, integer_to_list(AppId), 
                                     <<"addon">>, Addon]),
    case Res of
        <<"OK">> -> ok;
        Err -> Err
    end.

delete_token(TokenId) when is_binary(TokenId) ->
    case redis_pool:q(config_pool, [<<"DEL">>, TokenId]) of
        1 -> ok;
        Err -> Err
    end.

lookup_token(TokenId) when is_binary(TokenId) ->
    case redis_pool:q(config_pool, [<<"HGETALL">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #token{id = TokenId,
                   channel_id = list_to_integer(binary_to_list(logplex_utils:field_val(<<"ch">>, Fields))),
                   name = logplex_utils:field_val(<<"name">>, Fields),
		   app_id = list_to_integer(binary_to_list(logplex_utils:field_val(<<"app_id">>, Fields))),
		   addon = logplex_utils:field_val(<<"addon">>, Fields)
            };
        _ ->
            undefined
    end.

lookup_tokens() ->
    lists:flatten(lists:foldl(
        fun(Key, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["tok", TokenId, "data"] ->
                    case lookup_token(list_to_binary(TokenId)) of
                        undefined -> Acc;
                        Token -> [Token|Acc]
                    end;
                _ ->
                    Acc
            end
        end, [], redis_pool:q(config_pool, [<<"KEYS">>, <<"tok:*:data">>]))).

%%====================================================================
%% DRAIN
%%====================================================================
drain_index() ->
    case redis_pool:q(config_pool, [<<"INCR">>, <<"drain_index">>]) of
        {error, Err} -> {error, Err};
        DrainId when is_integer(DrainId) -> DrainId
    end.

create_drain(DrainId, ChannelId, Host, Port) when is_integer(DrainId), is_integer(ChannelId), is_binary(Host) ->
    Key = iolist_to_binary([<<"drain:">>, integer_to_list(DrainId), <<":data">>]),
    Res = redis_pool:q(config_pool, [<<"HMSET">>, Key,
        <<"ch">>, integer_to_list(ChannelId),
        <<"host">>, Host] ++
        [<<"port">> || is_integer(Port)] ++
        [integer_to_list(Port) || is_integer(Port)]),
    case Res of
        <<"OK">> -> ok;
        Err -> Err
    end.

delete_drain(DrainId) when is_integer(DrainId) ->
    case redis_pool:q(config_pool, [<<"DEL">>, iolist_to_binary([<<"drain:">>, integer_to_list(DrainId), <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.

lookup_drains() ->
    lists:foldl(
        fun(Key, Acc) ->
            case string:tokens(binary_to_list(Key), ":") of
                ["drain", DrainId, "data"] ->
                    [lookup_drain(list_to_integer(DrainId))|Acc];
                _ -> Acc
            end
        end, [], redis_pool:q(config_pool, [<<"KEYS">>, <<"drain:*:data">>])).

lookup_drain(DrainId) when is_integer(DrainId) ->
    case redis_pool:q(config_pool, [<<"HGETALL">>, iolist_to_binary([<<"drain:">>, integer_to_list(DrainId), <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            #drain{
                id = DrainId,
                channel_id = list_to_integer(binary_to_list(logplex_utils:field_val(<<"ch">>, Fields))),
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
    redis_pool:q(config_pool, [<<"SETEX">>, iolist_to_binary([<<"node:">>, Domain, <<":">>, Node]), <<"60">>, Ip]).

register_with_face(Domain, Ip) ->
    redis_pool:q(config_pool, [<<"SETEX">>, iolist_to_binary([Domain, <<":alive:">>, Ip]), <<"180">>, ""]).

set_weight(Domain, Ip, Weight) when is_integer(Weight) ->
    redis_pool:q(config_pool, [<<"SETEX">>, iolist_to_binary([Domain, <<":weight:">>, Ip]), <<"604800">>, integer_to_list(Weight)]).

get_nodes(Domain) when is_binary(Domain) ->
    redis_pool:q(config_pool, [<<"KEYS">>, iolist_to_binary([<<"node:">>, Domain, <<":*">>])]).

get_node(Node) when is_binary(Node) ->
    redis_pool:q(config_pool, [<<"GET">>, Node]).

shard_urls() ->
    redis_pool:q(config_pool, [<<"SMEMBERS">>, <<"redis:shard:urls">>], 30000).

%%====================================================================
%% HEALTHCHECK
%%====================================================================
healthcheck() ->
    case redis_pool:q(config_pool, [<<"INCR">>, <<"healthcheck">>]) of
        Count when is_integer(Count) -> Count;
        Error -> exit(Error)
    end.

%%====================================================================
%% STATS
%%====================================================================
publish_stats(InstanceName, Json) when is_list(InstanceName), is_binary(Json) ->
    redis_pool:q(config_pool, [<<"PUBLISH">>, iolist_to_binary([<<"stats.">>, InstanceName]), Json]).

register_stat_instance() ->
    InstanceName = logplex_utils:instance_name(),
    Domain = logplex_utils:heorku_domain(),
    redis_pool:q(config_pool, [<<"SETEX">>, iolist_to_binary([Domain, <<":stats:logplex:">>, InstanceName]), <<"60">>, <<"1">>]).
