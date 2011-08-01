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
    redo:cmd(config, [<<"SETEX">>, Session, <<"360">>, Body]).

lookup_session(Session) when is_binary(Session) ->
    case redo:cmd(config, [<<"GET">>, Session]) of
        {error, Err} -> {error, Err};
        Data when is_binary(Data) -> Data
    end.

%%====================================================================
%% CHANNEL
%%====================================================================
channel_index() ->
    case redo:cmd(config, [<<"INCR">>, <<"channel_index">>]) of
        {error, Err} -> {error, Err};
        ChannelId when is_integer(ChannelId) -> ChannelId
    end.

create_channel(ChannelId, ChannelName, AppId) when is_integer(ChannelId), is_binary(ChannelName), is_integer(AppId) ->
    Key = iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]),
    Cmd = [<<"HMSET">>, Key, <<"name">>, ChannelName, <<"app_id">>, integer_to_list(AppId)],
    case redo:cmd(config, Cmd) of
        <<"OK">> ->
            ok;
        {error, Err} ->
            {error, Err}
    end.

delete_channel(ChannelId) when is_integer(ChannelId) ->
    case redo:cmd(config, [<<"DEL">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.

build_push_msg(ChannelId, Length, Msg) when is_integer(ChannelId), is_binary(Length), is_binary(Msg) ->
    Key = iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]),
    iolist_to_binary([
        redis_proto:build([<<"LPUSH">>, Key, Msg]),
        redis_proto:build([<<"LTRIM">>, Key, <<"0">>, Length])
    ]).

lookup_channel(ChannelId) when is_integer(ChannelId) ->
    case redo:cmd(config, [<<"HGETALL">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>])]) of
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
create_token(ChannelId, TokenId, TokenName) when is_integer(ChannelId), is_binary(TokenId), is_binary(TokenName) ->
    Res = redo:cmd(config, [<<"HMSET">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>]), <<"ch">>, integer_to_list(ChannelId), <<"name">>, TokenName]),
    case Res of
        <<"OK">> -> ok;
        Err -> Err
    end.

delete_token(TokenId) when is_binary(TokenId) ->
    case redo:cmd(config, [<<"DEL">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.

%%====================================================================
%% DRAIN
%%====================================================================
drain_index() ->
    case redo:cmd(config, [<<"INCR">>, <<"drain_index">>]) of
        {error, Err} -> {error, Err};
        DrainId when is_integer(DrainId) -> DrainId
    end.

create_drain(DrainId, ChannelId, Token, Host, Port) when is_integer(DrainId), is_integer(ChannelId), is_binary(Token), is_binary(Host) ->
    Key = iolist_to_binary([<<"drain:">>, integer_to_list(DrainId), <<":data">>]),
    Res = redo:cmd(config, [<<"HMSET">>, Key,
        <<"ch">>, integer_to_list(ChannelId),
        <<"token">>, Token,
        <<"host">>, Host] ++
        [<<"port">> || is_integer(Port)] ++
        [integer_to_list(Port) || is_integer(Port)]),
    case Res of
        <<"OK">> -> ok;
        Err -> Err
    end.

delete_drain(DrainId) when is_integer(DrainId) ->
    case redo:cmd(config, [<<"DEL">>, iolist_to_binary([<<"drain:">>, integer_to_list(DrainId), <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.
    
%%====================================================================
%% GRID
%%====================================================================
set_node_ex(Node, Ip, Domain) when is_binary(Node), is_binary(Ip), is_binary(Domain) ->
    redo:cmd(config, [<<"SETEX">>, iolist_to_binary([<<"node:">>, Domain, <<":">>, Node]), <<"60">>, Ip]).

register_with_face(Domain, Ip) ->
    redo:cmd(config, [<<"SETEX">>, iolist_to_binary([Domain, <<":alive:">>, Ip]), <<"180">>, ""]).

set_weight(Domain, Ip, Weight) when is_integer(Weight) ->
    redo:cmd(config, [<<"SETEX">>, iolist_to_binary([Domain, <<":weight:">>, Ip]), <<"604800">>, integer_to_list(Weight)]).

get_nodes(Domain) when is_binary(Domain) ->
    redo:cmd(config, [<<"KEYS">>, iolist_to_binary([<<"node:">>, Domain, <<":*">>])]).

get_node(Node) when is_binary(Node) ->
    redo:cmd(config, [<<"GET">>, Node]).

shard_urls() ->
    redo:cmd(config, [<<"SMEMBERS">>, <<"redis:shard:urls">>], 30000).

%%====================================================================
%% HEALTHCHECK
%%====================================================================
healthcheck() ->
    case redo:cmd(config, [<<"INCR">>, <<"healthcheck">>]) of
        Count when is_integer(Count) -> Count;
        Error -> exit(Error)
    end.

%%====================================================================
%% STATS
%%====================================================================
publish_stats(InstanceName, Json) when is_list(InstanceName), is_binary(Json) ->
    redo:cmd(config, [<<"PUBLISH">>, iolist_to_binary([<<"stats.">>, InstanceName]), Json]).

register_stat_instance() ->
    InstanceName = logplex_utils:instance_name(),
    Domain = logplex_utils:heroku_domain(),
    redo:cmd(config, [<<"SETEX">>, iolist_to_binary([Domain, <<":stats:logplex:">>, InstanceName]), <<"60">>, <<"1">>]).

set_flag(ChannelId, Flag) ->
    case redo:cmd(config, [<<"SADD">>, iolist_to_binary([<<"flag:">>, integer_to_list(ChannelId), <<":data">>]), Flag]) of
        1 -> ok;
        _ -> undefined
    end.
