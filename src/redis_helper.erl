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

-spec session_key(uuid:binary_string_uuid()) -> binary().
session_key(UUID) when is_binary(UUID) ->
    iolist_to_binary([<<"session:">>, UUID]).

-spec create_session(uuid:binary_string_uuid(), Body::binary()) -> any().
create_session(UUID, Body) when is_binary(UUID), is_binary(Body) ->
    Key = session_key(UUID),
    SessionExpiry = logplex_session:expiry(),
    redo:cmd(config, [<<"SETEX">>, Key, SessionExpiry, Body]).

-spec lookup_session(uuid:binary_string_uuid()) -> {error, any()} |
                                                   binary().
lookup_session(UUID) when is_binary(UUID) ->
    case redo:cmd(config, [<<"GET">>, session_key(UUID)]) of
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

-spec create_channel(logplex_channel:id()) -> 'ok' | {'error', Reason::term()}.
create_channel(ChannelId) when is_integer(ChannelId) ->
    Key = iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]),
    Cmd = [<<"HMSET">>, Key, <<"name">>, ""],
    case redo:cmd(config, Cmd) of
        <<"OK">> ->
            ok;
        {error, Err} ->
            {error, Err}
    end.

-spec store_channel(logplex_channel:id(),
                     logplex_channel:name(),
                     binary()) -> 'ok' | {'error', Reason::term()}.
store_channel(ChannelId, Name, FlagStr)
  when is_integer(ChannelId),
       is_binary(Name),
       is_binary(FlagStr) ->
    Key = iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]),
    Cmd = [<<"HMSET">>, Key, <<"name">>, Name, <<"flags">>, FlagStr],
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

build_push_msg(ChannelId, Length, Msg, Expiry)
  when is_integer(ChannelId), is_binary(Length), is_binary(Msg),
       is_binary(Expiry) ->
    Key = iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]),
    Cmds = [ [<<"MULTI">>]
            ,[<<"LPUSH">>, Key, Msg]
            ,[<<"LTRIM">>, Key, <<"0">>, Length]
            ,[<<"EXPIRE">>, Key, Expiry]
            ,[<<"EXEC">>] ],
    iolist_to_binary([ redis_proto:build(Cmd)
                       || Cmd <- Cmds ]).

lookup_channel(ChannelId) when is_integer(ChannelId) ->
    case redo:cmd(config, [<<"HGETALL">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            logplex_channel:new(ChannelId);
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

reserve_drain(DrainId, Token, ChannelId)
  when is_integer(DrainId),
       is_binary(Token),
       is_integer(ChannelId) ->
    Key = drain_redis_key(DrainId),
    Res = redo:cmd(config, [<<"HMSET">>, Key,
                            <<"ch">>, integer_to_list(ChannelId),
                            <<"token">>, Token]),
    case Res of
        <<"OK">> -> ok;
        Err -> Err
    end.

create_url_drain(DrainId, ChannelId, Token, URL)
  when is_integer(DrainId), is_integer(ChannelId),
       is_binary(Token), is_binary(URL) ->
    Key = drain_redis_key(DrainId),
    Res = redo:cmd(config, [<<"HMSET">>, Key,
                            <<"ch">>, integer_to_list(ChannelId),
                            <<"token">>, Token,
                            <<"url">>, URL,
                            <<"state">>,<<"provisioned">>]),
    case Res of
        <<"OK">> -> ok;
        Err -> Err
    end.

drain_redis_key(DrainId) when is_integer(DrainId) ->
    iolist_to_binary([<<"drain:">>, integer_to_list(DrainId), <<":data">>]).

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

%%====================================================================
%% QUARANTINES
%%====================================================================

quarantine_channel(ChannelId) when is_integer(ChannelId) ->
    redo:cmd(config, [<<"SADD">>, <<"quarantine:channels">>,
                      integer_to_list(ChannelId)]).

unquarantine_channel(ChannelId) when is_integer(ChannelId) ->
    redo:cmd(config, [<<"SREM">>, <<"quarantine:channels">>,
                      integer_to_list(ChannelId)]).
