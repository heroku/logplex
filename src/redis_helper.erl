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
    redo_block:cmd(config_block, [<<"SETEX">>, Key, SessionExpiry, Body]).

%% This seems to be used by nobody?
-spec lookup_session(uuid:binary_string_uuid()) -> {error, any()} |
                                                   binary().
lookup_session(UUID) when is_binary(UUID) ->
    case redo_block:cmd(config_block, [<<"GET">>, session_key(UUID)]) of
        {error, Err} -> {error, Err};
        Data when is_binary(Data) -> Data
    end.

%%====================================================================
%% CHANNEL
%%====================================================================
channel_index() ->
    case redo_block:cmd(config_block, [<<"INCR">>, <<"channel_index">>]) of
        {error, Err} -> {error, Err};
        ChannelId when is_integer(ChannelId) -> ChannelId
    end.

-spec create_channel(logplex_channel:id()) -> 'ok' | {'error', Reason::term()}.
create_channel(ChannelId) when is_integer(ChannelId) ->
    Key = iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]),
    Cmd = [<<"HMSET">>, Key, <<"name">>, ""],
    case redo_block:cmd(config_block, Cmd) of
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
    case redo_block:cmd(config_block, Cmd) of
        <<"OK">> ->
            ok;
        {error, Err} ->
            {error, Err}
    end.

delete_channel(ChannelId) when is_integer(ChannelId) ->
    case redo_block:cmd(config_block, [<<"DEL">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.

build_push_msg(ChannelId, Length, Msg, Expiry)
  when is_integer(ChannelId), is_integer(Length), is_binary(Msg),
       is_binary(Expiry) ->
    build_push_msg(ChannelId, integer_to_list(Length),
                   Msg, Expiry);
build_push_msg(ChannelId, Length, Msg, Expiry)
  when is_integer(ChannelId), is_list(Length), is_binary(Msg),
       is_binary(Expiry) ->
    build_push_msg(ChannelId, iolist_to_binary(Length),
                   Msg, Expiry);

build_push_msg(ChannelId, Length, Msg, Expiry)
  when is_integer(ChannelId), is_binary(Length),
       is_binary(Msg), is_binary(Expiry) ->
    Key = iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]),
    Cmds = [ [<<"LPUSH">>, Key, Msg]
            ,[<<"LTRIM">>, Key, <<"0">>, Length]
            ,[<<"EXPIRE">>, Key, Expiry] ],
    iolist_to_binary([ redis_proto:build(Cmd)
                       || Cmd <- Cmds ]).

%% seems unused
lookup_channel(ChannelId) when is_integer(ChannelId) ->
    case redo_block:cmd(config_block, [<<"HGETALL">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            logplex_channel:new(ChannelId);
        _ ->
            undefined
    end.

%%====================================================================
%% TOKEN
%%====================================================================
-spec create_token(logplex_channel:id(), logplex_token:id(),
                   logplex_token:name()) -> any().
create_token(ChannelId, TokenId, TokenName)
  when is_integer(ChannelId), is_binary(TokenId), is_binary(TokenName) ->
    Res = redo_block:cmd(config_block, [<<"HMSET">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>]), <<"ch">>, integer_to_list(ChannelId), <<"name">>, TokenName]),
    case Res of
        <<"OK">> -> ok;
        Err -> Err
    end.

delete_token(TokenId) when is_binary(TokenId) ->
    case redo_block:cmd(config_block, [<<"DEL">>, iolist_to_binary([<<"tok:">>, TokenId, <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.

%%====================================================================
%% DRAIN
%%====================================================================
drain_index() ->
    case redo_block:cmd(config_block, [<<"INCR">>, <<"drain_index">>]) of
        {error, Err} -> {error, Err};
        DrainId when is_integer(DrainId) -> DrainId
    end.

reserve_drain(DrainId, Token, ChannelId)
  when is_integer(DrainId),
       is_binary(Token),
       is_integer(ChannelId) ->
    Key = drain_redis_key(DrainId),
    Res = redo_block:cmd(config_block, [<<"HMSET">>, Key,
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
    Res = redo_block:cmd(config_block, [<<"HMSET">>, Key,
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
    case redo_block:cmd(config_block, [<<"DEL">>, iolist_to_binary([<<"drain:">>, integer_to_list(DrainId), <<":data">>])]) of
        1 -> ok;
        Err -> Err
    end.

%%====================================================================
%% GRID
%%====================================================================
%% appears unused
set_node_ex(Node, Ip, Domain) when is_binary(Node), is_binary(Ip), is_binary(Domain) ->
    redo_block:cmd(config_block, [<<"SETEX">>, iolist_to_binary([<<"node:">>, Domain, <<":">>, Node]), <<"60">>, Ip]).

%% appears unused
register_with_face(Domain, Ip) ->
    redo_block:cmd(config_block, [<<"SETEX">>, iolist_to_binary([Domain, <<":alive:">>, Ip]), <<"180">>, ""]).

%% appears unused
set_weight(Domain, Ip, Weight) when is_integer(Weight) ->
    redo_block:cmd(config_block, [<<"SETEX">>, iolist_to_binary([Domain, <<":weight:">>, Ip]), <<"604800">>, integer_to_list(Weight)]).

%% appears unused
get_nodes(Domain) when is_binary(Domain) ->
    redo_block:cmd(config_block, [<<"KEYS">>, iolist_to_binary([<<"node:">>, Domain, <<":*">>])]).

%% appears unused
get_node(Node) when is_binary(Node) ->
    redo_block:cmd(config_block, [<<"GET">>, Node]).

%%====================================================================
%% HEALTHCHECK
%%====================================================================
healthcheck() ->
    case redo_block:cmd(config_block, [<<"INCR">>, <<"healthcheck">>]) of
        Count when is_integer(Count) -> Count;
        Error -> exit(Error)
    end.

%%====================================================================
%% STATS
%%====================================================================
%% appears unused
publish_stats(InstanceName, Json) when is_list(InstanceName), is_binary(Json) ->
    redo_block:cmd(config_block, [<<"PUBLISH">>, iolist_to_binary([<<"stats.">>, InstanceName]), Json]).

%% appears unused
register_stat_instance() ->
    InstanceName = logplex_app:config(instance_name),
    Domain = logplex_app:config(cloud_name),
    redo_block:cmd(config_block, [<<"SETEX">>, iolist_to_binary([Domain, <<":stats:logplex:">>, InstanceName]), <<"60">>, <<"1">>]).

%%====================================================================
%% QUARANTINES
%%====================================================================

%% appears unused
quarantine_channel(ChannelId) when is_integer(ChannelId) ->
    redo_block:cmd(config_block, [<<"SADD">>, <<"quarantine:channels">>,
                                  integer_to_list(ChannelId)]).

%% appears unused
unquarantine_channel(ChannelId) when is_integer(ChannelId) ->
    redo_block:cmd(config_block, [<<"SREM">>, <<"quarantine:channels">>,
                                  integer_to_list(ChannelId)]).

%%====================================================================
%% CREDS
%%====================================================================

-spec store_cred(ID::binary(),
                 Pass::binary(),
                 Perms::[{binary(), binary()}],
                 Name::binary()) ->
                        'ok' | {'error', Reason::term()}.
store_cred(Id, Pass, Perms, Name)
  when is_binary(Id),
       is_binary(Pass),
       is_list(Perms),
       is_binary(Name) ->
    Key = iolist_to_binary([<<"cred:">>, Id]),
    Cmd = [<<"HMSET">>, Key,
           <<"pass">>, Pass,
           <<"name">>, Name
           | lists:append([ [Perm, Value] || {Perm, Value} <- Perms]) ],
    case redo_block:cmd(config_block, Cmd) of
        <<"OK">> ->
            ok;
        {error, Err} ->
            {error, Err}
    end.

delete_cred(Id) when is_binary(Id) ->
    Key = iolist_to_binary([<<"cred:">>, Id]),
    case redo_block:cmd(config_block, [<<"DEL">>, Key ]) of
        1 -> ok;
        Err -> Err
    end.
