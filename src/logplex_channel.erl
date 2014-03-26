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
-module(logplex_channel).

-export([whereis/1
         ,register/1
         ,unregister/1
         ,post_msg/2
        ]).

-export([create_id/0, delete/1, lookup/1,
         lookup_tokens/1, lookup_drains/1, logs/2, info/1
         ,can_add_drain/1
        ]).

-export([new/1
         ,new/2
         ,new/3
         ,create/1
         ,destroy/1
         ,id/1
         ,binary_to_id/1
         ,id_to_binary/1
         ,name/1
         ,flags/1
        ]).

-export([lookup_flag/2
         ,lookup_flags/1
         ,is_flagged/2
         ,store/1
         ,cache/3
         ,set_local_flag/2
         ,unset_local_flag/2
         ,binary_to_flags/1
         ,flags_to_binary/1
         ,create_ets_table/0
        ]).

-export([num_channels/0]).

-compile({no_auto_import,[whereis/1]}).

-include("logplex.hrl").
-include("logplex_channel.hrl").
-include("logplex_logging.hrl").

-type id() :: integer().
-type name() :: binary().
-type flag() :: 'no_tail' | 'no_redis' | 'no_redis_local'.
-type flags() :: [flag()].
-type channel() :: #channel{}.
-export_type([id/0, name/0, flags/0]).

create(Name) ->
    Chan = new(create_id(), Name),
    store(Chan),
    Chan.

destroy(Chan) ->
    delete(id(Chan)).

new(Id) when is_integer(Id) ->
    #channel{id=Id}.
new(Id, Name) when is_integer(Id),
                   is_binary(Name) ->
    #channel{id=Id, name=Name}.
new(Id, Name, Flags) when is_integer(Id),
                          is_binary(Name),
                          is_list(Flags) ->
    #channel{id=Id, name=Name, flags=Flags}.

id(#channel{id=Id}) -> Id.
name(#channel{id=Name}) -> Name.
flags(#channel{flags=Flags}) -> Flags.

create_ets_table() ->
    ets:new(channels, [named_table, public, set, {keypos, #channel.id}]).

register({channel, ChannelId} = C)
  when is_integer(ChannelId) ->
    put(logplex_channel_id, ChannelId), %% post mortem debug info
    gproc:add_local_property(C, true).

unregister({channel, ChannelId} = C)
  when is_integer(ChannelId) ->
    erase(logplex_channel_id),
    gproc:unreg({p, l, C}).

whereis({channel, _ChannelId} = Name) ->
    [ Pid || {Pid, true} <- gproc:lookup_local_properties(Name) ].

post_msg(Where, Msg) when is_binary(Msg) ->
    case logplex_syslog_utils:from_msg(Msg) of
        {error, _} = E -> E;
        ParsedMsg -> post_msg(Where, ParsedMsg)
    end;
post_msg({channel, ChannelId} = Name, Msg) when is_tuple(Msg) ->
    logplex_stats:incr(#channel_stat{channel_id=ChannelId, key=channel_post}),
    gproc:send({p, l, Name}, {post, Msg}),
    ok.

-spec create_id() -> id() | {'error', term()}.
create_id() ->
    case redis_helper:channel_index() of
        ChannelId when is_integer(ChannelId) ->
            case redis_helper:create_channel(ChannelId) of
                ok ->
                    ChannelId;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

-spec store(channel()) -> any().
store(#channel{id=ChannelId, flags=Flags, name=Name}) ->
    redis_helper:store_channel(ChannelId, Name, flags_to_binary(Flags)).

-spec cache(id(), name(), flags()) -> channel().
cache(ChannelId, Name, Flags)
  when is_integer(ChannelId),
       is_binary(Name),
       is_list(Flags) ->
    Chan = #channel{id=ChannelId,
                    name=Name,
                    flags=Flags},
    true = ets:insert(channels, Chan),
    Chan.

-spec flags_to_binary(flags()) -> binary().
flags_to_binary(Flags) when is_list(Flags) ->
    Str = string:join([ atom_to_list(Flag) || Flag <- lists:usort(Flags),
                                              Flag =/= 'no_redis_local' ],
                      ":"),
    iolist_to_binary(Str).

-spec binary_to_flags(binary()) -> flags().
binary_to_flags(Str) when is_binary(Str) ->
    [ case Flag of
          <<"no_tail">> -> no_tail;
          <<"no_redis">> -> no_redis
      end || Flag <- binary:split(Str, <<":">>),
             Flag =/= <<>> ].

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            logplex_token:delete_by_channel(ChannelId),
            logplex_drain:delete_by_channel(ChannelId),
            redis_helper:delete_channel(ChannelId)
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    case ets:lookup(channels, ChannelId) of
        [Channel = #channel{}] -> Channel;
        _ -> undefined
    end.

-spec lookup_flag(F, id()) -> F | 'no_such_flag' | 'not_found'
                                  when is_subtype(F, flag()).
lookup_flag(Flag, ChannelId) when Flag =:= no_tail;
                                  Flag =:= no_redis ->
    try
        Flags =ets:lookup_element(channels, ChannelId, #channel.flags),
        case lists:member(Flag, Flags) of
            true -> Flag;
            false -> no_such_flag
        end
    catch
        error:badarg ->
            not_found
    end.

-spec lookup_flags(id()) -> flags() | 'not_found'.
lookup_flags(ChannelId) when is_integer(ChannelId) ->
    try ets:lookup_element(channels, ChannelId, #channel.flags)
    catch
        error:badarg ->
            not_found
    end.

is_flagged(Flags, ChannelId) when is_list(Flags),
                                  is_integer(ChannelId) ->
    case lookup_flags(ChannelId) of
        not_found -> not_found;
        ChannelFlags ->
            is_flagged(Flags, ChannelFlags)
    end;

is_flagged([], ChannelFlags) when is_list(ChannelFlags) ->
    false;

is_flagged([Flag | Rest], ChannelFlags) ->
    case lists:member(Flag, ChannelFlags) of
        true -> true;
        false -> is_flagged(Rest, ChannelFlags)
    end.

set_local_flag(ChannelId, Flag) when is_integer(ChannelId),
                                     Flag =:= no_redis_local ->
    try
        Flags =ets:lookup_element(channels, ChannelId, #channel.flags),
        case lists:member(Flag, Flags) of
            true -> true;
            false ->
                ets:update_element(channels, ChannelId, {#channel.flags, [no_redis_local | Flags]})
        end
    catch
        error:badarg ->
            false
    end.

unset_local_flag(ChannelId, Flag) when is_integer(ChannelId),
                                       Flag =:= no_redis_local ->
    try
        Flags =ets:lookup_element(channels, ChannelId, #channel.flags),
        case lists:member(Flag, Flags) of
            false -> true;
            true ->
                ets:update_element(channels, ChannelId, {#channel.flags, lists:delete(no_redis_local, Flags)})
        end
    catch
        error:badarg ->
            false
    end.

lookup_tokens(ChannelId) when is_integer(ChannelId) ->
    logplex_token:lookup_by_channel(ChannelId).

lookup_drains(ChannelId) when is_integer(ChannelId) ->
    logplex_drain:lookup_by_channel(ChannelId).

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->

    {Map, Interval, _TS} = logplex_shard_info:read(logplex_read_pool_map),
    Index = redis_shard:key_to_index(integer_to_list(ChannelId)),
    {_RedisUrl, Pool} = redis_shard:get_matching_pool(Index, Map, Interval),
    Cmd = [<<"LRANGE">>, iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))],
    case catch redo:cmd(Pool, Cmd) of
        {'EXIT', Err} ->
            ?ERR("at=fetch_logs channel_id=~p err=\"~p\"",
                 [ChannelId, Err]),
            [];
        Logs ->
            Logs
    end.

info(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        #channel{} ->
            {ChannelId,
             lookup_tokens(ChannelId),
             lookup_drains(ChannelId)};
        _ -> not_found
    end.

can_add_drain(ChannelId)
  when is_integer(ChannelId) ->
    CurrentCount = logplex_drain:count_by_channel(ChannelId),
    Max = logplex_app:config(max_drains_per_channel),
    if CurrentCount < Max ->
            can_add_drain;
       true ->
            cannot_add_drain
    end.

binary_to_id(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

id_to_binary(Id) when is_integer(Id) ->
    iolist_to_binary(integer_to_list(Id)).

num_channels() ->
    ets:info(channels, size).
