%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Access functions for reading and caching logplex redis shard info
%% @end
-module(logplex_shard_info).

-define(TABLE, logplex_shard_info).
-define(TS_POS, 4).

-export([save/3
         ,read/1
         ,cached_read/2
         ,map_interval/1
         ,pid_info/1
         ,map_list/1
         ,pid_list/1
         ,copy/2
         ,delete/1
        ]).

-type key() :: 'logplex_read_pool_map' | 'logplex_redis_buffer_map' |
               'new_logplex_read_pool_map' | 'new_logplex_redis_buffer_map' |
               'backup_logplex_read_pool_map' | 'backup_logplex_redis_buffer_map'.
-type map() :: dict().
-type interval() :: pos_integer().
-type shard_info() :: {map(), interval(), erlang:timestamp()}.
-type entry() :: {interval(), {Url::iolist(), pid()}}.

-export_type([shard_info/0]).

-spec save(key(), map(), interval()) -> 'true'.
save(Key, Map, Interval) ->
    TS = erlang:now(),
    ets:insert(?TABLE, [{Key, Map, Interval, TS}]).

-spec read(key()) -> shard_info() | 'no_such_key'.
read(Key) ->
    case ets:lookup(?TABLE, Key) of
        [{Key, Map, Interval, TS}] ->
            {Map, Interval, TS};
        [] ->
            no_such_key
    end.

-spec cached_read(key(), shard_info()) -> shard_info() | 'no_such_key'.
cached_read(Key, SI) ->
    case info_outdated(Key, ts(SI)) of
        up_to_date ->
            SI;
        out_of_date ->
            read(Key)
    end.

-spec ts(shard_info()) -> erlang:timestamp().
ts({_Map, _Interval, TS}) -> TS.

-spec info_outdated(key(), erlang:timestamp()) ->
                           'up_to_date' |
                           'out_of_date'.
info_outdated(Key, TS) ->
    try ets:lookup_element(?TABLE, Key, ?TS_POS) of
        TS -> up_to_date;
        _Newer -> out_of_date
    catch
        error:badarg ->
            %% XXX - change to 'no_such_key' post v33
            out_of_date
    end.

-spec map_interval(shard_info()) -> {map(), interval()}.
map_interval({Map, Interval, _TS}) ->
    {Map, Interval}.


-spec pid_info(pid()) ->
                      'undefined' |
                      {key(),
                       {entry(), map(), interval()}}.
pid_info(Pid) ->
    case pid_info(Pid, read(logplex_read_pool_map)) of
        undefined ->
            case pid_info(Pid, read(logplex_redis_buffer_map)) of
                undefined -> undefined;
                Info -> {logplex_redis_buffer_map, Info}
            end;
        Info ->
            {logplex_read_pool_map, Info}
    end.

pid_info(Pid, {Map, V, _TS}) ->
    case [ Item
           || Item = {_Shard, {_Url, OldPid}} <- dict:to_list(Map),
              OldPid =:= Pid] of
        [ Item ] ->
            {Item, Map, V};
        [] -> undefined
    end.

map_list(Key) ->
    {Map, _, _} = read(Key),
    dict:to_list(Map).

pid_list(Key) ->
    [ Pid || {_, {_, Pid}} <- map_list(Key) ].

copy(FromKey, ToKey) when FromKey =/= ToKey ->
    {Map, Interval, _} = read(FromKey),
    save(ToKey, Map, Interval).

delete('logplex_read_pool_map') -> {error, not_allowed};
delete('logplex_redis_buffer_map') -> {error, not_allowed};
delete(Key) ->
    ets:delete(logplex_shard_info, Key).
