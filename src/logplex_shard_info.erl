%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc 
%% @end
-module(logplex_shard_info).

-define(TABLE, logplex_shard_info).
-define(TS_POS, 4).

-export([save/3
         ,read/1
         ,cached_read/2
         ,map_interval/1
        ]).

-type key() :: 'logplex_read_pool_map' | 'logplex_redis_buffer_map'.
-type map() :: dict().
-type interval() :: pos_integer().
-type shard_info() :: {map(), interval(), erlang:timestamp()}.

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
                           'out_of_date' |
                           'no_such_key'.
info_outdated(Key, TS) ->
    try ets:lookup_element(?TABLE, Key, ?TS_POS) of
        TS -> up_to_date;
        _Newer -> out_of_date
    catch
        error:badarg ->
            no_such_key
    end.

-spec map_interval(shard_info()) -> {map(), interval()}.
map_interval({Map, Interval, _TS}) ->
    {Map, Interval}.
