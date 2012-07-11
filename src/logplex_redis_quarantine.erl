%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Table manager for redis channel quarantines.
%% @end
%%%-------------------------------------------------------------------
-module(logplex_redis_quarantine).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([create_ets_table/0
         ,channel/1
         ,quarantine_channel/1
         ,unquarantine_channel/1
        ]).
-define(TABLE, ?MODULE).

%%====================================================================
%% API
%%====================================================================

create_ets_table() ->
    ets:new(?MODULE, [named_table, public, set]).

-spec channel(ChannelId::integer()) -> quarantined | not_quarantined.
channel(ChannelId) when is_integer(ChannelId) ->
    try ets:lookup(?TABLE, {channel, ChannelId}) of
        [] ->
            not_quarantined;
        [_] ->
            quarantined
    catch
        error:badarg ->
            not_quarantined
    end.

quarantine_channel(ChannelId) when is_integer(ChannelId) ->
    ets:insert(?TABLE, {{channel, ChannelId}, os:timestamp()}).

unquarantine_channel(ChannelId) when is_integer(ChannelId) ->
    ets:delete(?TABLE, {channel, ChannelId}).
