%% Copyright (c) 2014 Alex Arnell <alex@heroku.com>
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
%% OF MERCHANLOOKUP_TABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(logplex_firehose).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-define(MASTER_TAB, firehose_master).
-define(SHARD_TAB, firehose_shards).

-export([next_shard/2,
         post_msg/3]).

-export([create_ets_tables/0,
         enable/0,
         disable/0]).

-export([firehose_channel_ids/0,
         firehose_filter_tokens/0]).

-record(shard_pool, {key=master_shard,
                     size=0 :: integer(),
                     pool={} :: tuple()}).
-record(shard, {id :: {integer(), binary()},
                channel_id :: logplex_channel:id() }).

%%%--------------------------------------------------------------------
%%% API
%%%--------------------------------------------------------------------

create_ets_tables() ->
    ets:new(?MASTER_TAB, [named_table, public, set,
                              {keypos, #shard_pool.key},
                              {read_concurrency, true}]),
    ets:new(?SHARD_TAB, [named_table, public, set,
                              {keypos, #shard.id},
                              {read_concurrency, true}]),
    [?MASTER_TAB, ?SHARD_TAB].

next_shard(ChannelId, Token) 
  when is_integer(ChannelId),
       is_binary(Token) ->
    lookup_shard({next_hash(ChannelId), Token}).

post_msg(SourceId, TokenName, Msg)
  when is_integer(SourceId),
       is_binary(TokenName) ->
    case next_shard(SourceId, TokenName) of
        undefined -> ok; % no shards, drop
        SourceId -> ok; % do not firehose a firehose
        ChannelId when is_integer(ChannelId) ->
            logplex_realtime:incr(metric_name(ChannelId)),
            logplex_channel:post_msg({channel, ChannelId}, Msg)
    end.

enable() ->
    ChannelIds = firehose_channel_ids(),
    FilterTokens = firehose_filter_tokens(),
    store_master_info(ChannelIds),
    store_channels(ChannelIds, FilterTokens),
    ok.

disable() ->
    store_master_info([]),
    ets:delete_all_objects(?SHARD_TAB),
    ok.

%%%--------------------------------------------------------------------
%%% private functions
%%%--------------------------------------------------------------------

compute_hash(_, 0) ->
    0;
compute_hash(ChannelId, Bounds) ->
    erlang:phash2({os:timestamp(), self(), ChannelId}, Bounds) + 1.

firehose_channel_ids() ->
    [ list_to_integer(Id) || Id <- split_list(firehose_channel_ids) ].

firehose_filter_tokens() ->
    [ list_to_binary(Token) || Token <- split_list(firehose_filter_tokens) ].

split_list(Env) ->
    case logplex_app:config(Env, []) of
        [] -> [];
        Ids when is_list(Ids) ->
            string:tokens(Ids, ",")
    end.


lookup_shard({0, _}) ->
    undefined;
lookup_shard(ShardId) ->
    try ets:lookup_element(?SHARD_TAB, ShardId, #shard.channel_id) of
        ChannelId -> ChannelId
    catch error:badarg -> undefined
    end.

next_hash(ChannelId) ->
    try ets:lookup_element(?MASTER_TAB, master_shard, #shard_pool.size) of
        Num -> compute_hash(ChannelId, Num)
    catch error:badarg -> 0
    end.

store_master_info(ChannelIds) ->
    ets:insert(?MASTER_TAB,
               #shard_pool{size=length(ChannelIds),
                           pool=ChannelIds}).

store_channels(ChannelIds, FilterTokens) ->
    ShardIds = lists:zip(lists:seq(1, length(ChannelIds)), ChannelIds),
    Shards = [ #shard{id={Idx, Token}, channel_id=ChannelId}
               || {Idx, ChannelId} <- ShardIds, Token <- FilterTokens ],
    store_channels(Shards).

store_channels([]) ->
    ok;
store_channels([Shard | Rest]) ->
    ets:insert(?SHARD_TAB, Shard),
    create_metric(Shard),
    store_channels(Rest).

create_metric(#shard{ channel_id=ChannelId }) ->
    logplex_realtime:create_counter_metric(metric_name(ChannelId)).

metric_name(ChannelId) when is_integer(ChannelId) ->
    binary_to_atom(
      iolist_to_binary(
        ["firehose.post.", integer_to_list(ChannelId)]), utf8).

