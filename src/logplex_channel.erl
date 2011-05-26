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

-export([create/3, delete/1, update_addon/2, lookup/1, lookup_drains/1, logs/2, info/1]).

-include_lib("logplex.hrl").

create(ChannelName, AppId, Addon) when is_binary(ChannelName), is_integer(AppId), is_binary(Addon) ->
    case sync_incr_channel_id() of
        {atomic, ChannelId} when is_integer(ChannelId) ->
            {atomic, _} = mnesia:sync_transaction(
                fun() ->
                    Channel = #channel{id=ChannelId, name=ChannelName, app_id=AppId, addon=Addon},
                    mnesia:write(channel, Channel, write)
                end),
            ChannelId;
        {aborted, Reason} ->
            {error, Reason}
    end.

sync_incr_channel_id() ->
    mnesia:sync_transaction(fun() ->
        case mnesia:wread({counters, channel}) of
            [{counters, channel, ChannelId}] ->
                mnesia:write(counters, {counters, channel, ChannelId+1}, write),
                ChannelId+1;
            [] ->
                mnesia:write(counters, {counters, channel, 1}, write),
                1
        end
    end).

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            {atomic, _} = mnesia:transaction(
                fun() ->
                    Tokens = mnesia:match_object(token, token_match_expr(ChannelId), read),
                    Drains = mnesia:match_object(drain, drain_match_expr(ChannelId), read),
                    [mnesia:delete(token, TokenId, write) || #token{id=TokenId} <- Tokens],
                    [mnesia:delete(drain, DrainId, write) || #drain{id=DrainId} <- Drains],
                    mnesia:delete(channel, ChannelId, write)
                end),
            ok 
    end.

update_addon(ChannelId, Addon) when is_integer(ChannelId), is_binary(Addon) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        Channel ->
            {atomic, _} = mnesia:transaction(
                fun() ->
                    Tokens = mnesia:match_object(token, token_match_expr(ChannelId), read),
                    [mnesia:write(token, Token#token{addon=Addon}, write) || Token <- Tokens],
                    mnesia:write(channel, Channel#channel{addon=Addon}, write)                     
                end),
            ok
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    case ets:lookup(channel, ChannelId) of
        [Channel] -> Channel;
        _ -> undefined
    end.

lookup_drains(ChannelId) when is_integer(ChannelId) ->
    ets:match_object(drain, drain_match_expr(ChannelId)).

token_match_expr(ChannelId) ->
    #token{id='_', channel_id=ChannelId, name='_', app_id='_', addon='_'}.

drain_match_expr(ChannelId) ->
    #drain{id='_', channel_id=ChannelId, resolved_host='_', host='_', port='_'}.

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->
    [{logplex_read_pool_map, {Map, Interval}}] = ets:lookup(logplex_shard_info, logplex_read_pool_map),
    Index = redis_shard:key_to_index(integer_to_list(ChannelId)),
    {_RedisUrl, Pool} = redis_shard:get_matching_pool(Index, Map, Interval),
    redis_pool:q(Pool, [<<"LRANGE">>, iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

info(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        #channel{name=ChannelName, app_id=AppId, addon=Addon} ->
            Tokens = ets:match_object(token, token_match_expr(ChannelId)),
            Drains = ets:match_object(drain, drain_match_expr(ChannelId)),
            [{channel_id, ChannelId},
             {channel_name, ChannelName},
             {app_id, AppId},
             {addon, Addon},
             {tokens, lists:sort([{Name, Token} || #token{id=Token, name=Name} <- Tokens])},
             {drains, [iolist_to_binary([<<"syslog://">>, Host, ":", integer_to_list(Port)]) || #drain{host=Host, port=Port} <- Drains]}];
        _ ->
            []
    end.
