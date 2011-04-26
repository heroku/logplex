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

%% API
-export([create/3, delete/1, update_addon/2, lookup/1, logs/2, tokens/1, drains/1, info/1]).

-include_lib("logplex.hrl").

%% API functions
create(ChannelName, AppId, Addon) when is_binary(ChannelName), is_integer(AppId), is_binary(Addon) ->
    case redis_helper:create_channel(ChannelName, AppId, Addon) of
        ChannelId when is_integer(ChannelId) ->
            ChannelId;
        Err ->
            Err
    end.

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            [logplex_token:delete(TokenId) || TokenId <- tokens(ChannelId)],
            [logplex_drain:delete(DrainId) || DrainId <- drains(ChannelId)],
            redis_helper:delete_channel(ChannelId)
    end.

update_addon(ChannelId, Addon) when is_integer(ChannelId), is_binary(Addon) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        Channel ->
            redis_helper:update_channel_addon(ChannelId, Addon),
            [redis_helper:update_token_addon(Token#token.id, Addon) || Token <- tokens(ChannelId)]
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    %% @todo: there's implicit coupling between this function and redis_helper:create_channel/3.
    %%        fix it.
    case ets:lookup(nsync:tid(?MODULE), redis_helper:channel_key(ChannelId)) of
        [{_, Channel}] ->
            Name = dict:fetch(<<"name">>, Channel),
            AppId = list_to_integer(binary_to_list(dict:fetch(<<"app_id">>, Channel))),
            Addon = dict:fetch(<<"addon">>, Channel),
            #channel{id=ChannelId, name=Name, app_id=AppId, addon=Addon};
        _ -> undefined
    end.

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->
    [{logplex_read_pool_map, {Map, Interval}}] = ets:lookup(logplex_shard_info, logplex_read_pool_map),
    Index = redis_shard:key_to_index(integer_to_list(ChannelId)),
    {_RedisUrl, Pool} = redis_shard:get_matching_pool(Index, Map, Interval),
    redis_pool:q(Pool, [<<"LRANGE">>, iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

tokens(ChannelId) when is_integer(ChannelId) ->
    logplex_token:tokens_of_channel(ChannelId).

drains(ChannelId) when is_integer(ChannelId) ->
    logplex_drain:drains_of_channel(ChannelId).

info(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        #channel{name=ChannelName, app_id=AppId, addon=Addon} ->
            [{channel_id, ChannelId},
             {channel_name, ChannelName},
             {app_id, AppId},
             {addon, Addon},
             {tokens, lists:sort([{Name, Token} || #token{id=Token, name=Name} <- tokens(ChannelId)])},
             {drains, [iolist_to_binary([<<"syslog://">>, Host, ":", integer_to_list(Port)]) || #drain{host=Host, port=Port} <- drains(ChannelId)]}];
        _ ->
            []
    end.
