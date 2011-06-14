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

-export([create/3, delete/1, update_addon/2, lookup/1,
         lookup_tokens/1, lookup_drains/1, logs/2, info/1]).

-include_lib("logplex.hrl").

create(ChannelName, AppId, Addon) when is_binary(ChannelName), is_integer(AppId), is_binary(Addon) ->
    case redis_helper:channel_index() of
        ChannelId when is_integer(ChannelId) ->
            case redis_helper:create_channel(ChannelId, ChannelName, AppId, Addon) of
                ok ->
                    Channel = #channel{id=ChannelId, name=ChannelName, app_id=AppId, addon=Addon},
                    ets:insert(channels, Channel),
                    ChannelId;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            [logplex_token:delete(TokenId) || #token{id=TokenId} <- lookup_tokens(ChannelId)],
            [logplex_drain:delete(DrainId) || #drain{id=DrainId} <- lookup_drains(ChannelId)], 
            case redis_helper:delete_channel(ChannelId) of
                ok ->
                    ets:delete(channels, ChannelId),
                    ok;
                Err ->
                    Err
            end
    end.

update_addon(ChannelId, Addon) when is_integer(ChannelId), is_binary(Addon) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        Channel ->
            case redis_helper:update_channel_addon(ChannelId, Addon) of
                ok ->
                    case lookup_tokens(ChannelId) of
                        [] -> ok;
                        Tokens -> ets:insert(tokens, [Token#token{addon=Addon} || Token <- Tokens])
                    end,
                    ets:insert(channels, Channel#channel{addon=Addon}),
                    ok;
                Err ->
                    Err
            end
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    case ets:lookup(channels, ChannelId) of
        [Channel] -> Channel;
        _ -> undefined
    end.

lookup_tokens(ChannelId) when is_integer(ChannelId) ->
    ets:match_object(tokens, token_match_expr(ChannelId)).

lookup_drains(ChannelId) when is_integer(ChannelId) ->
    ets:match_object(drains, drain_match_expr(ChannelId)).

token_match_expr(ChannelId) ->
    #token{id='_', channel_id=ChannelId, name='_', app_id='_', addon='_', drains='_'}.

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
            Tokens = lookup_tokens(ChannelId),
            Drains = lookup_drains(ChannelId),
            [{channel_id, ChannelId},
             {channel_name, ChannelName},
             {app_id, AppId},
             {addon, Addon},
             {tokens, lists:sort([{Name, Token} || #token{id=Token, name=Name} <- Tokens])},
             {drains, [iolist_to_binary([<<"syslog://">>, Host, ":", integer_to_list(Port)]) || #drain{host=Host, port=Port} <- Drains]}];
        _ ->
            []
    end.
