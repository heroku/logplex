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

-export([create/3, delete/1, update_addon/2, lookup/1, logs/2, tokens/1, drains/1, info/1, refresh_dns/0]).
%% Nsync calls
-export([extract_channels/1, add_token/1, del_token/1, add_channel/2, 
	 del_channel/1, add_drain/1, del_drain/1, update_channel/2]).

-include_lib("logplex.hrl").
-include_lib("nsync_helper.hrl").

%% API functions

create(ChannelName, AppId, Addon) when is_binary(ChannelName), is_integer(AppId), is_binary(Addon) ->
    redis_helper:create_channel(ChannelName, AppId, Addon).

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            redis_helper:delete_channel(ChannelId)
    end.

update_addon(ChannelId, Addon) when is_integer(ChannelId), is_binary(Addon) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _Channel ->
            redis_helper:update_channel_addon(ChannelId, Addon)
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    case ets:lookup(logplex_channel, ChannelId) of
        [Channel] -> Channel;
        _ -> undefined
    end.

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->
    [{logplex_read_pool_map, {Map, Interval}}] = ets:lookup(logplex_shard_info, logplex_read_pool_map),
    Index = redis_shard:key_to_index(integer_to_list(ChannelId)),
    {_RedisUrl, Pool} = redis_shard:get_matching_pool(Index, Map, Interval),
    redis_pool:q(Pool, [<<"LRANGE">>, iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

tokens(ChannelId) when is_integer(ChannelId) ->
    ets:lookup(logplex_channel_tokens, ChannelId).

drains(ChannelId) when is_integer(ChannelId) ->
    ets:lookup(logplex_channel_drains, ChannelId).

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


refresh_dns() ->
    [begin
        case logplex_utils:resolve_host(Host) of
            undefined -> ok;
            Ip ->     
		ets:delete_object(logplex_channel_drains, Drain),
		ets:insert(logplex_channel_drains, Drain#drain{resolved_host=Ip})
        end
    end || #drain{host=Host}=Drain <- ets:tab2list(logplex_channel_drains)].



%%% NSYNC CALLS
%%--------------------------------------------------------------------
%% @doc Extracts channels from a raw list of nsync data. It will use 
%% the logplex_channel ETS, previously created in sync function in 
%% logplex_nsync_callback module
%% @spec extract_channels(RawData::list()) -> ok
%% @end
%%--------------------------------------------------------------------
extract_channels([{<<?CHANNEL_PREFIX,Rest/binary>>, Dict}|T]) ->
    Size = size(Rest)- length(?DATA_SUFFIX),
    case Rest of
	<<RawID:Size/binary,?DATA_SUFFIX>> ->
	    ID = nsync_helper:binary_to_integer(RawID),
	    ets:insert(logplex_channel, 
		       #channel{id = ID,
			        name = 
				    dict:fetch(<<"name">>, Dict),
				app_id = 
				    nsync_helper:binary_to_integer(
				      dict:fetch(<<"app_id">>, Dict)),
				addon = 
				    dict:fetch(<<"addon">>, Dict)});
	_ ->
	    ok
    end,
    extract_channels(T);

extract_channels([_H|T]) ->
    extract_channels(T);

extract_channels([]) ->
    ok.


%%--------------------------------------------------------------------
%% @doc Adds a channel from its nsync raw data representation to 
%% logplex_channel ETS
%% @spec add_channel(ID::binary(), Params::list()) -> ok
%% @end
%%--------------------------------------------------------------------
add_channel(ID, Params) ->
    Ch = add_channel_intern(
	   Params, 
	   #channel{id = nsync_helper:binary_to_integer(ID)}),
    ets:insert(logplex_channel, Ch).

%%--------------------------------------------------------------------
%% @private
%% @doc Iterates over the nsync data list in order to find the 
%% values needed for completing a correct channel record
%% @spec add_channel_intern(list(), channel()) -> channel()
%% @end
%%--------------------------------------------------------------------
add_channel_intern([], Ch) ->
    Ch;

add_channel_intern([<<"name">>|[Name|Rest]], Ch) ->
    add_channel_intern(Rest, Ch#channel{name = Name});

add_channel_intern([<<"app_id">>|[AppID|Rest]], Ch) ->
    add_channel_intern(
      Rest, 
      Ch#channel{app_id = nsync_helper:binary_to_integer(AppID)});

add_channel_intern([<<"addon">>|[Addon|Rest]], Ch) ->
    add_channel_intern(Rest, Ch#channel{addon = Addon});

add_channel_intern([_|R], Ch) ->
    add_channel_intern(R, Ch).


%%--------------------------------------------------------------------
%% @doc Adds a token to logplex_channel_tokens ETS
%% @spec add_token(token()) -> ok
%% @end
%%--------------------------------------------------------------------
add_token(Token) ->
    ets:insert(logplex_channel_tokens, Token).

%%--------------------------------------------------------------------
%% @doc Adds a drain to logplex_channel_drains ETS
%% @spec add_drain(drain()) -> ok
%% @end
%%--------------------------------------------------------------------
add_drain(Drain) ->
    ets:insert(logplex_channel_drains, Drain).

%%--------------------------------------------------------------------
%% @doc Deletes a channel from logplex_channel ETS as well as tokens
%% and drains associated with it.
%% @spec del_channel(ID::binary()) -> ok
%% @end
%%--------------------------------------------------------------------
del_channel(ID) ->
    KeyID = nsync_helper:binary_to_integer(ID),
    ets:delete(logplex_channel, KeyID),
    ets:delete(logplex_channel_tokens, KeyID),
    ets:delete(logplex_channel_drains, KeyID),
    logplex_token:del_channel(KeyID),
    logplex_drain:del_channel(KeyID).

%%--------------------------------------------------------------------
%% @doc Deletes a token from logplex_chnanel_tokens
%% @spec del_token(ID::binary()) -> ok
%% @end
%%--------------------------------------------------------------------
del_token(ID) ->
    ets:match_delete(logplex_channel_tokens, 
		     #token{id = ID, channel_id = '_',
			    name = '_', app_id = '_', 
			    addon = '_'}).
	

%%--------------------------------------------------------------------
%% @doc Deletes a drain from logplex_channel_drains ETS
%% @spec del_drain(ID::integer()) -> ok
%% @end
%%--------------------------------------------------------------------
del_drain(ID) ->
    ets:match_delete(logplex_channel_drains,
		     #drain{id = ID, channel_id = '_',
			    port = '_', host = '_',
			    resolved_host = '_'}).


%%--------------------------------------------------------------------
%% @doc Updates a channel modifying its addon attribute. It will also
%% change it for all its associated data
%% @spec update_channel(ID::binary(), list()) -> ok
%% @end
%%--------------------------------------------------------------------
update_channel(ID, [<<"addon">>|[Addon|_]]) ->
    KeyID = nsync_helper:binary_to_integer(ID),
    ets:update_element(logplex_channel, KeyID, 
		       [{#channel.addon, Addon}]),
    logplex_token:update_channel(KeyID, Addon),
    [begin
        ets:delete_object(logplex_channel_tokens, Token),
        ets:insert(logplex_channel_tokens, Token#token{addon=Addon})
     end || Token <- ets:lookup(logplex_channel_tokens, KeyID)].


