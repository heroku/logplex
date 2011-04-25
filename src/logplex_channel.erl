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
    case nsync_helper:tab_channel() of
	undefined ->
	    undefined;
	Tab ->
	    lookup(Tab, ChannelId)
    end.
lookup(Tab, ChannelId) ->
    case ets:lookup(Tab, iolist_to_binary([<<"ch:">>,integer_to_list(ChannelId),<<":data">>])) of
        [{_, Dict}] ->
            #channel{id = ChannelId,
		     name = dict:fetch(<<"name">>, Dict),
		     app_id = list_to_integer(binary_to_list(dict:fetch(<<"app_id">>, Dict))),
		     addon = dict:fetch(<<"addon">>, Dict)
		    };
	_ -> undefined
    end.

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->
    [{logplex_read_pool_map, {Map, Interval}}] = ets:lookup(logplex_shard_info, logplex_read_pool_map),
    Index = redis_shard:key_to_index(integer_to_list(ChannelId)),
    {_RedisUrl, Pool} = redis_shard:get_matching_pool(Index, Map, Interval),
    redis_pool:q(Pool, [<<"LRANGE">>, iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

tokens(ChannelId) when is_integer(ChannelId) ->
    case nsync_helper:tab_tokens() of
	undefined ->
	    [];
	TokensTab ->
	    tokens(TokensTab, ChannelId)
    end.
tokens(TokensTab, ChannelId) ->
    lists:foldl(fun({Token, Dict}, Acc) -> 
			case dict:fetch(<<"ch">>,Dict) == list_to_binary(integer_to_list(ChannelId)) of 
			    true -> 
				[#token{id = list_to_binary(string:sub_word(binary_to_list(Token), 2, $:)),
					channel_id = ChannelId,
					name = dict:fetch(<<"name">>, Dict),
					app_id = list_to_integer(binary_to_list(dict:fetch(<<"app_id">>, Dict))),
					addon = dict:fetch(<<"addon">>, Dict)
				       } | Acc]; 
			    false-> 
				Acc 
			end 
		end, [], ets:tab2list(TokensTab)).

drains(ChannelId) when is_integer(ChannelId) ->
    case nsync_helper:tab_drains() of
	undefined ->
	    [];
	DrainsTab ->
	    drains(DrainsTab, ChannelId)
    end.
drains(DrainsTab, ChannelId) ->
    lists:foldl(fun({Drain, Dict}, Acc) -> 
			case dict:fetch(<<"ch">>, Dict) == list_to_binary(integer_to_list(ChannelId)) of 
			    true -> 
				Id = list_to_integer(string:sub_word(binary_to_list(Drain), 2, $:)),
				case logplex_drain:lookup_host(Id) of
				    undefined -> Acc;
				    Ip -> 
					[#drain{id = Id,
						channel_id = ChannelId,
						host = dict:fetch(<<"host">>, Dict),
						port = 
						    case dict:find(<<"port">>, Dict) of
							{ok, Val} when is_binary(Val), size(Val) > 0 ->
							    list_to_integer(binary_to_list(Val));
							_ -> undefined
						    end,
						resolved_host = Ip
					       } | Acc]
				end;
			    false-> 
				Acc 
			end 
		end, [], ets:tab2list(DrainsTab)).

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
