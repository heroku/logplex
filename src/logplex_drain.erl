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
-module(logplex_drain).

-export([create/3, delete/3, clear_all/1, lookup/1]).
%% Nsync calls
-export([extract_drains/1, add_drain/2, del_drain/1, del_channel/1]).

-include_lib("logplex.hrl").
-include_lib("nsync_helper.hrl").

create(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host), (is_integer(Port) orelse Port == undefined) ->
    case ets:match_object(?MODULE, #drain{id='_', channel_id=ChannelId, resolved_host='_', host=Host, port=Port}) of
        [_] ->
            {error, already_exists};
        [] ->
            case logplex_utils:resolve_host(Host) of
                undefined ->
                    error_logger:error_msg("invalid drain: ~p:~p~n", [Host, Port]),
                    {error, invalid_drain};
                Ip ->
                    case redis_helper:drain_index() of
                        DrainId when is_integer(DrainId) ->
                            redis_helper:create_drain(DrainId, ChannelId, Host, Port),
                            DrainId;
                        Error ->
                            Error
                    end
            end
    end.

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case ets:match_object(?MODULE, #drain{id='_', channel_id=ChannelId, resolved_host='_', host=Host, port=Port1}) of
        [#drain{id=DrainId}|_] ->
            redis_helper:delete_drain(DrainId);
        _ ->
            {error, not_found}
    end.

clear_all(ChannelId) when is_integer(ChannelId) ->
    List = 
	ets:match_object(logplex_drain, #drain{id='_',
					       channel_id=ChannelId, 
					       resolved_host='_', 
					       host='_', 
					       port='_'}),
    [begin
        redis_helper:delete_drain(DrainId)
    end || #drain{id=DrainId} <- List],
    ok.

lookup(DrainId) when is_integer(DrainId) ->
    ets:lookup(logplex_drain, DrainId).


%%% Nsync functions
%%--------------------------------------------------------------------
%% @doc Extracts drains from a raw list of nsync data, assuming
%% channels have already been extracted. It will use the logplex_drain
%% ETSs, previously created in sync function in logplex_nsync_callback 
%% module
%% @spec extract_drains(RawData::list()) -> ok
%% @end
%%--------------------------------------------------------------------
extract_drains([{<<?DRAIN_PREFIX, Rest/binary>>, Dict}|T]) ->
    Size = size(Rest) - length(?DATA_SUFFIX),
    case Rest of
	<<RawID:Size/binary,?DATA_SUFFIX>> ->
	    ChID = nsync_helper:binary_to_integer(
		     dict:fetch(<<"ch">>, Dict)),
	    Ch = logplex_channel:lookup(ChID),
	    Host = dict:fetch(<<"host">>, Dict),
	    Drain = #drain{id = nsync_helper:binary_to_integer(RawID),
			   channel_id = Ch#channel.id,
			   resolved_host = logplex_utils:resolve_host(Host),
			   host = Host,
			   port = nsync_helper:binary_to_integer(
				    dict:fetch(<<"port">>, Dict))},
	    ets:insert(logplex_drain, Drain),
	    logplex_channel:add_drain(Drain);
	_ ->
	    ok
    end,
    extract_drains(T);


extract_drains([_|T]) ->
    extract_drains(T);

extract_drains([]) ->
    ok.


%%--------------------------------------------------------------------
%% @doc Adds a drain from its nsync raw data representation to 
%% logplex_drain ETS
%% @spec add_drain(ID::binary(), Params::list()) -> ok
%% @end
%%--------------------------------------------------------------------
add_drain(ID, Params) ->
    Drain = 
	add_drain_intern(Params, 
			 #drain{id =  nsync_helper:binary_to_integer(ID)}),
    ets:insert(logplex_drain, Drain),
    logplex_channel:add_drain(Drain).

%%--------------------------------------------------------------------
%% @private
%% @doc Iterates over the nsync data list in order to find the 
%% values needed for completing a correct drain record
%% @spec add_drain_intern(list(), drain()) -> drain()
%% @end
%%--------------------------------------------------------------------
add_drain_intern([<<"ch">>|[Ch|Rest]], Drain) ->
    add_drain_intern(
      Rest,
      Drain#drain{channel_id = nsync_helper:binary_to_integer(Ch)});

add_drain_intern([<<"port">>|[Port|Rest]], Drain) ->
    add_drain_intern(
      Rest,
      Drain#drain{port = nsync_helper:binary_to_integer(Port)});

add_drain_intern([<<"host">>|[Host|Rest]], Drain) ->
    add_drain_intern(
      Rest,
      Drain#drain{host = Host,
		  resolved_host = logplex_utils:resolve_host(Host)});

add_drain_intern([], Drain) ->
    Drain;

add_drain_intern([_|R], Drain) ->
    add_drain_intern(R, Drain).

%%--------------------------------------------------------------------
%% @doc Deletes a drain from logplex_drain ETS
%% @spec del_drain(ID::binary()) -> ok
%% @end
%%--------------------------------------------------------------------
del_drain(ID) ->
    KeyID = nsync_helper:binary_to_integer(ID),
    ets:delete(logplex_drain, KeyID),
    logplex_channel:del_drain(KeyID).

%%--------------------------------------------------------------------
%% @doc Deletes drains associated with a channel ID
%% @spec del_channel(ID::integer()) -> ok
%% @end
%%--------------------------------------------------------------------
del_channel(KeyID) ->
    ets:match_delete(logplex_drain, 
		     #drain{id='_', channel_id=KeyID, 
			    resolved_host='_', host='_', 
			    port='_'}).

