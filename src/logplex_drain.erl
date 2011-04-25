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

%% API
-export([create/3, delete/3, clear_all/1, lookup/1]).

-include_lib("logplex.hrl").

%% API functions
create(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host), (is_integer(Port) orelse Port == undefined) ->
    case lookup_drains(ChannelId, Host, Port) of
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
                            gen_server:call(logplex_channel, {create_drain, DrainId, ChannelId, Ip, Host, Port}, 8000),
                            redis_helper:create_drain(DrainId, ChannelId, Host, Port),
                            DrainId;
                        Error ->
                            Error
                    end
            end
    end.

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case lookup_drains(ChannelId, Host, Port1) of
        [#drain{id=DrainId}|_] ->
            gen_server:cast(logplex_channel, {delete_drain, DrainId}),
            redis_helper:delete_drain(DrainId);
        _ ->
            {error, not_found}
    end.

clear_all(ChannelId) when is_integer(ChannelId) ->
    List = lookup_drains(ChannelId),
    [begin
        gen_server:cast(logplex_channel, {delete_drain, DrainId}),
        redis_helper:delete_drain(DrainId)
    end || #drain{id=DrainId} <- List],
    ok.

lookup(DrainId) when is_integer(DrainId) ->
    redis_helper:lookup_drain(DrainId).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
lookup_drains(ChannelId, Host, Port) ->
    ets:match_object(logplex_channel_drains,
                    #drain{id='_', channel_id=ChannelId, resolved_host='_', host=Host, port=Port}).

lookup_drains(ChannelId) ->
    ets:match_object(logplex_channel_drains,
                    #drain{id='_', channel_id=ChannelId, resolved_host='_', host='_', port='_'}).
