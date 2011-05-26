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

-export([create/3, delete/3, clear_all/1, lookup/1, refresh_dns/0, init/1, loop/0]).

-include_lib("logplex.hrl").

create(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host), (is_integer(Port) orelse Port == undefined) ->
    case ets:match_object(drain, #drain{id='_', channel_id=ChannelId, resolved_host='_', host=Host, port=Port}) of
        [_] ->
            {error, already_exists};
        [] ->
            case logplex_utils:resolve_host(Host) of
                undefined ->
                    error_logger:error_msg("invalid drain: ~p:~p~n", [Host, Port]),
                    {error, invalid_drain};
                Ip ->
                    case sync_incr_drain_id() of
                        {atomic, DrainId} when is_integer(DrainId) ->
                            {atomic, _} = mnesia:transaction(
                                fun() ->
                                    Drain = #drain{id=DrainId, channel_id=ChannelId, resolved_host=Ip, host=Host, port=Port},
                                    mnesia:write(drain, Drain, write)
                                end),
                            DrainId;
                        {aborted, Reason} ->
                            {error, Reason}
                    end
            end
    end.

sync_incr_drain_id() ->
    mnesia:sync_transaction(fun() ->
        case mnesia:wread({counters, drain}) of
            [{counters, drain, DrainId}] ->
                mnesia:write(counters, {counters, drain, DrainId+1}, write),
                DrainId+1;
            [] ->
                mnesia:write(counters, {counters, drain, 1}, write),
                1
        end
    end).

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case ets:match_object(drain, #drain{id='_', channel_id=ChannelId, resolved_host='_', host=Host, port=Port1}) of
        [#drain{id=DrainId}|_] ->
            {atomic, _} = mnesia:transaction(
                fun() ->
                    mnesia:delete(drain, DrainId, write)
                end),
            ok;
        _ ->
            {error, not_found}
    end.

clear_all(ChannelId) when is_integer(ChannelId) ->
    List = ets:match_object(drain, #drain{id='_', channel_id=ChannelId, resolved_host='_', host='_', port='_'}),
    {atomic, _} = mnesia:transaction(
        fun() ->
            [mnesia:delete(drain, DrainId, write) || #drain{id=DrainId} <- List]
        end),
    ok.

lookup(DrainId) when is_integer(DrainId) ->
    case ets:lookup(drain, DrainId) of
        [Drain] -> Drain;
        _ -> undefined
    end.

refresh_dns() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    loop().

loop() ->
    timer:sleep(60 * 1000),
    [begin
        case logplex_utils:resolve_host(Host) of
            undefined -> ok;
            Ip ->
                {atomic, _} = mnesia:transaction(
                    fun() ->
                        mnesia:write(drain, Drain#drain{resolved_host=Ip}, write)
                    end)
        end
    end || #drain{host=Host}=Drain <- ets:tab2list(drain)],
    loop().
