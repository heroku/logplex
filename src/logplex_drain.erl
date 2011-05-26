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
    case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, resolved_host='_', host=Host, port=Port}) of
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
                            case redis_helper:create_drain(DrainId, ChannelId, Host, Port) of
                                ok ->
                                    Drain = #drain{id=DrainId, channel_id=ChannelId, resolved_host=Ip, host=Host, port=Port},
                                    ets:insert(drains, Drain),
                                    DrainId;
                                Err ->
                                    Err
                            end;
                        Err ->
                            Err
                    end
            end
    end.

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, resolved_host='_', host=Host, port=Port1}) of
        [#drain{id=DrainId}|_] ->
            delete(DrainId);
        _ ->
            {error, not_found}
    end.

delete(DrainId) when is_integer(DrainId) ->
    case redis_helper:delete_drain(DrainId) of
        ok ->
            ets:delete(drains, DrainId),
            ok;
        Err ->
            Err
    end.

clear_all(ChannelId) when is_integer(ChannelId) ->
    List = ets:match_object(drains, #drain{id='_', channel_id=ChannelId, resolved_host='_', host='_', port='_'}),
    [delete(DrainId) || #drain{id=DrainId} <- List],
    ok.

lookup(DrainId) when is_integer(DrainId) ->
    case ets:lookup(drains, DrainId) of
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
                ets:insert(drains, Drain#drain{resolved_host=Ip})
        end
    end || #drain{host=Host}=Drain <- ets:tab2list(drains)],
    loop().
