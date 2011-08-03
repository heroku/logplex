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

-export([reserve_token/0, cache/3, create/5, create/4,
         delete/1, delete/3, clear_all/1, lookup/1]).

-include_lib("logplex.hrl").

reserve_token() ->
    Token = new_token(),
    case redis_helper:drain_index() of
        DrainId when is_integer(DrainId) ->
           {ok, DrainId, Token};
        Err ->
            Err
    end.

cache(DrainId, Token, ChannelId)  when is_integer(DrainId),
                                        is_binary(Token),
                                        is_integer(ChannelId) ->
    true = ets:insert(drains, #drain{id=DrainId, channel_id=ChannelId, token=Token}).

create(DrainId, Token, ChannelId, Host, Port) when is_integer(DrainId),
                                                   is_binary(Token),
                                                   is_integer(ChannelId),
                                                   is_binary(Host),
                                                   (is_integer(Port) orelse Port == undefined) ->
    case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host=Host, port=Port, tcp='_'}) of
        [_] ->
            {error, already_exists};
        [] ->
            case logplex_utils:resolve_host(Host) of
                undefined ->
                    error_logger:error_msg("invalid drain: ~p:~p~n", [Host, Port]),
                    {error, invalid_drain};
                _Ip ->
                    case redis_helper:create_drain(DrainId, ChannelId, Token, Host, Port) of
                        ok ->
                            #drain{id=DrainId, channel_id=ChannelId, token=Token, host=Host, port=Port};
                        Err ->
                            Err
                    end
            end
    end.

create(DrainId, ChannelId, Host, Port) when is_integer(DrainId),
                                            is_integer(ChannelId),
                                            is_binary(Host) ->
    case ets:lookup(drains, DrainId) of
        [#drain{channel_id=ChannelId, token=Token}] ->
            case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host=Host, port=Port, tcp='_'}) of
                [_] ->
                    {error, already_exists};
                [] ->
                    case logplex_utils:resolve_host(Host) of
                        undefined ->
                            error_logger:error_msg("invalid drain: ~p:~p~n", [Host, Port]),
                            {error, invalid_drain};
                        _Ip ->
                            case redis_helper:create_drain(DrainId, ChannelId, Token, Host, Port) of
                                ok -> #drain{id=DrainId, channel_id=ChannelId, token=Token, host=Host, port=Port};
                                Err -> Err
                            end
                    end
            end;
        _ ->
            {error, not_found}
    end.        

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host=Host, port=Port1, tcp='_'}) of
        [#drain{id=DrainId}|_] ->
            delete(DrainId);
        _ ->
            {error, not_found}
    end.

delete(DrainId) when is_integer(DrainId) ->
    redis_helper:delete_drain(DrainId).

clear_all(ChannelId) when is_integer(ChannelId) ->
    List = ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host='_', port='_', tcp='_'}),
    [delete(DrainId) || #drain{id=DrainId} <- List],
    ok.

lookup(DrainId) when is_integer(DrainId) ->
    case ets:lookup(drains, DrainId) of
        [Drain] -> Drain;
        _ -> undefined
    end.

new_token() ->
    new_token(10).

new_token(0) ->
    exit({error, failed_to_reserve_drain_token});

new_token(Retries) ->
    Token = list_to_binary("d." ++ string:strip(os:cmd("uuidgen"), right, $\n)),
    case ets:match_object(drains, #drain{id='_', channel_id='_', token=Token, resolved_host='_', host='_', port='_', tcp='_'}) of
        [#drain{}] -> new_token(Retries-1);
        [] -> Token
    end.
