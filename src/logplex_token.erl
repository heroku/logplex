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
-module(logplex_token).

-export([create/2, lookup/1, delete/1, refresh_dns/0, init/1, loop/0]).

-include_lib("logplex.hrl").

create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    case logplex_channel:lookup(ChannelId) of
        #channel{app_id=AppId, addon=Addon} ->
            TokenId = list_to_binary("t." ++ string:strip(os:cmd("uuidgen"), right, $\n)),
            case redis_helper:create_token(ChannelId, TokenId, TokenName) of
                ok ->
                    Token = #token{id=TokenId, channel_id=ChannelId, name=TokenName, app_id=AppId, addon=Addon},
                    ets:insert(tokens, Token),
                    TokenId;
                Err ->
                    Err
            end;
        _ ->
            {error, not_found}
    end.

delete(TokenId) when is_binary(TokenId) ->
    case lookup(TokenId) of
        #token{} ->
            case redis_helper:delete_token(TokenId) of
                ok ->
                    ets:delete(tokens, TokenId),
                    ok;
                Err ->
                    Err
            end;
        _ ->
            {error, not_found}
    end.

lookup(TokenId) when is_binary(TokenId) ->
    case ets:lookup(tokens, TokenId) of
        [Token] when is_record(Token, token) ->
            Token;
        _ ->
            undefined
    end.

refresh_dns() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    loop().

loop() ->
    timer:sleep(60 * 1000),
    refresh_tokens(ets:tab2list(tokens)),
    loop().

refresh_tokens([]) ->
    ok;

refresh_tokens([#token{drains=Drains}=Token|Tail]) ->
    Drains1 = [begin
        case logplex_utils:resolve_host(Host) of
            undefined -> Drain;
            Ip -> Drain#drain{resolved_host=Ip}
        end
    end || #drain{host=Host}=Drain <- Drains],
    ets:insert(tokens, Token#token{drains=Drains1}),
    refresh_tokens(Tail).
