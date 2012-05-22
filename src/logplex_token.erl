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

-export([create/2, lookup/1, delete/1]).

-include("logplex.hrl").

create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    TokenId = new_token(),
    case redis_helper:create_token(ChannelId, TokenId, TokenName) of
        ok ->
            TokenId;
        Err ->
            Err
    end.

delete(TokenId) when is_binary(TokenId) ->
    case lookup(TokenId) of
        #token{} ->
            redis_helper:delete_token(TokenId);
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

new_token() ->
    new_token(10).

new_token(0) ->
    exit({error, failed_to_provision_token});

new_token(Retries) ->
    Token = list_to_binary("t." ++ uuid:to_string(uuid:v4())),
    T = logplex_utils:empty_token(),
    case ets:match_object(tokens, T#token{id=Token}) of
        [#token{}] -> new_token(Retries-1);
        [] -> Token
    end.
