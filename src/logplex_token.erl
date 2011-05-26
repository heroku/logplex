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

-include_lib("logplex.hrl").

create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    case logplex_channel:lookup(ChannelId) of
        #channel{app_id=AppId, addon=Addon} ->
            TokenId = list_to_binary("t." ++ string:strip(os:cmd("uuidgen"), right, $\n)),
            {atomic, _} = mnesia:sync_transaction(
                fun() ->
                    Token = #token{id=TokenId, channel_id=ChannelId, name=TokenName, app_id=AppId, addon=Addon},
                    mnesia:write(token, Token, write)
                end), 
            TokenId;
        _ ->
            {error, not_found}
    end.

delete(TokenId) when is_binary(TokenId) ->
    case lookup(TokenId) of
        #token{} ->
            {atomic, _} = mnesia:transaction(
                fun() ->
                    mnesia:delete(token, TokenId, write)
                end),
            ok;
        _ ->
            ok
    end.

lookup(TokenId) when is_binary(TokenId) ->
    case ets:lookup(token, TokenId) of
        [Token] when is_record(Token, token) ->
            Token;
        _ ->
            undefined
    end.
