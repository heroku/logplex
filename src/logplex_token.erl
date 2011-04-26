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

%% API
-export([create/2,
         lookup/1,
         delete/1,
         tokens_of_channel/1]).

-include_lib("logplex.hrl").

%% API functions
create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    case logplex_channel:lookup(ChannelId) of
        #channel{app_id=AppId, addon=Addon} ->
            TokenId = list_to_binary("t." ++ string:strip(os:cmd("uuidgen"), right, $\n)),
            redis_helper:add_token_to_channel(ChannelId, TokenId),
            redis_helper:create_token(ChannelId, TokenId, TokenName, AppId, Addon),
            TokenId;
        _ ->
            {error, not_found}
    end.

delete(TokenId) when is_binary(TokenId) ->
    case lookup(TokenId) of
        #token{channel_id=ChannelId} ->
            redis_helper:delete_token_from_channel(ChannelId, TokenId),
            redis_helper:delete_token(TokenId);
        _ ->
            ok
    end.

lookup(Token) when is_binary(Token) ->
    %% @todo: there's implicit coupling between this function and redis_helper:create_token/3.
    %%        fix it.
    case ets:lookup(nsync:tid(?MODULE), redis_helper:token_key(Token)) of
        [{_, T}] ->
            ChannelId = list_to_integer(binary_to_list(dict:fetch(<<"ch">>, T))),
            Name = dict:fetch(<<"name">>, T),
            AppId = list_to_integer(binary_to_list(dict:fetch(<<"appid">>, T))),
            Addon = dict:fetch(<<"addon">>, T),
            #token{id=Token, channel_id=ChannelId, name=Name, app_id=AppId, addon=Addon};
        _ -> undefined
    end.

tokens_of_channel(ChannelId) when is_integer(ChannelId) ->
    ChannelTokensKey = redis_helper:channel_tokens_key(ChannelId),
    case ets:lookup(nsync:tid(logplex_channel_tokens), ChannelTokensKey) of
        [{_, Ids}] ->
            Tokens = [lookup(TokenId) || TokenId <- Ids],
            Tokens;
        _ -> undefined
    end.

