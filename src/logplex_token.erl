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

%% API functions
create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    case logplex_channel:lookup(ChannelId) of
        #channel{app_id=AppId, addon=Addon} ->
            TokenId = list_to_binary("t." ++ string:strip(os:cmd("uuidgen"), right, $\n)),
            redis_helper:create_token(ChannelId, TokenId, TokenName, AppId, Addon),
            TokenId;
        _ ->
            {error, not_found}
    end.

delete(TokenId) when is_binary(TokenId) ->
    redis_helper:delete_token(TokenId).
    
lookup(Token) when is_binary(Token) ->
    case nsync_helper:tab_tokens() of
        undefined ->
            undefined;
        Tab ->
            lookup(Tab, Token)
    end.
lookup(Tab, Token) ->
    case ets:lookup(Tab, iolist_to_binary([<<"tok:">>,Token,<<":data">>])) of
        [{_, Dict}] ->
            #token{id = Token,
                   channel_id = list_to_integer(binary_to_list(dict:fetch(<<"ch">>, Dict))),
                   name = dict:fetch(<<"name">>, Dict),
                   app_id = list_to_integer(binary_to_list(dict:fetch(<<"app_id">>, Dict))),
                   addon = dict:fetch(<<"addon">>, Dict)
                  };
        _ -> undefined
    end.
