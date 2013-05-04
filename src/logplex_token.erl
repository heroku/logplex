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
-export([lookup_by_channel/1]).

-export([id/1
         ,channel_id/1
         ,name/1
         ,cache/1
         ,new/2
         ,new_unique_token_id/0
        ]).

-export([store/1]).

-include("logplex.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-type id() :: binary().
-type name() :: binary().
-type token() :: #token{}.

-export_type([id/0
              ,name/0
              ,token/0
             ]).

new(Id, ChannelId)
  when is_binary(Id), is_integer(ChannelId) ->
    #token{id = Id, channel_id = ChannelId}.

id(#token{id=Id}) -> Id.
channel_id(#token{channel_id=ChannelId}) -> ChannelId.
name(#token{name=Name}) -> Name.


create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    TokenId = new_unique_token_id(),
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

new_unique_token_id() ->
    new_unique_token_id(10).

new_unique_token_id(0) ->
    exit({error, failed_to_provision_token});
new_unique_token_id(Retries) when is_integer(Retries),
                                  Retries > 0 ->
    TokenId = new_token_id(),
    case ets:lookup(tokens, TokenId) of
        [#token{}] -> new_unique_token_id(Retries-1);
        [] -> TokenId
    end.

new_token_id() ->
    <<"t.", (uuid:v4())/binary>>.

lookup_by_channel(ChannelId) when is_integer(ChannelId) ->
    ets:select(tokens,
               ets:fun2ms(fun (#token{channel_id=C})
                                when C =:= ChannelId ->
                                  object()
                          end)).

store(#token{id=Token,
             channel_id=ChannelId,
             name=Name}) ->
    redis_helper:create_token(ChannelId, Token, Name).

cache(Token = #token{}) ->
    ets:insert(tokens, Token).
