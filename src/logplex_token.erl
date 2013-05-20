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

-export([lookup/1
         ,lookup_by_channel/1
         ,lookup_ids_by_channel/1
        ]).

-export([id/1
         ,channel_id/1
         ,name/1
         ,cache/1
         ,load/1
         ,delete/1
         ,delete_by_id/1
         ,delete_by_channel/1
         ,new/3
         ,new/2
         ,new_unique_token_id/0
         ,new_token_id/0
        ]).

-export([store/1
         ,create/2
         ,destroy/1
         ,create_ets_table/0
         ,reindex_tokens/1
         ,reindex_tokens/0
         ,num_records/0
        ]).

-include("logplex.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-type id() :: binary().
-type name() :: binary().
-type token() :: #token{}.

-record(token_idx, {key :: {logplex_channel:id(),
                            id() | '$1'} }).

-export_type([id/0
              ,name/0
              ,token/0
             ]).

-define(TOKEN_TAB, tokens).
-define(CHAN_TOKEN_TAB, channel_tokens).

new(Id, ChannelId, Name)
  when is_binary(Id), is_integer(ChannelId), is_binary(Name) ->
    #token{id = Id, channel_id = ChannelId, name = Name}.

new(ChannelId, Name)
  when is_integer(ChannelId), is_binary(Name) ->
    new(new_unique_token_id(), ChannelId, Name).

id(#token{id=Id}) -> Id.
channel_id(#token{channel_id=ChannelId}) -> ChannelId.
name(#token{name=Name}) -> Name.

create_ets_table() ->
    ets:new(?TOKEN_TAB, [named_table, public, set, {keypos, #token.id},
                         {read_concurrency, true},
                         {write_concurrency, true}]),
    ets:new(?CHAN_TOKEN_TAB, [named_table, public, ordered_set,
                              {keypos, #token_idx.key},
                              {read_concurrency, true},
                              {write_concurrency, true}]).

create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    TokenId = new_unique_token_id(),
    case store(new(TokenId, ChannelId, TokenName)) of
        ok ->
            TokenId;
        Err ->
            Err
    end.

destroy(TokenId) when is_binary(TokenId) ->
    case lookup(TokenId) of
        #token{} ->
            redis_helper:delete_token(TokenId);
        _ ->
            {error, not_found}
    end.

lookup(TokenId) when is_binary(TokenId) ->
    case ets:lookup(?TOKEN_TAB, TokenId) of
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
    case ets:lookup(?TOKEN_TAB, TokenId) of
        [#token{}] -> new_unique_token_id(Retries-1);
        [] -> TokenId
    end.

new_token_id() ->
    iolist_to_binary(["t.", uuid:to_iolist(uuid:v4())]).

lookup_by_channel(ChannelId) ->
    lists:flatmap(fun (Id) ->
                          ets:lookup(?TOKEN_TAB, Id)
                  end,
                  lookup_ids_by_channel(ChannelId)).

store(#token{id=Token,
             channel_id=ChannelId,
             name=Name}) ->
    redis_helper:create_token(ChannelId, Token, Name).

%% Load token into ETS and index it.
cache(Token = #token{}) ->
    load(Token),
    ets:insert(?CHAN_TOKEN_TAB, index_rec(Token)).

%% Load token into ETS only - used by nsync callback for faster boot time.
load(Token = #token{}) ->
    ets:insert(?TOKEN_TAB, Token).

delete(Token = #token{id = Id}) ->
    ets:delete(?TOKEN_TAB, Id),
    ets:delete(?CHAN_TOKEN_TAB, index_rec(Token)).

delete_by_id(Id) ->
    case lookup(Id) of
        Token = #token{} ->
            delete(Token);
        _ ->
            ok
    end.

delete_by_channel(ChannelId) when is_integer(ChannelId) ->
    Ids = lookup_ids_by_channel(ChannelId),
    [ ets:delete(?TOKEN_TAB, Id)
      || Id <- Ids ],
    [ ets:delete(?CHAN_TOKEN_TAB, index_key(ChannelId, Id))
      || Id <- Ids ],
    ok.

lookup_ids_by_channel(ChannelId) when is_integer(ChannelId) ->
    ets:select(?CHAN_TOKEN_TAB,
               [{#token_idx{key = {ChannelId, '$1'}},[],['$1']}]).

index_rec(#token{id = Id, channel_id = Chan}) ->
    index_rec(Chan, Id).

index_rec(Chan, Id) ->
    #token_idx{key = index_key(Chan, Id) }.

index_key(Chan, Id) ->
    {Chan, Id}.

sel_pat() ->
    ets:fun2ms(fun (#token{channel_id = C, id = I}) ->
                       {C, I}
               end).

reindex_tokens() ->
    Step = logplex_app:config(ets_token_reindex_step_size),
    reindex_tokens(Step).

reindex_tokens(Step) ->
    ets:delete_all_objects(?CHAN_TOKEN_TAB),
    reindex_tokens_itr(ets:select(?TOKEN_TAB,
                                  sel_pat(),
                                  Step)).

reindex_tokens_itr('$end_of_table') ->
    ok;
reindex_tokens_itr({Recs, Cont}) ->
    ets:insert(?CHAN_TOKEN_TAB,
               [index_rec(Chan, Id)
                || {Chan, Id} <- Recs]),
    reindex_tokens_itr(ets:select(Cont)).

num_records() ->
    ets:info(?TOKEN_TAB, size).
