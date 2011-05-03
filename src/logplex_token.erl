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
%% Nsync calls
-export([extract_tokens/1, add_token/2, del_token/1, update_channel/2,
	del_channel/1]).

-include_lib("logplex.hrl").
-include_lib("nsync_helper.hrl").

create(ChannelId, TokenName) when is_integer(ChannelId), is_binary(TokenName) ->
    case logplex_channel:lookup(ChannelId) of
        #channel{app_id=AppId, addon=Addon} ->
            TokenId = list_to_binary("t." ++ string:strip(os:cmd("uuidgen"), right, $\n)),
            redis_helper:create_token(ChannelId, TokenId, TokenName),
            TokenId;
        _ ->
            {error, not_found}
    end.

delete(TokenId) when is_binary(TokenId) ->
    case lookup(TokenId) of
        #token{channel_id=ChannelId} ->
            redis_helper:delete_token(TokenId);
        _ ->
            ok
    end.

lookup(Token) when is_binary(Token) ->
    case ets:lookup(?MODULE, Token) of
        [Token1] when is_record(Token1, token) ->
            Token1;
        _ ->
            undefined
    end.



%%% NSYNC CALLS
%%--------------------------------------------------------------------
%% @doc Extracts tokens from a raw list of nsync data, assuming
%% channels have already been extracted. It will use the logplex_token
%% ETSs previously created in sync function in logplex_nsync_callback 
%% module.
%% @spec extract_tokens(RawData::list()) -> ok
%% @end
%%--------------------------------------------------------------------
extract_tokens([{<<?TOKEN_PREFIX, Rest/binary>>, Dict}|T]) ->
    Size = size(Rest) - length(?DATA_SUFFIX),
    case Rest of
	<<RawID:Size/binary,?DATA_SUFFIX>> ->
	    ChID = nsync_helper:binary_to_integer(
		     dict:fetch(<<"ch">>, Dict)),
	    Ch = logplex_channel:lookup(ChID),
	    Token = #token{id = RawID,
			   channel_id = Ch#channel.id,
			   name = 
			       binary_to_list(dict:fetch(<<"name">>,
							 Dict)),
			   app_id = Ch#channel.app_id,
			   addon = Ch#channel.addon},
	    ets:insert(logplex_token, Token),
	    logplex_channel:add_token(Token);
	_ ->	    
	    ok
    end,
    extract_tokens(T);

extract_tokens([_|T]) ->
    extract_tokens(T);

extract_tokens([]) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Adds a token from its nsync raw data representation to 
%% logplex_token ETS
%% @spec add_token(ID::binary(), Params::list()) -> ok
%% @end
%%--------------------------------------------------------------------
add_token(ID, Params) ->
    Tok = add_token_intern(Params, #token{id = ID}),
    Ch = logplex_channel:lookup(Tok#token.channel_id),
    TokComplete = 
	Tok#token{app_id = Ch#channel.app_id,
		  addon = Ch#channel.addon},	
    ets:insert(logplex_token, TokComplete),
    logplex_channel:add_token(TokComplete).


%%--------------------------------------------------------------------
%% @private
%% @doc Iterates over the nsync data list in order to find the 
%% values needed for completing a correct token record
%% @spec add_token_intern(list(), token()) -> token()
%% @end
%%--------------------------------------------------------------------
add_token_intern([], Tok) ->
    Tok;

add_token_intern([<<"name">>|[Name|Rest]], Tok) ->
    add_token_intern(Rest, Tok#token{name = Name});

add_token_intern([<<"ch">>|[Ch|Rest]], Tok) ->
    add_token_intern(
      Rest, 
      Tok#token{channel_id = nsync_helper:binary_to_integer(Ch)});

add_token_intern([_|R], Tok) ->
    add_token_intern(R, Tok).   


%%--------------------------------------------------------------------
%% @doc Deletes a token from logplex_token and logplex_channel_tokens
%% ETSs
%% @spec del_token(ID::binary()) -> ok
%% @end
%%--------------------------------------------------------------------
del_token(ID) ->
    ets:delete(logplex_token, ID), 
    logplex_channel:del_token(ID).

%%--------------------------------------------------------------------
%% @doc Updates the addon value for a specific channel ID
%% @spec update_channel(ID::integer(), Addon::binary()) -> ok
%% @end
%%--------------------------------------------------------------------
update_channel(KeyID, Addon) ->
    [ets:insert(logplex_token, Token#token{addon=Addon}) || 
	Token <- ets:match_object(logplex_token, 
				  #token{id='_', 
					 channel_id=KeyID, 
					 name='_', 
					 app_id='_', 
					 addon='_'})].

%%--------------------------------------------------------------------
%% @doc Deletes associated tokens with a channel ID
%% @spec del_channel(ID::integer()) -> ok
%% @end
%%--------------------------------------------------------------------
del_channel(KeyID) ->    
    ets:match_delete(logplex_token, 
		     #token{id ='_', channel_id = KeyID,
			    name = '_', app_id = '_', 
			    addon = '_'}).
