%%%-------------------------------------------------------------------
%%% @author Jose Manuel Rodriguez Blanco <jose@mabase>
%%% @doc
%%% Helper module with some functions to ease the access to nsync
%%% data making it as seamless as possible to logplex
%%% @end
%%%-------------------------------------------------------------------
-module('nsync_helper').

-include_lib("logplex.hrl").
-include_lib("nsync_helper.hrl").

-export([complete_channelID/1, complete_tokenID/1, binary_to_integer/1]).

%%--------------------------------------------------------------------
%% @private
%% @doc Completes a token ID to its nsync representation
%% @spec complete_tokenID(ID::binary()) -> binary()
%% @end
%%--------------------------------------------------------------------
complete_tokenID(Token) ->
    iolist_to_binary([<<?TOKEN_PREFIX>>, Token, <<?DATA_SUFFIX>>]).

  
%%--------------------------------------------------------------------
%% @private
%% @doc Completes a channel ID to its nsync representation
%% @spec complete_channelID(ID::binary()) -> binary()
%% @end
%%--------------------------------------------------------------------
complete_channelID(Channel) ->
    iolist_to_binary([<<?CHANNEL_PREFIX>>, Channel, <<?DATA_SUFFIX>>]).

%%--------------------------------------------------------------------
%% @doc Binary to integer conversion
%% @spec binary_to_integer(binary()) -> integer()
%% @end
%%--------------------------------------------------------------------
binary_to_integer(?DEFAULT_BINARY) ->
    ?DEFAULT_INTEGER;
    
binary_to_integer(BinInt) ->
    list_to_integer(binary_to_list(BinInt)).

