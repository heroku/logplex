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
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([create/2,
         lookup/1,
         delete/1,
         nsync_callback/2]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    TokenId = iolist_to_binary([<<"tok:">>, Token, <<":data">>]),
    case ets:lookup(nsync:tid(?MODULE), TokenId) of
        [{_, T}] ->
            ChannelId = list_to_integer(binary_to_list(dict:fetch(<<"ch">>, T))),
            Name = dict:fetch(<<"name">>, T),
            C = logplex_channel:lookup(ChannelId),
            %% In order to construct a token, two lookups are needed here. Another
            %% design choice would be to make logplex_token:create/2 do:
            %%      redis_helper:create_token(ChannelId, TokenId, TokenName, AppId, Addon)
            %% then we have a little faster lookup but need more space consumption.
            %% Which to choose depends on usage senario.
            #token{id=Token, channel_id=ChannelId, name=Name, app_id=C#channel.app_id, addon=C#channel.addon};
        _ -> undefined
    end.

nsync_callback(Tab, {load, Key, _Val}) ->
    case Key of
        <<"tok:", _/binary>> -> Tab;
        _ -> undefined
    end;
nsync_callback(_Tab, {load, eof}) ->
    ok;
nsync_callback(Tab, {cmd, _Cmd, Args}) ->
    case Args of
        [<<"tok:", _/binary>> | _] -> Tab;
        _ -> undefined
    end;
nsync_callback(_Tab, _) ->
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init([]) ->
    Tab = ets:new(?MODULE, [public, named_table, set]),
    Opts = [{callback, {?MODULE, nsync_callback, [Tab]}}],
    {ok, _Pid} = nsync:start_link(Opts),
    % populate_cache(),
	{ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call({create_token, ChannelId, TokenId, TokenName, AppId, Addon}, _From, State) ->
    ets:insert(?MODULE, #token{id=TokenId, channel_id=ChannelId, name=TokenName, app_id=AppId, addon=Addon}),
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({delete_channel, ChannelId}, State) ->
    ets:match_delete(?MODULE, #token{id='_', channel_id=ChannelId, name='_', app_id='_', addon='_'}),
    {noreply, State};

handle_cast({delete_token, TokenId}, State) ->
    ets:delete(?MODULE, TokenId),
    {noreply, State};

handle_cast({update_addon, ChannelId, Addon}, State) ->
    [begin
        ets:insert(?MODULE, Token#token{addon=Addon})
    end || Token <- ets:match_object(?MODULE, #token{id='_', channel_id=ChannelId, name='_', app_id='_', addon='_'})],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
populate_cache() ->
    Data = [begin
        case logplex_channel:lookup(Token#token.channel_id) of
            #channel{app_id=AppId, addon=Addon} -> Token#token{app_id=AppId, addon=Addon};
            _ -> Token
        end
    end || Token <- redis_helper:lookup_tokens()],
    length(Data) > 0 andalso ets:insert(?MODULE, Data).
