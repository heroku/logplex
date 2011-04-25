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
-module(logplex_channel).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([create/3, delete/1, update_addon/2, lookup/1, logs/2, tokens/1, drains/1, info/1, refresh_dns/0]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(ChannelName, AppId, Addon) when is_binary(ChannelName), is_integer(AppId), is_binary(Addon) ->
    case redis_helper:create_channel(ChannelName, AppId, Addon) of
        ChannelId when is_integer(ChannelId) ->
            ChannelId;
        Err ->
            Err
    end.

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            gen_server:cast(?MODULE, {delete_channel, ChannelId}),
            redis_helper:delete_channel(ChannelId)
    end.

update_addon(ChannelId, Addon) when is_integer(ChannelId), is_binary(Addon) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        Channel ->
            gen_server:cast(?MODULE, {update_channel, Channel#channel{addon=Addon}}),
            redis_helper:update_channel_addon(ChannelId, Addon),
            [redis_helper:update_token_addon(Token#token.id, Addon) || Token <- tokens(ChannelId)]
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    %% @todo: there's implicit coupling between this function and redis_helper:create_channel/3.
    %%        fix it.
    ChannelKey = iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId), <<":data">>]),
    case ets:lookup(nsync:tid(?MODULE), ChannelKey) of
        [{_, Channel}] ->
            Name = dict:fetch(<<"name">>, Channel),
            AppId = list_to_integer(binary_to_list(dict:fetch(<<"app_id">>, Channel))),
            Addon = dict:fetch(<<"addon">>, Channel),
            #channel{id=ChannelId, name=Name, app_id=AppId, addon=Addon};
        _ -> undefined
    end.

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->
    [{logplex_read_pool_map, {Map, Interval}}] = ets:lookup(logplex_shard_info, logplex_read_pool_map),
    Index = redis_shard:key_to_index(integer_to_list(ChannelId)),
    {_RedisUrl, Pool} = redis_shard:get_matching_pool(Index, Map, Interval),
    redis_pool:q(Pool, [<<"LRANGE">>, iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

tokens(ChannelId) when is_integer(ChannelId) ->
    ets:lookup(logplex_channel_tokens, ChannelId).

drains(ChannelId) when is_integer(ChannelId) ->
    ets:lookup(logplex_channel_drains, ChannelId).

info(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        #channel{name=ChannelName, app_id=AppId, addon=Addon} ->
            [{channel_id, ChannelId},
             {channel_name, ChannelName},
             {app_id, AppId},
             {addon, Addon},
             {tokens, lists:sort([{Name, Token} || #token{id=Token, name=Name} <- tokens(ChannelId)])},
             {drains, [iolist_to_binary([<<"syslog://">>, Host, ":", integer_to_list(Port)]) || #drain{host=Host, port=Port} <- drains(ChannelId)]}];
        _ ->
            []
    end.

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
    ets:new(logplex_channel_tokens, [protected, named_table, bag, {keypos, 3}]),
    ets:new(logplex_channel_drains, [protected, named_table, bag, {keypos, 3}]),
    populate_cache(),
    spawn_link(fun refresh_dns/0),
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
handle_call({create_drain, DrainId, ChannelId, ResolvedHost, Host, Port}, _From, State) ->
    ets:insert(logplex_channel_drains, #drain{id=DrainId, channel_id=ChannelId, resolved_host=ResolvedHost, host=Host, port=Port}),
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
handle_cast({update_channel, #channel{id=ChannelId, addon=Addon}=Channel}, State) ->
    [begin
        ets:delete_object(logplex_channel_tokens, Token),
        ets:insert(logplex_channel_tokens, Token#token{addon=Addon})
    end || Token <- ets:lookup(logplex_channel_tokens, ChannelId)],
    {noreply, State};

handle_cast({delete_channel, ChannelId}, State) ->
    ets:match_delete(logplex_channel_tokens, #token{id='_', channel_id=ChannelId, name='_', app_id='_', addon='_'}),
    ets:match_delete(logplex_channel_drains, #drain{id='_', channel_id=ChannelId, resolved_host='_', host='_', port='_'}),
    {noreply, State};

handle_cast({create_token, ChannelId, TokenId, TokenName, AppId, Addon}, State) ->
    ets:insert(logplex_channel_tokens, #token{id=TokenId, channel_id=ChannelId, name=TokenName, app_id=AppId, addon=Addon}),
    {noreply, State};

handle_cast({delete_token, ChannelId, TokenId}, State) ->
    ets:match_delete(logplex_channel_tokens, #token{id=TokenId, channel_id=ChannelId, name='_', app_id='_', addon='_'}),
    {noreply, State};

handle_cast({delete_drain, DrainId}, State) ->
    ets:match_delete(logplex_channel_drains, #drain{id=DrainId, channel_id='_', resolved_host='_', host='_', port='_'}),
    {noreply, State};

handle_cast({resolve_host, Ip, Drain}, State) ->
    ets:delete_object(logplex_channel_drains, Drain),
    ets:insert(logplex_channel_drains, Drain#drain{resolved_host=Ip}),
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
    Tokens = [begin
        case logplex_channel:lookup(Token#token.channel_id) of
            #channel{app_id=AppId, addon=Addon} -> Token#token{app_id=AppId, addon=Addon};
            _ -> Token
        end
    end || Token <- redis_helper:lookup_tokens()],
    length(Tokens) > 0 andalso ets:insert(logplex_channel_tokens, Tokens),

    Drains = lists:foldl(
        fun(#drain{host=Host}=Drain, Acc) ->
            case logplex_utils:resolve_host(Host) of
                undefined -> Acc;
                Ip -> [Drain#drain{resolved_host=Ip}|Acc]
            end
        end, [], redis_helper:lookup_drains()),
    length(Drains) > 0 andalso ets:insert(logplex_channel_drains, Drains),

    ok.

refresh_dns() ->
    timer:sleep(60 * 1000),
    [begin
        case logplex_utils:resolve_host(Host) of
            undefined -> ok;
            Ip -> gen_server:cast(?MODULE, {resolve_host, Ip, Drain})
        end
    end || #drain{host=Host}=Drain <- ets:tab2list(logplex_channel_drains)],
    ?MODULE:refresh_dns().
