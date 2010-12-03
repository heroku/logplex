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

-export([create/3, delete/1, update_addon/2, lookup/1, logs/2, tokens/1, drains/1, info/1]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(ChannelName, AppId, Addon) when is_binary(ChannelName), is_integer(AppId), is_binary(Addon) ->
    case redis_helper:create_channel(ChannelName, AppId, Addon) of
        ChannelId when is_integer(ChannelId) ->
            logplex_grid:publish(?MODULE, {create, ChannelId, ChannelName, AppId, Addon}),
            ChannelId;
        Err ->
            Err
    end.

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            logplex_grid:publish(?MODULE, {delete_channel, ChannelId}),
            logplex_grid:publish(logplex_token, {delete_channel, ChannelId}),
            logplex_grid:publish(logplex_drain, {delete_channel, ChannelId}),
            redis_helper:delete_channel(ChannelId)
    end.

update_addon(ChannelId, Addon) when is_binary(ChannelId), is_binary(Addon) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        Channel ->
            logplex_grid:publish(?MODULE, {update_channel, Channel#channel{addon=Addon}}),
            logplex_grid:publish(logplex_token, {update_addon, ChannelId, Addon}),
            redis_helper:update_channel_addon(ChannelId, Addon)
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    case ets:lookup(?MODULE, ChannelId) of
        [Channel] -> Channel;
        _ -> undefined
    end.

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->
    logplex_read_queue:in(ChannelId, Num),
    receive
        {logs, Logs} -> Logs
    after 60000 ->
        {error, timeout}
    end.

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
             {tokens, [Token || #token{id=Token} <- tokens(ChannelId)]},
             {drains, [iolist_to_binary([Host, ":", integer_to_list(Port)]) || #drain{host=Host, port=Port} <- drains(ChannelId)]}];
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
    ets:new(?MODULE, [protected, named_table, set, {keypos, 2}]),
    ets:new(logplex_channel_tokens, [protected, named_table, bag, {keypos, 3}]),
    ets:new(logplex_channel_drains, [protected, named_table, bag, {keypos, 3}]),
    populate_cache(),
    spawn_link(fun() -> truncate_logs() end),
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
handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info({create, ChannelId, ChannelName, AppId, Addon}, State) ->
    ets:insert(?MODULE, #channel{id=ChannelId, name=ChannelName, app_id=AppId, addon=Addon}),
    {noreply, State};

handle_info({update_channel, Channel}, State) ->
    ets:insert(?MODULE, Channel),
    ets:update_element(logplex_channel_tokens, Channel#channel.id, {5, Channel#channel.addon}),
    {noreply, State};

handle_info({delete_channel, ChannelId}, State) ->
    ets:delete(?MODULE, ChannelId),
    ets:match_delete(logplex_channel_tokens, #token{id='_', channel_id=ChannelId, name='_', addon='_'}),
    ets:match_delete(logplex_channel_drains, #drain{id='_', channel_id=ChannelId, host='_', port='_'}),
    {noreply, State};

handle_info({create_token, ChannelId, TokenId, TokenName, Addon}, State) ->
    ets:insert(logplex_channel_tokens, #token{id=TokenId, channel_id=ChannelId, name=TokenName, addon=Addon}),
    {noreply, State};

handle_info({delete_token, ChannelId, TokenId}, State) ->
    ets:match_delete(logplex_channel_tokens, #token{id=TokenId, channel_id=ChannelId, name='_', addon='_'}),
    {noreply, State};

handle_info({create_drain, DrainId, ChannelId, Host, Port}, State) ->
    ets:insert(logplex_channel_drains, #drain{id=DrainId, channel_id=ChannelId, host=Host, port=Port}),
    {noreply, State};

handle_info({delete_drain, DrainId}, State) ->
    ets:match_delete(logplex_channel_drains, #drain{id=DrainId, channel_id='_', host='_', port='_'}),
    {noreply, State};

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
            #channel{addon=Addon} -> Token#token{addon=Addon};
            _ -> Token
        end
    end || Token <- redis_helper:lookup_tokens()],
    length(Tokens) > 0 andalso ets:insert(logplex_channel_tokens, Tokens),

    Drains = lists:foldl(
        fun(#drain{host=Host}=Drain, Acc) ->
            case inet:getaddr(binary_to_list(Host), inet) of
                {ok, _Ip} -> [Drain|Acc];
                _ -> Acc
            end
        end, [], redis_helper:lookup_drains()),
    length(Drains) > 0 andalso ets:insert(logplex_channel_drains, Drains),

    Channels = redis_helper:lookup_channels(),
    length(Channels) > 0 andalso ets:insert(?MODULE, Channels).

truncate_logs() ->
    timer:sleep(2000),
    Packet = lists:foldl(
        fun({ChannelId, _}, Acc) ->
            case ets:lookup(logplex_channel_tokens, ChannelId) of
                [#token{addon=Addon}|_] ->
                    [redis:build_request([
                        <<"LTRIM">>,
                        iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]),
                        <<"0">>,
                        list_to_binary(integer_to_list(spool_length(Addon) - 1))])|Acc];
                _ -> Acc
            end
        end, [], ets:tab2list(logplex_stats_channels)),
    length(Packet) > 0 andalso redis_helper:truncate_logs(iolist_to_binary(Packet)),
    truncate_logs().

spool_length(<<"advanced">>) -> ?ADVANCED_LOG_HISTORY;
spool_length(_) -> ?DEFAULT_LOG_HISTORY.