-module(logplex_channel).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([create/1, delete/1, push/2, logs/2, tokens/1, drains/1, info/1]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
	gen_server:start_link(?MODULE, [], []).

create(ChannelName) when is_binary(ChannelName) ->
    case redis:q([<<"INCR">>, <<"channel_index">>]) of
        {ok, ChannelId} ->
            redis:q([<<"SET">>, iolist_to_binary([<<"ch:">>, integer_to_list(ChannelId)]), ChannelName]),
            ChannelId;
        Error -> Error
    end.

delete(ChannelId) when is_binary(ChannelId) ->
    redis:q([<<"DEL">>, iolist_to_binary([<<"ch:">>, ChannelId])]).

push(ChannelId, Msg) when is_binary(ChannelId), is_binary(Msg) ->
    redis:q([<<"LPUSH">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), Msg]).

logs(ChannelId, Num) when is_binary(ChannelId), is_integer(Num) ->
    redis:q([<<"LRANGE">>, iolist_to_binary(["ch:", ChannelId, ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))]).

tokens(ChannelId) when is_binary(ChannelId) ->
    [Token || {ok, Token} <- redis:q([<<"SMEMBERS">>, iolist_to_binary([<<"ch:">>, ChannelId, <<":tokens">>])])].

drains(ChannelId) when is_binary(ChannelId) ->
    ets:lookup(logplex_channel_drains, ChannelId).

info(ChannelId) when is_binary(ChannelId) ->
    {ok, ChannelName} = redis:q([<<"GET">>, iolist_to_binary([<<"ch:">>, ChannelId])]),
    Tokens = tokens(ChannelId),
    [{channel_id, ChannelId},
     {channel_name, ChannelName},
     {tokens, Tokens}].

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
    ets:new(logplex_channel_drains, [protected, named_table, set, {keypos, 3}]),
    populate_cache(),
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
handle_info({add_drain_to_channel, DrainId, ChannelId, Host, Port}, State) ->
    ets:insert(logplex_channel_drains, #drain{id=DrainId, channel_id=ChannelId, host=Host, port=Port}),
    {noreply, State};

handle_info({remove_drain_from_channel, ChannelId, DrainId}, State) ->
    ets:match_delete(logplex_channel_drains, #drain{id=DrainId, channel_id=ChannelId, host='_', port='_'}),
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
    ets:insert(logplex_channel_drains, redis_helper:lookup_drains()).