-module(logplex_drain).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([create/3, delete/3, lookup/1]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(ChannelId, Host, Port) when is_integer(Port) ->
    create(ChannelId, Host, list_to_binary(integer_to_list(Port)));

create(ChannelId, Host, Port) when is_binary(ChannelId), is_binary(Host), (is_binary(Port) orelse Port == undefined) ->
    case ets:match_object(?MODULE, #drain{id='_', channel_id=ChannelId, host=Host, port=Port}) of
        [_] ->
            {error, already_exists};
        [] ->
            case redis_helper:drain_index() of
                DrainId when is_integer(DrainId) ->
                    BinDrainId = list_to_binary(integer_to_list(DrainId)),
                    logplex_grid:publish(?MODULE, {create_drain, BinDrainId, ChannelId, Host, Port}),
                    logplex_grid:publish(logplex_channel, {create_drain, BinDrainId, ChannelId, Host, Port}),
                    redis_helper:create_drain(BinDrainId, ChannelId, Host, Port),
                    DrainId;
                Error ->
                    Error
            end
    end.

delete(ChannelId, Host, Port) when is_binary(ChannelId), is_binary(Host) ->
    case ets:match_object(?MODULE, #drain{id='_', channel_id=ChannelId, host=Host, port=Port}) of
        [#drain{id=DrainId}] ->
            logplex_grid:publish(?MODULE, {delete_drain, DrainId}),
            logplex_grid:publish(logplex_channel, {delete_drain, DrainId}),
            redis_helper:delete_drain(DrainId);
        _ ->
            {error, not_found}
    end.

lookup(DrainId) when is_binary(DrainId) ->
    redis_helper:lookup_drain(DrainId).

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
handle_info({delete_channel, ChannelId}, State) ->
    ets:match_delete(?MODULE, #drain{id='_', channel_id=ChannelId, host='_', port='_'}),
    {noreply, State};

handle_info({create_drain, DrainId, ChannelId, Host, Port}, State) ->
    ets:insert(?MODULE, #drain{id=DrainId, channel_id=ChannelId, host=Host, port=Port}),
    {noreply, State};

handle_info({delete_drain, DrainId}, State) ->
    ets:delete(?MODULE, DrainId),
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
    Data = redis_helper:lookup_drains(),
    length(Data) > 0 andalso ets:insert(?MODULE, Data).