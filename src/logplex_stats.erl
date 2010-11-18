-module(logplex_stats).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([healthcheck/0, incr/1, flush/0]).

-record(state, {http_server_started=0,
                session_created=0,
                session_accessed=0,
                session_tailed=0,
                message_routed=0,
                message_processed=0,
                syslog_server_started=0,
                message_received=0}).

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

healthcheck() ->
    {ok, Count} = redis:q([<<"INCR">>, <<"healthcheck">>]),
    Count.

incr(Key) when is_atom(Key) ->
    gen_server:cast(?MODULE, {incr, Key});

incr({Key, Value}) when is_atom(Key), is_atom(Value) ->
    gen_server:cast(?MODULE, {incr, Key, Value}).
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
    spawn_link(fun flush/0),
	{ok, #state{}}.

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
handle_cast({incr, message_received}, #state{message_received=Val}=State) ->
    {noreply, State#state{message_received=Val+1}};

handle_cast({incr, message_processed}, #state{message_processed=Val}=State) ->
    {noreply, State#state{message_processed=Val+1}};

handle_cast({incr, message_routed}, #state{message_routed=Val}=State) ->
    {noreply, State#state{message_routed=Val+1}};

handle_cast(flush, State) ->
    io:format("message_received=~w message_processed=~w message_routed=~w~n", [
        State#state.message_received,
        State#state.message_processed,
        State#state.message_routed]),
    {noreply, #state{}};

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
flush() ->
    timer:sleep(60 * 1000),
    gen_server:cast(?MODULE, flush),
    flush().