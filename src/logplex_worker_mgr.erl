-module(logplex_worker_mgr).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% API functions
start_link(RedisOpts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RedisOpts], []).

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
init([RedisOpts]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [protected, named_table, set]),
    [self() ! {'EXIT', {logplex_worker, start_link, []}, undefined} || _ <- lists:seq(1, 300)],
    [self() ! {'EXIT', {logplex_writer, start_link, [RedisOpts]}, undefined} || _ <- lists:seq(1, 800)],
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
handle_info({'EXIT', {M,F,A}, _Reason}, State) ->
    spawn_worker({M,F,A}),
    {noreply, State};

handle_info({'EXIT', Pid, _Reason}, State) ->
    case ets:lookup(?MODULE, Pid) of
        [{Pid, {M,F,A}}] ->
            spawn_worker({M,F,A});
        _ ->
            ok
    end,
    ets:delete(?MODULE, Pid),
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
spawn_worker({M,F,A}) ->
    case (catch erlang:apply(M,F,A)) of
        {ok, Pid} ->
            ets:insert(?MODULE, {Pid, {M,F,A}});
        _ ->
            erlang:send_after(1000, self(), {'EXIT', {M,F,A}, undefined})
    end.