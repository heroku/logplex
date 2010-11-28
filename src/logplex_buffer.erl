-module(logplex_buffer).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([in/1, out/1]).

-record(state, {queue, length}).

-define(MAX_LENGTH, 2000).

%% API functions
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

in(Packet) ->
    gen_server2:cast(?MODULE, {in, Packet}).

out(Num) when is_integer(Num) ->
    gen_server2:call(?MODULE, {out, Num}, 30000).

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
    Self = self(),
    spawn_link(fun() -> report_stats(Self) end),
    {ok, #state{queue=queue:new(), length=0}}.

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
handle_call({out, Num}, _From, #state{queue=Queue, length=Length}=State) ->
    case drain(Queue, Num) of
        {Items, Queue1} when length(Items) > 0 ->
            NumItems = length(Items),
            {reply, {NumItems, lists:reverse(Items)}, State#state{queue=Queue1, length=Length-NumItems}};
        _ ->
            {reply, undefined, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({in, _Packet}, #state{length=Length}=State) when Length >= ?MAX_LENGTH ->
    logplex_stats:incr(buffer_dropped),
    {noreply, State};

handle_cast({in, Packet}, #state{queue=Queue, length=Length}=State) ->
    Queue1 = queue:in(Packet, Queue),
    {noreply, State#state{queue=Queue1, length=Length+1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(report_stats, #state{length=Length}=State) ->
    ets:insert(logplex_stats, {logplex_buffer, Length}),
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
drain(Queue, N) ->
    drain(Queue, N, []).

drain(Queue, 0, Acc) ->
    {Acc, Queue};

drain(Queue, N, Acc) ->
    case queue:out(Queue) of
        {{value, Out}, Queue1} ->
            drain(Queue1, N-1, [Out|Acc]);
        {empty, _Queue} ->
            {Acc, Queue}
    end.

report_stats(Pid) ->
    timer:sleep(60000),
    Pid ! report_stats,
    report_stats(Pid).