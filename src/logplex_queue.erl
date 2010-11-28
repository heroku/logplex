-module(logplex_queue).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([in/1, out/0]).

-record(state, {queue, length, active, last_notified}).

-define(MAX_LENGTH, 1500).

%% API functions
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).

in(Packet) ->
    gen_server2:cast(?MODULE, {in, Packet}).

out() ->
    gen_server2:call(?MODULE, out, 30000).

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
	{ok, #state{queue=queue:new(), length=0, active=true}}.

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
handle_call(out, _From, #state{queue=Queue, length=Length, active=false, last_notified=Last}=State) ->
    {Out, Queue1, Length1} = dequeue(Queue, Length),
    case should_notify(Last, 100000) of
        true ->
            syslog_acceptor:active(true),
            {reply, Out, State#state{queue=Queue1, length=Length1, active=true, last_notified=now()}};
        false ->
            {reply, Out, State#state{queue=Queue1, length=Length1}}
    end;

handle_call(out, _From, #state{queue=Queue, length=Length}=State) ->
    {Out, Queue1, Length1} = dequeue(Queue, Length),
    {reply, Out, State#state{queue=Queue1, length=Length1}};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({in, _Packet}, #state{active=false}=State) ->
    {noreply, State};

handle_cast({in, _Packet}, #state{length=Length, last_notified=Last}=State) when Length >= ?MAX_LENGTH ->
    case should_notify(Last, 5000000) of
        true ->
            syslog_acceptor:active(false),
            error_logger:info_msg("queue over capacity~n"),
            {noreply, State#state{last_notified=now(), active=false}};
        false ->
            {noreply, State}
    end;

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
    io:format("logplex_stats logplex_queue length=~w~n", [Length]),
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
dequeue(Queue, Length) ->
    case queue:out(Queue) of
        {{value, Out}, Queue1} ->
            {Out, Queue1, Length-1};
        {empty, _Queue} ->
            {undefined, Queue, Length}
    end.

should_notify(undefined, _) -> true;
should_notify(Last, MicroSecs) ->
    timer:now_diff(now(), Last) > MicroSecs.

report_stats(Pid) ->
    timer:sleep(60000),
    Pid ! report_stats,
    report_stats(Pid).