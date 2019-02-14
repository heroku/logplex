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
-module(logplex_queue).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, start_link/2, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([in/2, out/1, out/2, info/1, get/2, set_max_length/2, stop/1]).
-export([register/2, all_workers/1]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-record(state, {
    dropped_stat_key,
    length_stat_key,
    queue,
    length,
    max_length,
    waiting,
    dict,
    workers=[],
    num_dropped=0,
    accepting=true,
    redis_url
}).

-define(TIMEOUT, 30000).

%% API functions
start_link(Props) when is_list(Props) ->
    gen_server:start_link(?MODULE, [Props], []).

start_link(Name, Props) when is_atom(Name), is_list(Props) ->
    gen_server:start_link({local, Name}, ?MODULE, [Props], []).

in(NameOrPid, Packet) when is_atom(NameOrPid); is_pid(NameOrPid) ->
    gen_server:cast(NameOrPid, {in, Packet}).

register(NameOrPid, WorkerPid) ->
    gen_server:cast(NameOrPid, {register, WorkerPid}).

all_workers(NameOrPid) ->
    gen_server:call(NameOrPid, all_workers).

out(NameOrPid) ->
    out(NameOrPid, 1).

-spec out(Name, pos_integer()) ->
                 'ok' | {'error', 'closed'} |
                 {non_neg_integer(), [any()]}
                     when Name :: pid() | atom().
out(NameOrPid, Num) when (is_atom(NameOrPid) orelse is_pid(NameOrPid)) andalso is_integer(Num) ->
    case gen_server:call(NameOrPid, {out, Num}, ?TIMEOUT) of
        empty ->
            Pid = case NameOrPid of Name when is_atom(Name) -> whereis(Name); _ -> NameOrPid end,
            receive
                stop -> exit(normal);
                {Pid, Packet} -> Packet
            after 60 * 1000 ->
                timeout
            end;
        Packet ->
            Packet
    end.

-spec info(atom()|pid()) ->
                  {pos_integer(), pos_integer()}.
info(NameOrPid) when is_atom(NameOrPid); is_pid(NameOrPid) ->
    gen_server:call(NameOrPid, info, ?TIMEOUT).

get(NameOrPid, redis_url) when is_atom(NameOrPid); is_pid(NameOrPid) ->
    gen_server:call(NameOrPid, {get, redis_url}, ?TIMEOUT).

set_max_length(NameOrPid, MaxLength) when (is_atom(NameOrPid) orelse is_pid(NameOrPid)) andalso is_integer(MaxLength) ->
    gen_server:cast(NameOrPid, {max_length, MaxLength}).

stop(NameOrPid) when is_atom(NameOrPid); is_pid(NameOrPid) ->
    gen_server:cast(NameOrPid, stop).

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
init([Props]) ->
    Name = proplists:get_value(name, Props),
    Self = self(),
    ?INFO("at=init name=~p", [Name]),
    WorkerSup = proplists:get_value(worker_sup, Props),
    NumWorkers = proplists:get_value(num_workers, Props),
    WorkerArgs = proplists:get_value(worker_args, Props),
    start_workers(WorkerSup, NumWorkers, WorkerArgs),
    spawn_link(fun() -> report_stats(Self) end),
    State = #state{
        dropped_stat_key = build_stat_key(Name, "dropped"),
        length_stat_key = build_stat_key(Name, "length"),
        queue = queue:new(),
        length = 0,
        max_length = proplists:get_value(max_length, Props),
        waiting = queue:new(),
        dict = proplists:get_value(dict, Props, dict:new()),
        redis_url = proplists:get_value(redis_url, Props)
    },
    {ok, State}.

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
handle_call({out, Num}, {From, _Mref}, #state{queue=Queue, length=Length, waiting=Waiting}=State) ->
    State1 = enable_producer(State),
    case drain(Queue, Num) of
        {Items, Queue1} when is_list(Items), length(Items) > 0 ->
            NumItems = length(Items),
            {reply, {NumItems, lists:reverse(Items)}, State1#state{queue=Queue1, length=Length-NumItems}};
        _ ->
            {reply, empty, State1#state{waiting=queue:in(From, Waiting)}}
    end;

handle_call(info, _From, #state{length=Length, max_length=MaxLength}=State) ->
    {reply, {Length, MaxLength}, State};

handle_call({get, Key}, _From, #state{dict=Dict}=State) ->
    Result =
        case dict:find(Key, Dict) of
            {ok, Value} -> Value;
            error -> undefined
        end,
    {reply, Result, State};

handle_call(all_workers, _From, #state{workers=Workers}=State) ->
    {reply, Workers, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({in, _Packet}, #state{dict=Dict, dropped_stat_key=StatKey, length=Length, max_length=MaxLength, num_dropped=NumDropped,
                                  redis_url=RedisUrl}=State) when Length >= MaxLength ->
    logplex_realtime:incr(StatKey),
    logplex_stats:incr(#queue_stat{key=StatKey,
                                   redis_url=RedisUrl}),
    case dict:find(producer_callback, Dict) of
        {ok, Fun} -> Fun(self(), stop_accepting);
        error -> ok
    end,
    {noreply, State#state{accepting=false, num_dropped=NumDropped+1}};

handle_cast({in, Packet}, #state{queue=Queue, length=Length, waiting=Waiting}=State) ->
    case queue:out(Waiting) of
        {empty, _} ->
            Queue1 = queue:in(Packet, Queue),
            {noreply, State#state{queue=Queue1, length=Length+1}};
        {{value, Pid}, Waiting1} ->
            Pid ! {self(), {1, [Packet]}},
            {noreply, State#state{waiting=Waiting1}}
    end;

handle_cast({max_length, MaxLength}, State) ->
    {noreply, State#state{max_length=MaxLength}};

handle_cast(stop, State = #state{workers=Workers}) ->
    [ exit(Worker, shutdown) ||
        Worker <- Workers ],
    {stop, normal, State};

handle_cast({register, WorkerPid}, #state{workers=Workers}=State) ->
    {noreply, State#state{workers=[WorkerPid|Workers]}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(report_stats, #state{length_stat_key=StatKey, length=Length}=State) ->
    logplex_stats:incr(StatKey, Length),
    {noreply, State};

handle_info({'DOWN', _Ref, process, WorkerPid, _Reason}, #state{workers=Workers}=State) ->
    case lists:member(WorkerPid, Workers) of
        true ->
            NewState = State#state{workers=lists:delete(WorkerPid, Workers)},
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;
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
code_change("v69.11", #state{dict=Dict,
                             redis_url=RedisUrl}=State, _Extra) ->
    NewState = list_to_tuple(lists:sublist(tuple_to_list(State), tuple_size(State)-1)),
    case dict:find(redis_url, Dict) of
        {ok, RedisUrl} ->
            % The redis_url is not removed from the dictionary during
            % upgrade to make the rollback easier
            {ok, NewState};
        error ->
            % No redis URL in the dictionary
            Dict1 = dict:store(redis_url, RedisUrl, Dict),
            NewState1 = erlang:setelement(8, NewState, Dict1),
            {ok, NewState1}
    end;
code_change("v69.12", State, _Extra) when size(State) =:= 11 ->
    Dict = element(8, State),
    case dict:find(redis_url, Dict) of
        {ok, Value} ->
            {ok, State#state{redis_url=Value}};
        error ->
            {ok, State}
    end;
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
build_stat_key("logplex_" ++ Name, Postfix) when is_list(Postfix) ->
    build_stat_key(Name, Postfix);

build_stat_key(Name, Postfix) when is_list(Name), is_list(Postfix) ->
    Name ++ "_" ++ Postfix;

build_stat_key(Name, Postfix) ->
    exit({poorly_formatted_stat_key, Name, Postfix}).

start_workers(WorkerSup, NumWorkers, WorkerArgs) ->
    lists:foldl(
        fun (_, Acc) ->
            case start_worker(WorkerSup, WorkerArgs) of
                undefined -> Acc;
                Pid -> [Pid|Acc]
            end
        end, [], lists:seq(1, NumWorkers)).

start_worker(WorkerSup, WorkerArgs) ->
    case logplex_worker_sup:start_child(WorkerSup, [self() | WorkerArgs]) of
        {ok, Pid} when is_pid(Pid) ->
            erlang:monitor(process, Pid),
            Pid;
        {ok, Pid, _Info} ->
            erlang:monitor(process, Pid),
            Pid;
        {error, Reason} ->
            error_logger:error_msg("~p failed to start worker: ~p~n", [WorkerSup, Reason]),
            undefined
    end.

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

enable_producer(#state{accepting=true}=State) ->
    State;

enable_producer(#state{dict=Dict, length=Length, max_length=MaxLength, accepting=false, dropped_stat_key=Key, num_dropped=NumDropped}=State) ->
    case Length < (MaxLength div 2) of
        true ->
            case dict:find(producer_callback, Dict) of
                {ok, Fun} -> Fun(self(), start_accepting);
                error -> ok
            end,
            ?INFO("at=enable_producer accepting=true num_dropped=~w key=~p queue=~p~n", [NumDropped, Key, which_queue()]),
            State#state{accepting=true, num_dropped=0};
        false ->
            State
    end.

report_stats(Pid) ->
    timer:sleep(60000),
    Pid ! report_stats,
    report_stats(Pid).

which_queue() ->
    case process_info(self(), registered_name) of
        {registered_name, Name} -> Name;
        _ -> self()
    end.
