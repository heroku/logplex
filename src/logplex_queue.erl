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
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([in/1, out/0, info/0, set_max_length/1]).

-include_lib("logplex.hrl").

-record(state, {queue, length, max_length}).

-define(TIMEOUT, 30000).

%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

in(Packet) ->
    gen_server:cast(?MODULE, {in, Packet}).

out() ->
    gen_server:call(?MODULE, out, ?TIMEOUT).

info() ->
    gen_server:call(?MODULE, info, ?TIMEOUT).

set_max_length(MaxLength) when is_integer(MaxLength) ->
    gen_server:cast(?MODULE, {max_length, MaxLength}).

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
    start_workers(),
    MaxLength =
        case os:getenv("LOGPLEX_QUEUE_LENGTH") of
            false -> 2000;
            StrNum -> list_to_integer(StrNum)
        end,
    spawn_link(fun() -> report_stats(Self) end),
	{ok, #state{queue=queue:new(), length=0, max_length=MaxLength}}.

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
handle_call(out, _From, #state{queue=Queue, length=Length}=State) ->
    case queue:out(Queue) of
        {{value, Out}, Queue1} ->
            {reply, Out, State#state{queue=Queue1, length=Length-1}};
        {empty, _Queue} ->
            {reply, undefined, State}
    end;

handle_call(info, _From, #state{length=Length, max_length=MaxLength}=State) ->
    {reply, {Length, MaxLength}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({in, _Packet}, #state{length=Length, max_length=MaxLength}=State) when Length >= MaxLength ->
    logplex_stats:incr(queue_dropped),
    {noreply, State};

handle_cast({in, Packet}, #state{queue=Queue, length=Length}=State) ->
    Queue1 = queue:in(Packet, Queue),
    {noreply, State#state{queue=Queue1, length=Length+1}};

handle_cast({max_length, MaxLength}, State) ->
    {noreply, State#state{max_length=MaxLength}};

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
    ets:insert(logplex_stats, {queue_length, Length}),
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
start_workers() ->
    NumWorkers =
        case os:getenv("LOGPLEX_WORKERS") of
            false -> 10;
            StrNum -> list_to_integer(StrNum)
        end,
    lists:foldl(
        fun (_, Acc) ->
            case start_worker() of
                undefined -> Acc;
                Pid -> [Pid|Acc]
            end
        end, [], lists:seq(1,NumWorkers)).

start_worker() ->
    case logplex_worker_sup:start_child() of
        {ok, Pid} -> Pid;
        {ok, Pid, _Info} -> Pid;
        {error, Reason} ->
            log(error, "failed to start worker: ~p", [Reason]),
            undefined
    end.

report_stats(Pid) ->
    timer:sleep(60000),
    Pid ! report_stats,
    report_stats(Pid).