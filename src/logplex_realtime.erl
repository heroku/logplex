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
-module(logplex_realtime).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([incr/1, incr/2]).

-include_lib("logplex.hrl").

-record(state, {
    message_received=0,
    message_processed=0,
    message_routed=0,
    work_queue_dropped=0,
    drain_buffer_dropped=0,
    redis_buffer_dropped=0
}).

%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

incr(Key) ->
    incr(Key, 1).

incr(Key, Inc) when is_integer(Inc) ->
    gen_server:cast(?MODULE, {incr, Key, Inc}).

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
    spawn_link(fun() -> flush(Self) end),
    spawn_link(fun() -> register() end),
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
handle_cast({incr, message_received, Incr}, #state{message_received=Count}=State) ->
    {noreply, State#state{message_received=Count+Incr}};

handle_cast({incr, message_processed, Incr}, #state{message_processed=Count}=State) ->
    {noreply, State#state{message_processed=Count+Incr}};

handle_cast({incr, message_routed, Incr}, #state{message_routed=Count}=State) ->
    {noreply, State#state{message_routed=Count+Incr}};

handle_cast({incr, "work_queue_dropped", Incr}, #state{work_queue_dropped=Count}=State) ->
    {noreply, State#state{work_queue_dropped=Count+Incr}};

handle_cast({incr, "drain_buffer_dropped", Incr}, #state{drain_buffer_dropped=Count}=State) ->
    {noreply, State#state{drain_buffer_dropped=Count+Incr}};

handle_cast({incr, "redis_buffer_dropped", Incr}, #state{redis_buffer_dropped=Count}=State) ->
    {noreply, State#state{redis_buffer_dropped=Count+Incr}};

handle_cast(_Msg, State) ->
    io:format("realtime: recv'd ~p~n", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(flush, State) ->
    spawn(fun() ->
        Json = iolist_to_binary(mochijson2:encode({struct, [
            {message_received, State#state.message_received},
            {message_processed, State#state.message_processed},
            {message_routed, State#state.message_routed},
            {work_queue_dropped, State#state.work_queue_dropped},
            {drain_buffer_dropped, State#state.drain_buffer_dropped},
            {redis_buffer_dropped, State#state.redis_buffer_dropped}
        ]})),
        redis_helper:publish_stats(logplex_utils:instance_name(), Json)
    end),
    {noreply, #state{}};

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
flush(Pid) ->
    timer:sleep(1000),
    Pid ! flush,
    flush(Pid).

register() ->
    redis_helper:register_stat_instance(),
    timer:sleep(10 * 1000),
    register().