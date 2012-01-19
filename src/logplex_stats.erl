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
-module(logplex_stats).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([healthcheck/0, workers/0, incr/1, incr/2, cached/0]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

healthcheck() ->
    redis_helper:healthcheck().

workers() ->
    Sups = [logplex_redis_writer_sup, logplex_redis_buffer_sup, logplex_worker_sup],
    [{Sup, length(supervisor:which_children(Sup))} || Sup <- Sups].

incr(Key) ->
    incr(Key, 1).

-spec incr(#drain_stat{} | #channel_stat{} | list() | atom(), integer()) -> any().
incr(Key, Incr) when is_integer(Incr) ->
    try ets:update_counter(?MODULE, Key, Incr)
    catch error:badarg ->
            try ets:insert_new(?MODULE, {Key, Incr})
            catch error:badarg ->
                    catch ets:update_counter(?MODULE, Key, Incr)
            end
    end.

cached() ->
    gen_server:call(?MODULE, cached, 30000).

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
    ets:new(?MODULE, [public, named_table, set]),
    start_timer(),
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
handle_call(cached, _From, Stats) ->
    {reply, Stats, Stats};

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
handle_info({timeout, _TimerRef, flush}, _State) ->

    {Mega, S, _} = os:timestamp(),
    UnixTS = Mega * 1000000 + S,
    Stats = ets:tab2list(logplex_stats),
    [ begin
          log_stat(UnixTS, K, V),
          ets:update_counter(?MODULE, K, V * -1)
      end
      || {K, V} <- Stats,
         V =/= 0],

    start_timer(),
    {noreply, Stats};

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
start_timer() ->
    {_Mega, Secs, Micro} = now(),
    Time = 60000 - ((Secs rem 60 * 1000) + (Micro div 1000)),
    erlang:start_timer(Time, ?MODULE, flush).

log_stat(UnixTS, #drain_stat{drain_id=DrainId, channel_id=ChannelId, key=Key}, Val) ->
    io:format("logplex_stats ts=~p channel_id=~p drain_id=~p ~p=~p~n",
        [UnixTS, ChannelId, DrainId, Key, Val]);

log_stat(UnixTS, #channel_stat{channel_id=ChannelId, key=Key}, Val) ->
    io:format("logplex_stats ts=~p channel_id=~p ~p=~p~n",
        [UnixTS, ChannelId, Key, Val]);

log_stat(UnixTS, Key, Val) when is_atom(Key); is_list(Key) ->
    io:format("logplex_stats ts=~p ~p=~p~n", [UnixTS, Key, Val]).
