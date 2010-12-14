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

-export([healthcheck/0, workers/0, incr/1, incr/2, incr/3]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

healthcheck() ->
    redis_helper:healthcheck().

workers() ->
    Sups = [logplex_redis_writer_sup, logplex_redis_buffer_sup, logplex_read_queue_sup, logplex_reader_sup, logplex_worker_sup, logplex_drain_sup],
    [{Sup, length(supervisor:which_children(Sup))} || Sup <- Sups].

incr(Key) ->
    incr(?MODULE, Key).

incr(Table, Key) ->
    incr(Table, Key, 1).

incr(Table, Key, Incr) when is_atom(Table), is_integer(Incr) ->
    case (catch ets:update_counter(Table, Key, Incr)) of
        {'EXIT', _} ->
            ets:insert(Table, {Key, 0}),
            incr(Table, Key, Incr);
        Res ->
            Res
    end.

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
    ets:new(logplex_stats_channels, [public, named_table, set]),
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
handle_info(flush, State) ->
    start_timer(),

    logplex_rate_limit:clear_all(),

    Stats = ets:tab2list(logplex_stats),
    ets:delete_all_objects(logplex_stats),
    io:format("logplex_stats~s~n", [lists:flatten([[" ", atom_to_list(Key), "=", integer_to_list(Value)] || {Key, Value} <- Stats, Value > 0])]),

    ChannelStats = ets:tab2list(logplex_stats_channels),
    ets:delete_all_objects(logplex_stats_channels),

    [begin
        io:format("logplex_channel_stats app_id=~w\tchannel_id=~w\tmessage_processed=~w~n", [AppId, ChannelId, Val])
    end || {{message_processed, AppId, ChannelId}, Val} <- ChannelStats, Val > 0],

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
start_timer() ->
    {_Mega, Secs, Micro} = now(),
    Time = 60000 - ((Secs rem 60 * 1000) + (Micro div 1000)),
    erlang:start_timer(Time, ?MODULE, flush).
