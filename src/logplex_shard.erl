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
-module(logplex_shard).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([lookup/3, lookup_urls/0, urls/0, list_diff/2, poll_shard_urls/0]).

-include_lib("logplex.hrl").

-record(state, {urls}).

-define(TIMEOUT, 30000).

%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lookup(Key, Map, Interval) ->
    Index = redis_shard:key_to_index(Key),
    case redis_shard:get_matching_pool(Index, Map, Interval) of
        {_RedisUrl, Pid} -> Pid;
        Other -> exit({shard_not_found, Other})
    end.

urls() ->
    gen_server:call(?MODULE, urls).

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
    io:format("init ~p~n", [?MODULE]),
    Urls = lookup_urls(),

    [logplex_sup:start_child(logplex_read_queue_sup, [Url]) || Url <- Urls],
    [logplex_sup:start_child(logplex_redis_buffer_sup, [Url]) || Url <- Urls],

    ets:new(logplex_shard_info, [protected, set, named_table]),

    populate_info_table(),

    spawn_link(fun poll_shard_urls/0),

    {ok, #state{urls=Urls}}.

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
handle_call(urls, _From, State) ->
    {reply, State#state.urls, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(poll_shard_urls, #state{urls=Urls}=State) ->
    case lookup_urls() of
        Urls ->
            {noreply, State};
        NewList ->
            {Removed, Added} = list_diff(Urls, NewList),
            [begin
                logplex_sup:start_child(logplex_read_queue_sup, [Url]),
                logplex_sup:start_child(logplex_redis_buffer_sup, [Url])
            end || Url <- Added],
            [begin
                [begin
                    case logplex_read_queue:url(Pid) of
                        Url -> supervisor:terminate_child(logplex_read_queue_sup, Id);
                        _ -> ok
                    end
                end || {Id, Pid, worker, _Modules} <- supervisor:which_children(logplex_read_queue_sup)],
                [begin
                    case logplex_redis_buffer:url(Pid) of
                        Url -> supervisor:terminate_child(logplex_redis_buffer_sup, Id);
                        _ -> ok
                    end
                end || {Id, Pid, worker, _Modules} <- supervisor:which_children(logplex_redis_buffer_sup)]
            end || Url <- Removed],
            populate_info_table(),
            {noreply, State#state{urls=NewList}}
    end;

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
lookup_urls() ->
    case lists:sort([binary_to_list(Url) || {ok, Url} <- redis_helper:shard_urls()]) of
        [] ->
            case os:getenv("LOGPLEX_CONFIG_REDIS_URL") of
                false -> ["redis://127.0.0.1:6379/"];
                Url -> [Url]
            end;
        Urls ->
            Urls
    end.

populate_info_table() ->
    ReadQueues = [{logplex_read_queue:url(Pid), Pid} || {_Id, Pid, worker, _Modules} <- supervisor:which_children(logplex_read_queue_sup)],
    {ok, Map1, Interval1} = redis_shard:generate_map_and_interval(ReadQueues),

    RedisBuffers = [{logplex_redis_buffer:url(Pid), Pid} || {_Id, Pid, worker, _Modules} <- supervisor:which_children(logplex_redis_buffer_sup)],
    {ok, Map2, Interval2} = redis_shard:generate_map_and_interval(RedisBuffers),

    ets:delete_all_objects(logplex_shard_info),
    ets:insert(logplex_shard_info, {logplex_read_queue_map, {Map1, Interval1}}),
    ets:insert(logplex_shard_info, {logplex_redis_buffer_map, {Map2, Interval2}}),

    ok.

list_diff(List1, List2) ->
    list_diff(List1, List2, {[], []}).

list_diff([], [], Acc) ->
    Acc;

list_diff([A|Tail1], [A|Tail2], Acc) ->
    list_diff(Tail1, Tail2, Acc);

list_diff([A|Tail1], [B|Tail2]=List2, {Acc1, Acc2}) ->
    case lists:member(A, List2) of
        true -> list_diff([A|Tail1], Tail2, {Acc1, [B|Acc2]});
        false -> list_diff(Tail1, List2, {[A|Acc1], Acc2})
    end;

list_diff([], List2, {Acc1, Acc2}) ->
    {Acc1, List2 ++ Acc2};

list_diff(List1, [], {Acc1, Acc2}) ->
    {List1 ++ Acc1, Acc2}.

poll_shard_urls() ->
    timer:sleep(20 * 1000),
    gen_server:cast(?MODULE, poll_shard_urls),
    ?MODULE:poll_shard_urls().