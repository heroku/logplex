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

-export([lookup/3, lookup_urls/0, urls/0]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

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

    case length(logplex_queue_sup:which_children(logplex_redis_buffer_sup)) of
        N when N =:= length(Urls) ->
            ?INFO("at=restart existing_redis_buffer_children=~p", [N]);
        0 ->
            [logplex_queue_sup:start_child(logplex_redis_buffer_sup,
                                           [redis_buffer_args(Url)])
             || Url <- Urls],
            ?INFO("at=start new_redis_buffer_children=~p", [length(Urls)])
    end

    ets:new(logplex_shard_info, [protected, set, named_table]),

    populate_info_table(Urls),

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
    [exit(Pid, normal) || {_Id, Pid, worker, _Modules} <- supervisor:which_children(logplex_redis_buffer_sup)],
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
    case lists:sort([binary_to_list(Url) || Url <- redis_helper:shard_urls()]) of
        [] ->
            case os:getenv("LOGPLEX_CONFIG_REDIS_URL") of
                false -> ["redis://127.0.0.1:6379/"];
                Url -> [Url]
            end;
        Urls ->
            Urls
    end.

populate_info_table(Urls) ->
    Pools = add_pools(Urls, []),
    {ok, Map1, Interval1} = redis_shard:generate_map_and_interval(lists:sort(Pools)),

    RedisBuffers = [{logplex_queue:get(Pid, redis_url), Pid} || {_Id, Pid, worker, _Modules} <- supervisor:which_children(logplex_redis_buffer_sup)],
    {ok, Map2, Interval2} = redis_shard:generate_map_and_interval(lists:sort(RedisBuffers)),

    ets:delete_all_objects(logplex_shard_info),
    ets:insert(logplex_shard_info, {logplex_read_pool_map, {Map1, Interval1}}),
    ets:insert(logplex_shard_info, {logplex_redis_buffer_map, {Map2, Interval2}}),

    ok.

add_pools([], Acc) -> Acc;

add_pools([Url|Tail], Acc) ->
    Opts = redo_uri:parse(Url),
    {ok, Pool} = redo:start_link(undefined, Opts),
    add_pools(Tail, [{Url, Pool}|Acc]).

redis_buffer_args(Url) ->
    MaxLength =
        case os:getenv("LOGPLEX_REDIS_BUFFER_LENGTH") of
            false -> ?DEFAULT_LOGPLEX_REDIS_BUFFER_LENGTH;
            StrNum1 -> list_to_integer(StrNum1)
        end,
    NumWorkers =
        case os:getenv("LOGPLEX_REDIS_WRITERS") of
            false -> ?DEFAULT_LOGPLEX_REDIS_WRITERS;
            StrNum2 -> list_to_integer(StrNum2)
        end,
    RedisOpts = logplex_utils:parse_redis_url(Url),
    [{name, "logplex_redis_buffer"},
     {max_length, MaxLength},
     {num_workers, NumWorkers},
     {worker_sup, logplex_redis_writer_sup},
     {worker_args, [RedisOpts]},
     {dict, dict:from_list([
        {redis_url, Url}
     ])}].
