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

-export([lookup/3
         ,urls/0
         ,redis_sort/1]).

%% Redis Migration API
-export([prepare_shard_urls/1,
         prepare_url_update/1,
         attempt_to_commit_url_update/0,
         make_update_permanent/0
        ]).

-include("logplex.hrl").
-include("logplex_logging.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

-define(NEW_READ_MAP, new_logplex_read_pool_map).
-define(CURRENT_READ_MAP, logplex_read_pool_map).
-define(BACKUP_READ_MAP, backup_logplex_read_pool_map).
-define(NEW_WRITE_MAP, new_logplex_redis_buffer_map).
-define(CURRENT_WRITE_MAP, logplex_redis_buffer_map).
-define(BACKUP_WRITE_MAP, backup_logplex_redis_buffer_map).

-record(state, {}).

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

-spec urls() -> [ Url::string() ].
urls() ->
    redis_sort(logplex_app:config(logplex_shard_urls)).

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
    ?INFO("at=init", []),
    Urls = urls(),

    erlang:process_flag(trap_exit, true),

    ets:new(logplex_shard_info, [protected, set, named_table]),

    populate_info_table(Urls),

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
handle_call({commit, new_shard_info}, _From, State) ->
    backup_shard_info(),
    try
        make_new_shard_info_permanent(),
        {reply, ok, State}
    catch C:E ->
            revert_shard_info(),
            {reply, {error, {C, E}}, State}
    end;

handle_call({abort, new_shard_info}, _From, State) ->
    try
        true = have_backup(),
        {reply, revert_shard_info(), State}
    catch C:E ->
            {reply, {error, {C, E}}, State}
    end;

handle_call({prepare, {new_shard_info, NewShardInfo}}, _From, State) ->
    {reply, prepare_new_shard_info(NewShardInfo), State};

handle_call({make_permanent, new_shard_info}, _From, State) ->
    try
        [ stop_buffer(B)
          || B <- logplex_shard_info:pid_list(?BACKUP_WRITE_MAP) ],
        logplex_shard_info:delete(?BACKUP_WRITE_MAP),
        logplex_shard_info:delete(?NEW_WRITE_MAP),
        [ stop_pool(P)
          || P <- logplex_shard_info:pid_list(?BACKUP_READ_MAP) ],
        logplex_shard_info:delete(?BACKUP_READ_MAP),
        logplex_shard_info:delete(?NEW_READ_MAP),
        {reply, ok, State}
    catch
        C:E ->
            {reply, {error, {C,E}}, State}
    end;

handle_call(consistency_check, _From, State = #state{}) ->
    {reply, try consistent(urls())
            catch C:E ->
                    {error, {C, E, erlang:get_stacktrace()}}
            end, State};

handle_call({state_apply, F}, _From, State)
  when is_function(F, 1) ->
    case catch F(State) of
        NewState = #state{} ->
            {reply, ok, NewState};
        Else ->
            {reply, {error, Else}, State}
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) ->
    ?INFO("child=~p exit_reason=~p", [Pid, Reason]),
    handle_child_death(Pid),
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
populate_info_table(Urls) ->
    populate_info_table(?CURRENT_READ_MAP, ?CURRENT_WRITE_MAP, Urls).

populate_info_table(ReadMap, WriteMap, Urls) ->
    %% Populate Read pool
    ReadPools = [ {Url, add_pool(Url)} || Url <- redis_sort(Urls)],
    {ok, Map1, Interval1} =
        redis_shard:generate_map_and_interval(ReadPools),
    logplex_shard_info:save(ReadMap, Map1, Interval1),

    %% Populate write pool
    WritePools = [ {Url, add_buffer(Url)}
                   || Url <- redis_sort(Urls)],
    {ok, Map2, Interval2} =
        redis_shard:generate_map_and_interval(WritePools),

    logplex_shard_info:save(WriteMap, Map2, Interval2),
    ok.

add_pool(Url) ->
    Opts = parse_redis_uri(Url),
    {ok, Pool} = redo:start_link(undefined, Opts),
    Pool.

add_buffer(Url) ->
    Opts = redis_buffer_opts(Url),
    {ok, Buffer} = logplex_queue:start_link(Opts),
    Buffer.

redis_buffer_opts(Url) ->
    MaxLength = logplex_utils:to_int(logplex_app:config(redis_buffer_length)),
    NumWorkers = logplex_utils:to_int(logplex_app:config(redis_writers)),
    RedisOpts = logplex_utils:parse_redis_url(Url),
    [{name, "logplex_redis_buffer"},
     {max_length, MaxLength},
     {num_workers, NumWorkers},
     {worker_sup, logplex_redis_writer_sup},
     {worker_args, [RedisOpts]},
     {dict, dict:from_list([
        {redis_url, Url}
     ])}].

handle_child_death(Pid) ->
    case logplex_shard_info:pid_info(Pid) of
        {logplex_read_pool_map, {{Shard, {Url, Pid}}, Map, V}} ->
            NewPid = add_pool(Url),
            NewMap = dict:store(Shard, {Url, NewPid}, Map),
            logplex_shard_info:save(logplex_read_pool_map, NewMap, V),
            ?INFO("at=read_pool_restart oldpid=~p newpid=~p",
                  [Pid, NewPid]);
        {logplex_redis_buffer_map, {{Shard, {Url, Pid}}, Map, V}} ->
            NewPid = add_buffer(Url),
            NewMap = dict:store(Shard, {Url, NewPid}, Map),
            logplex_shard_info:save(logplex_redis_buffer_map, NewMap, V),
            ?INFO("at=write_pool_restart oldpid=~p newpid=~p",
                  [Pid, NewPid]);
        undefined ->
            ?WARN("at=trap_exit err=unknown_pid pid=~p", [Pid])
    end,
    ok.

consistent(URLs) ->
    {Map, _V, _TS}
        = logplex_shard_info:read(logplex_read_pool_map),
    FlatMap = [{S, U, P} || {S, {U, P}} <- dict:to_list(Map)],
    true = length(URLs) =:= length(FlatMap),
    Correct = [true || {_S,U,P} <- FlatMap,
                       is_process_alive(P),
                       lists:member(U,URLs)],
    true = length(Correct) =:= length(URLs),
    consistent.

%%--------------------------------------------------------------------
%%% Redis cluster move code
%%--------------------------------------------------------------------

%% Attempt to create new shard maps with new redo processes. Catch
%% errors and destroy any created processes.
prepare_new_shard_info(NewShardInfo) ->
    {links, OldLinks} = process_info(self(), links),
    try
        new_shard_info(NewShardInfo)
    catch
        C:E ->
            {links, NewLinks} = process_info(self(), links),
            %% Clean up any new processes we started
            [ erlang:exit(P, kill)
              || P <- (NewLinks -- OldLinks),
                 P > self()],
            delete_new_shard_info(),
            {error, {C,E}}
    end.

delete_new_shard_info() ->
    logplex_shard_info:delete(?NEW_READ_MAP),
    logplex_shard_info:delete(?NEW_WRITE_MAP),
    ok.

backup_shard_info() ->
    logplex_shard_info:copy(?CURRENT_WRITE_MAP, ?BACKUP_WRITE_MAP),
    logplex_shard_info:copy(?CURRENT_READ_MAP, ?BACKUP_READ_MAP),
    ok.

have_backup() ->
    logplex_shard_info:read(?BACKUP_WRITE_MAP) =/= no_such_key
        andalso logplex_shard_info:read(?BACKUP_READ_MAP) =/= no_such_key.

revert_shard_info() ->
    logplex_shard_info:copy(?BACKUP_WRITE_MAP, ?CURRENT_WRITE_MAP),
    logplex_shard_info:copy(?BACKUP_READ_MAP, ?CURRENT_READ_MAP),
    ok.

make_new_shard_info_permanent() ->
    logplex_shard_info:copy(?NEW_WRITE_MAP, ?CURRENT_WRITE_MAP),
    logplex_shard_info:copy(?NEW_READ_MAP, ?CURRENT_READ_MAP),
    ok.

new_shard_info(NewUrls) ->
    populate_info_table(?NEW_READ_MAP, ?NEW_WRITE_MAP,
                        NewUrls),
    ok.


%% logplex_logs_redis / logplex_shard online replacement guide:
%% NewShardInfo = prepare_new_urls(logplex_shard:redis_sort(string:tokens(NEW_LOGPLEX_SHARD_URLS, ",")).
%% Cluster = [node() | nodes()],
%% prepare_url_update(Cluster, NewShardInfo).
%% Check 'logplex_shard_info:read(new_logplex_read_pool_map).' looks sensible.
%% If all are good:
%%   attempt_to_commit_url_update(Cluster).
%% If that succeeds:
%%   make_update_permanent(Cluster).
%%
%% If anything goes wrong:
%%   abort_url_update(Cluster).

-spec prepare_shard_urls(string()) -> [string()]|[].
prepare_shard_urls(ShardUrls) ->
    Shards = string:tokens(ShardUrls, ","),
    logplex_shard:redis_sort(Shards).

-type shards_info() :: [string()].
-spec prepare_url_update(shards_info()) -> good|{error, any()}.
prepare_url_update(NewShardInfo) ->
    case gen_server:call(?MODULE, {prepare, {new_shard_info, NewShardInfo}}) of
        ok ->
            good;
        Err ->
            {failed, Err}
    end.

-spec attempt_to_commit_url_update() -> good|{failed, term()}.
attempt_to_commit_url_update() ->
    case gen_server:call(?MODULE, {commit, new_shard_info}) of
        ok ->
            good;
        Err ->
            abort_url_update(),
            {failed, Err}
    end.

-spec make_update_permanent() -> shard_info_updated|{failed, term()}.
make_update_permanent() ->
    case gen_server:call(?MODULE, {make_permanent, new_shard_info}) of
        ok ->
            shard_info_updated;
        {error, Err} ->
            {failed, Err}
    end.

abort_url_update() ->
    gen_server:call(?MODULE, {abort, new_shard_info}).

stop_pool(Pid) ->
    redo:shutdown(Pid).

stop_buffer(Pid) ->
    logplex_queue:stop(Pid).

parse_redis_uri(Url) when is_list(Url) ->
    case ex_uri:decode(Url) of
        {ok, Uri = #ex_uri{scheme="redis",
                           authority=#ex_uri_authority{host=Host,
                                                       port=Port}},
         _} ->
            lists:append([ [{url, Url},
                            {host, Host},
                            {port, case Port of
                                       undefined -> 6379;
                                       _ -> Port
                                   end} ],
                           parse_redis_uri_fragkey(Uri),
                           parse_redis_uri_pass(Uri),
                           parse_redis_uri_db(Uri) ]);
        _ ->
            {error, bad_uri}
    end.

parse_redis_uri_fragkey(#ex_uri{fragment = Frag}) when Frag =/= undefined ->
    [{sortkey, Frag}];
parse_redis_uri_fragkey(_) -> [].

parse_redis_uri_pass(#ex_uri{authority=Auth}) when Auth =/= undefined ->
    case Auth of
        #ex_uri_authority{userinfo=Pass} when Pass =/= undefined ->
            [{pass, Pass}];
        _ -> []
    end;
parse_redis_uri_pass(_) -> [].

parse_redis_uri_db(#ex_uri{path=Path}) when Path =/= undefined ->
    case iolist_to_binary(Path) of
        <<"/", DB/binary>> when DB =/= <<"">> ->
            [{db, binary_to_list(DB)}];
        _ ->
            []
    end;
parse_redis_uri_db(_) -> [].

redis_sort(Urls) ->
    Parsed = lists:map(fun parse_redis_uri/1, Urls),
    [ proplists:get_value(url, Server)
      || Server <- lists:sort(fun sortfun/2, Parsed) ].

sortfun(A, B) ->
    sortkey(A) =< sortkey(B).

sortkey(Info) ->
    case proplists:get_value(sortkey, Info) of
        undefined ->
            proplists:get_value(url, Info);
        Key -> Key
    end.
