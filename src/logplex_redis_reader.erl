-module(logplex_redis_reader).

-behaviour(gen_server).

-export([start_pool/2]).
-export([stop_pool/1]).
-export([cmd/2]).

-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).

-include("logplex_logging.hrl").

-record(state, {
          redo_pid :: undefined | pid(),
          redis_opts :: proplists:proplist(),
          backoff :: backoff:backoff()
         }).

start_pool(PoolId, RedisOpts) ->
    %% The pool id is the identifier for the supervisor. It's not used anywhere else.
    PoolOpts = [{worker_module, ?MODULE},
                {size, 1}, %% number of redis connections in the pool
                {max_overflow, 2} %% maximum number of workers created if pool is empty
               ],
    Spec = poolboy:child_spec(PoolId, PoolOpts, RedisOpts),
    logplex_redis_reader_sup:start_child(Spec).

stop_pool(PoolPid) ->
    logplex_redis_reader_sup:remove_child(PoolPid).

cmd(PoolPid, Cmd) ->
    poolboy:transaction(PoolPid, fun(Worker) ->
                                      gen_server:call(Worker, {cmd, Cmd})
                              end).

start_link(RedisOpts) when is_list(RedisOpts) ->
    gen_server:start_link(?MODULE, RedisOpts, []).

init(RedisOpts) ->
    process_flag(trap_exit, true),
    Cooldown = 100, %% ms
    MaxTime = timer:seconds(300),
    Backoff = backoff:init(Cooldown, MaxTime),
    State = #state{ redis_opts = RedisOpts,
                    backoff = Backoff },
    self() ! connect,
    {ok, State}.

handle_call({cmd, Cmd}, _From, #state{ redo_pid = Pid } = State) ->
    Result = redo:cmd(Pid, Cmd),
    {reply, Result, State};
handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

handle_cast(Msg, State) ->
    ?INFO("at=handle_cast unexpected=~p", [Msg]),
    {noreply, State}.

handle_info(connect, #state{ redis_opts = RedisOpts,
                             backoff = Backoff } = State) ->
    case redo:start_link(undefined, RedisOpts) of
        {ok, Pid} ->
            {_, NewBackoff} = backoff:succeed(Backoff),
            NewState = State#state{ redo_pid = Pid,
                                    backoff = NewBackoff },
            {noreply, NewState};
        {error, Reason} ->
            Host = proplists:get_value(host, RedisOpts),
            ?WARN("warn=failed_to_connect_to_redis error=~p redis_host=~p", [Reason, Host]),
            {noreply, State}
    end;
handle_info({'EXIT', Pid, _Reason}, #state{ redo_pid = Pid,
                                           backoff = Backoff } = State) ->
    {Wait, NewBackoff} = backoff:fail(Backoff),
    erlang:send_after(Wait, self(), connect),
    NewState = State#state{ backoff = NewBackoff },
    {noreply, NewState};
handle_info(Info, State) ->
    ?INFO("at=handle_info unexpected=~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{ redo_pid = Pid }) ->
    try
        unlink(Pid),
        redo:shutdown(Pid)
    catch _:_ ->
              ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
