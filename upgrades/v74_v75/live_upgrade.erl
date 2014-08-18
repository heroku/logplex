f(UpgradeNode).
UpgradeNode = fun () ->
    CurVsn = "v74",
    NextVsn = "v75.1",
    case logplex_app:config(git_branch) of
        CurVsn ->
            io:format(whereis(user), "at=upgrade_start cur_vsn=~p~n", [tl(CurVsn)]);
        NextVsn ->
            io:format(whereis(user),
                      "at=upgrade type=retry cur_vsn=~p old_vsn=~p~n", [tl(CurVsn), tl(NextVsn)]);
        Else ->
            io:format(whereis(user),
                      "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
            erlang:error({wrong_version, Else})
    end,

    % reload with state changes
    Pid = whereis(logplex_shard),
    sys:suspend(Pid),
    {module, logplex_shard} = l(logplex_shard),
    sys:change_code(Pid, logplex_shard, v74, undefined),
    sys:resume(Pid),

    % reloads without state changes
    {module, logplex_queue} = l(logplex_queue),
    {module, logplex_redis_writer} = l(logplex_redis_writer),

    io:format(whereis(user), "at=upgrade_end cur_vsn=~p~n", [NextVsn]),
    ok = application:set_env(logplex, git_branch, NextVsn),
    ok
end.

f(DowngradeNode).
DowngradeNode = fun () ->
    CurVsn = "v75.1",
    NextVsn = "v74",
    case logplex_app:config(git_branch) of
        CurVsn ->
            io:format(whereis(user), "at=upgrade_start cur_vsn=~p~n", [tl(CurVsn)]);
        NextVsn ->
            io:format(whereis(user),
                      "at=upgrade type=retry cur_vsn=~p old_vsn=~p~n", [tl(CurVsn), tl(NextVsn)]);
        Else ->
            io:format(whereis(user),
                      "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
            erlang:error({wrong_version, Else})
    end,

    LogplexShard = whereis(logplex_shard),

    %% Stateless
    {module, logplex_queue} = l(logplex_queue),
    {module, logplex_redis_writer} = l(logplex_redis_writer),
    {module, logplex_shard} = l(logplex_shard),

    ReadPids = logplex_shard_info:pid_list(logplex_read_pool_map),
    BufferPids = logplex_shard_info:pid_list(logplex_redis_buffer_map),
    catch exit(LogplexShard, kill),
   [ redo:shutdown(Pid) || Pid <- ReadPids ],
   [ logplex_queue:stop(Pid) || Pid <- BufferPids ],

    io:format(whereis(user), "at=upgrade_end cur_vsn=~p~n", [NextVsn]),
    ok = application:set_env(logplex, git_branch, NextVsn),
    ok
end.

f(NodeVersions).
NodeVersions = fun () ->
    lists:keysort(3,
                  [{N,
                    element(2, rpc:call(N, application, get_env, [logplex, git_branch])),
                    rpc:call(N, os, getenv, ["INSTANCE_NAME"])}
                   || N <- [node() | nodes()] ])
end.

f(NodesAt).
NodesAt = fun (Vsn) ->
    [ N || {N, V, _} <- NodeVersions(), V =:= Vsn ]
end.

f(RollingDowngrade).
RollingDowngrade = fun (Nodes) ->
  lists:foldl(fun (N, {good, Downgraded}) ->
    case rpc:call(N, erlang, apply, [ DowngradeNode, [] ]) of
      ok ->
        {good, [N | Downgraded]};
      Else ->
        {{bad, N, Else}, Downgraded}
    end;
    (N, {_, _} = Acc) -> Acc
    end,
    {good, []},
    Nodes)
end.

f(RollingUpgrade).
RollingUpgrade = fun (Nodes) ->
  lists:foldl(fun (N, {good, Upgraded}) ->
    case rpc:call(N, erlang, apply, [ UpgradeNode, [] ]) of
      ok ->
        {good, [N | Upgraded]};
      Else ->
        {{bad, N, Else}, Upgraded}
    end;
    (N, {_, _} = Acc) -> Acc
    end,
    {good, []},
    Nodes)
end.
