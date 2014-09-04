f(UpgradeNode).
UpgradeNode = fun () ->
    CurVsn = "v75.1",
    NextVsn = "v76",
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

    % reloads without state changes
    {module, logplex_app} = l(logplex_app),
    {module, logplex_db} = l(logplex_db),
    {module, logplex_logs_rest} = l(logplex_logs_rest),

    logplex_app:set_config(legacy_input_host, <<"east.logplex.io">>),
    logplex_app:set_config(legacy_ignored_input_host_channel_ids, [16721780]),
    logplex_app:set_config(legacy_input_ignored_agents, "log-shuttle"),
    logplex_app:set_config(legacy_input_host_gather, false),

    io:format(whereis(user), "at=upgrade_end cur_vsn=~p~n", [NextVsn]),
    ok = application:set_env(logplex, git_branch, NextVsn),
    ok
end.

f(DowngradeNode).
DowngradeNode = fun () ->
    CurVsn = "v76",
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

    % reloads without state changes
    {module, logplex_app} = l(logplex_app),
    {module, logplex_db} = l(logplex_db),
    {module, logplex_logs_rest} = l(logplex_logs_rest),

    logplex_app:cache_os_envvars(),
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

f(NextNodesAt).
NextNodesAt = fun(Vsn, N) -> lists:sublist(NodesAt(Vsn), N) end.

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
