f(UpgradeNode).
UpgradeNode = fun () ->
    OldVsn = "v91",
    NextVsn = "v92",
    case logplex_app:config(git_branch) of
        OldVsn ->
            io:format(whereis(user), "at=upgrade_start cur_vsn=~p~n", [tl(OldVsn)]);
        NextVsn ->
            io:format(whereis(user),
                      "at=upgrade type=retry cur_vsn=~p old_vsn=~p~n", [tl(OldVsn), tl(NextVsn)]);
        Else ->
            io:format(whereis(user),
                      "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
            erlang:error({wrong_version, Else})
    end,

    % load the new version of the module
    l(logplex_api),
    l(logplex_app),
    l(logplex_http_drain),
    l(logplex_stats),
    l(logplex_tcpsyslog_drain),
    l(logplex_tlssyslog_drain),

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
