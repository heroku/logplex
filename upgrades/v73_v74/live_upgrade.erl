f(UpgradeNode).
UpgradeNode = fun () ->
    CurVsn = "v73",
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

    %% Stateless
    {module, logplex_app} = l(logplex_app),
    {module, logplex_logs_rest} = l(logplex_logs_rest),

    case os:get_env("LOGPLEX_API_ENDPOINT_URL") of
        false ->
            erlang:error({missing_osvar, "LOGPLEX_API_ENDPOINT_URL"});
        Value ->
            application:set_env(logplex, api_endpoint_url, Value)
    end,

    cowboy:set_env(logplex_logs_rest, dispatch, logplex_logs_rest:dispatch()),

    io:format(whereis(user), "at=upgrade_end cur_vsn=~p~n", [NextVsn]),
    ok = application:set_env(logplex, git_branch, NextVsn),
    ok
end.

f(DowngradeNode).
DowngradeNode = fun () ->
    CurVsn = "v74",
    NextVsn = "v73",
    case logplex_app:config(git_branch) of
        CurVsn ->
            io:format(whereis(user), "at=downgrade_start cur_vsn=~p~n", [tl(CurVsn)]);
        NextVsn ->
            io:format(whereis(user),
                      "at=upgrade type=retry cur_vsn=~p old_vsn=~p~n", [tl(CurVsn), tl(NextVsn)]);
        Else ->
            io:format(whereis(user),
                      "at=downgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
            erlang:error({wrong_version, Else})
    end,

    %% Stateless
    {module, logplex_app} = l(logplex_app),
    {module, logplex_logs_rest} = l(logplex_logs_rest),

    application:unset_env(logplex, api_endpoint_url)
    Dispatch = cowboy_router:compile([{'_',
                            [{<<"/healthcheck">>, logplex_logs_rest, [healthcheck]},
                             {<<"/logs">>, logplex_logs_rest, [logs]}]}]),
    cowboy:set_env(logplex_logs_rest, dispatch, Dispatch),

    io:format(whereis(user), "at=downgrade_end cur_vsn=~p~n", [NextVsn]),
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
