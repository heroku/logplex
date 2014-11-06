f(UpgradeNode).
UpgradeNode = fun (Config) ->
    CurVsn = "v75",
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


    ok = application:set_env(ehmon, shh_connection, proplists:get_value(shh_connection, Config)),
    ok = application:set_env(ehmon, report_mf, {ehmon_shh_report, send_report}),
    ok = application:stop(ehmon),

    %% stateless, internal
    {module, ehmon_sup} = l(ehmon_sup),
    {module, ehmon_shh_report} = l(ehmon_shh_report),
    {module, shh_drv} = l(shh_drv),
    {module, ehmon_app} = l(ehmon_app),
    {module, ehmon} = l(ehmon),
    {module, ehmon_report_srv} = l(ehmon_report_srv),

    ok = application:start(ehmon),
    ok
end.

f(NodeVersions).
NodeVersions = fun () ->
                       lists:keysort(3,
                           [ {N,
                              element(2, rpc:call(N, application, get_env, [logplex, git_branch])),
                              rpc:call(N, os, getenv, ["INSTANCE_NAME"])}
                             || N <- [node() | nodes()] ])
               end.

f(NodesAt).
NodesAt = fun (Vsn) ->
                  [ N || {N, V, _} <- NodeVersions(), V =:= Vsn ]
          end.


f(RollingUpgrade).
RollingUpgrade = fun (Nodes, Config) ->
  lists:foldl(fun (N, {good, Upgraded}) ->
    case rpc:call(N, erlang, apply, [ UpgradeNode, [Config] ]) of
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
