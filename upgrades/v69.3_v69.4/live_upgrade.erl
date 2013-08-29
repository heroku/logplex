f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.3" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=69.3~n", []);
      "v69.4" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=69.3 old_vsn=69.4~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  application:set_env(redgrid, redis_url, os:getenv("LOGPLEX_REDGRID_REDIS_URL")),
  supervisor:terminate_child(logplex_sup, redgrid),
  supervisor:restart_child(logplex_sup, redgrid),
  l(logplex_app),

  application:set_env(logplex, git_branch, "v69.4"),
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
