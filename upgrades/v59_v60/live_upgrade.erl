f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v59" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=59~n", []);
      "v60" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=59 old_vsn=60~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless

  application:set_env(logplex, metrics_channel_id, 14209704),
  application:set_env(logplex, metrics_namespace, "us-east-1"),

  l(logplex_app),
  l(logplex_drain),
  l(logplex_realtime),
  l(nsync_callback),

  io:format(whereis(user), "at=upgrade_end cur_vsn=60~n", []),
  application:set_env(logplex, git_branch, "v60"),
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
