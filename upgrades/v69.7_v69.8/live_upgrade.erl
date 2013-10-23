f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.7" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=69.7~n", []);
      "v69.8" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=69.7 old_vsn=69.8~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  application:set_env(logplex, no_tail_warning, "Error L20 (Tail sessions forbidden) Tail sessions for this app are forbidden due to log volume."),
  l(logplex_utils),
  l(logplex_api),

  application:set_env(logplex, git_branch, "v69.8"),
  ok
end.

f(RollbackNode).
RollbackNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.8" ->
          io:format(whereis(user),
                    "at=rollback cur_vsn=69.8 old_vsn=69.7~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,
  l(logplex_api),
  l(logplex_utils),

  application:set_env(logplex, git_branch, "v69.7"),
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

f(RollingRollback).
RollingRollback = fun (Nodes) ->
  lists:foldl(fun (N, {good, Upgraded}) ->
    case rpc:call(N, erlang, apply, [ RollbackNode, [] ]) of
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


