f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v63" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=63~n", []);
      "v64" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=63 old_vsn=64~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% stateless, internal
  l(logplex_api),
  l(logplex_tail_buffer),

  io:format(whereis(user), "at=upgrade_end cur_vsn=64~n", []),
  application:set_env(logplex, git_branch, "v64"),
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
