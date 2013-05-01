f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v65" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=65~n", []);
      "v66" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=65 old_vsn=66~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% stateless, internal
  l(logplex_channel),
  l(logplex_drain),
  l(logplex_mon_sup),
  l(logplex_mon_serv),
  %% start a supervisor
  supervisor:start_child(logplex_sup,
                         {logplex_monitor, {logplex_mon_sup, start_link, []},
                          permanent, 8000, supervisor, [logplex_mon_sup]}),

  io:format(whereis(user), "at=upgrade_end cur_vsn=66~n", []),
  application:set_env(logplex, git_branch, "v66"),
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
