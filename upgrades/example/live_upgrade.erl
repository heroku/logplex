%% Helper fun - supervisor:childpids/1 -> [pid()].
f(ChildPids).
ChildPids = fun (Sup) ->
              [ element(2, R) || R <- supervisor:which_children(Sup) ]
            end.

%% Replace 1 with from_version, replace 2 with to_version.

f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v1" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=1~n", []);
      "v2" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=1 old_vsn=2~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless

  l(redo),
  l(redo_block),
  %% Add a child to a supervisor
  l(logplex_sup),
  l(example_child),
  supervisor:start_child(logplex_sup,
                         {example_child,
                         {module, function, [start, args]},
                          permanent, 2000, worker, [module]}),

  %% Upgrade a gen_server with a state transition

  Pid = whereis(logplex_shard),

  sys:suspend(Pid),
  l(logplex_shard),
  sys:change_code(Pid, logplex_shard, v2, undefined),
  sys:resume(Pid),

  %% Upgrade some gen_servers with a state transition

  Proxies = ChildPids(tcp_proxy_sup),
  lists:map(fun sys:suspend/1, Proxies),
  l(tcp_proxy),
  [ sys:change_code(P, tcp_proxy, v2, undefined) || P <- Proxies ],
  lists:map(fun sys:resume/1, Proxies),


  io:format(whereis(user), "at=upgrade_end cur_vsn=2~n", []),
  application:set_env(logplex, git_branch, "v2"),
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
