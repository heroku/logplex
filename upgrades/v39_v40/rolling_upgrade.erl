f(NodeVersions).
NodeVersions = fun () ->
                       F = fun () ->
                                   {node(), logplex_app:config(git_branch),
                                    os:getenv("INSTANCE_NAME")}
                           end,
                       {Good, _} = rpc:multicall(erlang, apply, [F, [] ]),
                       lists:keysort(2, Good)
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

