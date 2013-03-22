f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v57" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=57~n", []);
      "v58" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=57 old_vsn=58~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless

  l(redo),
  l(redo_block),
  l(logplex_sup),
  supervisor:start_child(logplex_sup,
                         {config_redis_block,
                         {redo_block, start_link, [config_block, redo_uri:parse(logplex_app:config(config_redis_url))]},
                          permanent, 2000, worker, [redo_block]}),
  l(redis_helper),

  io:format(whereis(user), "at=upgrade_end cur_vsn=58~n", []),
  application:set_env(logplex, git_branch, "v58"),
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
