f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.12" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=69.12~n", []);
      "v69.12.1" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=69.12.1 old_vsn=69.12~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,
  %% Find all the buffers
  Buffers = 
    case logplex_shard_info:read(logplex_redis_buffer_map) of
        {Map, _, _} ->
            [ Pid || { _, {_, Pid}} <- dict:to_list(Map) ];
        _ ->
            []
    end,
    Buffers2 = [ Pid || {_, Pid, _, _} <- supervisor:which_children(logplex_read_queue_sup) ] ++ Buffers,
    _ = [ sys:suspend(Pid, 30000) || Pid <- Buffers2 ],
    l(logplex_queue),
    _ = [ sys:change_code(Pid, logplex_queue, "v69.12.1", undefined, 30000)
       || Pid <- Buffers2, erlang:is_process_alive(Pid) ],
    _ = [ sys:resume(Pid, 30000) || Pid <- Buffers2, erlang:is_process_alive(Pid) ],
  application:set_env(logplex, git_branch, "v69.12.1"),
  ok
end.

f(RollbackNode).
RollbackNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.12.1" ->
          io:format(whereis(user),
                    "at=rollback cur_vsn=69.12.1 old_vsn=69.12~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Find all the buffers
  Buffers = 
    case logplex_shard_info:read(logplex_redis_buffer_map) of
        {Map, _, _} ->
            [ Pid || { _, {_, Pid}} <- dict:to_list(Map) ];
        _ ->
            []
    end,
    Buffers2 = [ Pid || {_, Pid, _, _} <- supervisor:which_children(logplex_read_queue_sup) ] ++ Buffers,
    _ = [ sys:suspend(Pid, 30000) || Pid <- Buffers2 ],
    _ = [ sys:change_code(Pid, logplex_queue, "v69.12", undefined, 30000)
       || Pid <- Buffers2, erlang:is_process_alive(Pid) ],
    l(logplex_queue),
    _ = [ sys:resume(Pid, 30000) || Pid <- Buffers2, erlang:is_process_alive(Pid) ],

  application:set_env(logplex, git_branch, "v69.12"),
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
