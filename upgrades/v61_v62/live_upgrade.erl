f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v61" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=61~n", []);
      "v62" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=61 old_vsn=62~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% stateless -- didn't change in this release, but might not have been
  %% reloaded in older ones
  l(logplex_msg_buffer),

  %% Stateful changes to HTTP drains -- gotta suspend, reload, and then
  %% resume all drains before going for the stateless drain buffer update
  Drains = [Pid || {Pid, http} <- gproc:lookup_local_properties(drain_type)],
  _ = [sys:suspend(Pid) || Pid <- Drains],
  l(logplex_http_drain),
  _ = [sys:change_code(Pid, logplex_http_drain, v61, undefined)
       || Pid <- Drains, erlang:is_process_alive(Pid)],
  _ = [sys:resume(Pid) || Pid <- Drains],

  %% stateless
  l(logplex_drain_buffer),

  io:format(whereis(user), "at=upgrade_end cur_vsn=62~n", []),
  application:set_env(logplex, git_branch, "v62"),
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
