f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.8" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=69.8~n", []);
      "v69.9" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=69.8 old_vsn=69.9~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  application:set_env(logplex, no_tail_warning, "Error L20 (Tail sessions forbidden) Tail sessions for this app are forbidden due to log volume."),

  %% stateless
  l(logplex_msg_buffer),
  l(logplex_tcpsyslog_drain),
  l(pobox),

  %% Stateful changes to HTTP drains -- gotta suspend, reload, and then
  %% resume all drains before going for the stateless drain buffer update
  Drains = [Pid || {Pid, http} <- gproc:lookup_local_properties(drain_type)],
  _ = [sys:suspend(Pid) || Pid <- Drains],
  l(logplex_http_drain),
  _ = [sys:change_code(Pid, logplex_http_drain, "v69.8", undefined)
       || Pid <- Drains, erlang:is_process_alive(Pid)],
  _ = [sys:resume(Pid) || Pid <- Drains],


  application:set_env(logplex, git_branch, "v69.9"),
  ok
end.

f(RollbackNode).
RollbackNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.9" ->
          io:format(whereis(user),
                    "at=rollback cur_vsn=69.9 old_vsn=69.8~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% stateless
  l(logplex_msg_buffer),
  l(logplex_tcpsyslog_drain),
  l(pobox),

  %% Stateful changes to HTTP drains -- gotta suspend, reload, and then
  %% resume all drains before going for the stateless drain buffer update
  Drains = [Pid || {Pid, http} <- gproc:lookup_local_properties(drain_type)],
  _ = [sys:suspend(Pid) || Pid <- Drains],
  l(logplex_http_drain),
  _ = [sys:change_code(Pid, logplex_http_drain, "v69.9", undefined)
       || Pid <- Drains, erlang:is_process_alive(Pid)],
  _ = [sys:resume(Pid) || Pid <- Drains],


  application:set_env(logplex, git_branch, "v69.8"),
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


