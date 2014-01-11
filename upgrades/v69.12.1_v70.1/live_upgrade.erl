f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.12.1" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=69.12.1~n", []);
      "v70.1" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=69.12.1 old_vsn=70.1~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  application:set_env(logplex, tcp_syslog_idle_timeout, timer:minutes(5)),
  application:set_env(logplex, tcp_syslog_idle_fuzz, 15000),
  application:set_env(logplex, http_drain_idle_timeout, timer:minutes(5)),
  application:set_env(logplex, http_drain_idle_fuzz, 15000),

  % stateful
  Drains = [ Pid || {Pid, http} <- gproc:lookup_local_properties(drain_type)],
  [ sys:suspend(Pid) || Pid <- Drains ],
  l(logplex_http_drain),
  [ sys:change_code(Pid, logplex_http_drain, "v70.1", undefined)
    || Pid <- Drains],
  [ sys:resume(Pid) || Pid <- Drains ],

  Drains = [ Pid || {Pid, tcpsyslog} <- gproc:lookup_local_properties(drain_type)],
  [ sys:suspend(Pid) || Pid <- Drains ],
  l(logplex_tcpsyslog_drain),
  [ sys:change_code(Pid, logplex_tcpsyslog_drain, "v70.1", undefined)
    || Pid <- Drains],
  [ sys:resume(Pid) || Pid <- Drains ],

  application:set_env(logplex, git_branch, "v70.1"),
  ok
end.

f(RollbackNode).
RollbackNode = fun () ->
  case logplex_app:config(git_branch) of
      "v70.1" ->
          io:format(whereis(user),
                    "at=rollback cur_vsn=70.1 old_vsn=69.12.1~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  % stateful
  Drains = [ Pid || {Pid, http} <- gproc:lookup_local_properties(drain_type)],
  [ sys:suspend(Pid) || Pid <- Drains ],
  l(logplex_http_drain),
  [ sys:change_code(Pid, logplex_http_drain, "v70.1", undefined)
    || Pid <- Drains],
  [ sys:resume(Pid) || Pid <- Drains ],

  Drains = [ Pid || {Pid, tcpsyslog} <- gproc:lookup_local_properties(drain_type)],
  [ sys:suspend(Pid) || Pid <- Drains ],
  l(logplex_tcpsyslog_drain),
  [ sys:change_code(Pid, logplex_tcpsyslog_drain, {down, "v70.1"}, undefined)
    || Pid <- Drains],
  [ sys:resume(Pid) || Pid <- Drains ],

  application:set_env(logplex, git_branch, "v69.12.1"),
  ok
end.

WIP: the stuff below is not done.

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


