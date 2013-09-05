f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.4" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=69.4~n", []);
      "v69.5" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=69.4 old_vsn=69.5~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Start batchio
  code:add_pathz("deps/batchio/ebin"),
  code:add_pathz("deps/pobox/ebin"),
  ok = application:start(pobox), %% we depend on pobox but it's not running.
  ok = application:start(batchio),
  %% Reload all the modules for logging changes :(
  %% We'll have a bunch of dependencies that still use io:format/2 but that's
  %% fine.
  [l(Mod) || Mod <- [logplex_api, logplex_app, logplex_channel, logplex_cred,
                     logplex_db, logplex_http_client, logplex_leak, logplex_msg_buffer,
                     logplex_logs_rest, logplex_mon_serv, logplex_queue,
                     logplex_realtime, logplex_redis_writer, logplex_shard,
                     logplex_tail_buffer, logplex_udpsyslog_drain, logplex_drain_buffer,
                     logplex_http_drain, logplex_tcpsyslog_drain,
                     logplex_worker, nsync_callback, tcp_proxy]],

  application:set_env(logplex, git_branch, "v69.5"),
  ok
end.

f(RollbackNode).
RollbackNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.5" ->
          io:format(whereis(user),
                    "at=rollback cur_vsn=69.5 old_vsn=69.4~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Reload all the modules for logging changes (should now use lager) :(
  %% We'll have a bunch of dependencies that still use io:format/2 but that's
  %% fine.
  [l(Mod) || Mod <- [logplex_api, logplex_app, logplex_channel, logplex_cred,
                     logplex_db, logplex_http_client, logplex_leak, logplex_msg_buffer,
                     logplex_logs_rest, logplex_mon_serv, logplex_queue,
                     logplex_realtime, logplex_redis_writer, logplex_shard,
                     logplex_tail_buffer, logplex_udpsyslog_drain, logplex_drain_buffer,
                     logplex_http_drain, logplex_tcpsyslog_drain,
                     logplex_worker, nsync_callback, tcp_proxy]],
  %% Stop batchio -- in case of failure it should fall back to io:format/2
  application:stop(batchio),
  application:stop(pobox),

  application:set_env(logplex, git_branch, "v69.4"),
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


