f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v69.1" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=69.1~n", []);
      "v69.2" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=69.1 old_vsn=69.2~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Start Lager with the correct handlers
  code:add_pathz("deps/lager/ebin"),
  code:add_pathz("deps/goldrush/ebin"),
  code:add_pathz("deps/recon/ebin"),
  application:load(lager),
  application:set_env(lager,
                      handlers,
                      [{lager_console_backend,
                        [info, {lager_default_formatter, [message, "\n"]}]}
                      ]),
  application:set_env(lager, crash_log, undefined),
  application:set_env(lager, async_threshold, 100),
  application:set_env(lager, async_threshold_window, 20),
  application:set_env(lager, error_logger_hwm, 500),
  logplex_app:a_start(lager, permanent),
  %% Potentially most catastrophic upgrade -- should be doable at run-time
  %% and self-upgrade
  l(logplex_msg_buffer),
  %% Hibernation reloads
  l(logplex_drain_buffer),
  l(logplex_http_drain),
  l(logplex_tcpsyslog_drain),
  %% Reload all the other modules for logging changes :(
  %% We'll have a bunch of dependencies that still use io:format/2 but that's
  %% fine.
  [l(Mod) || Mod <- [logplex_api, logplex_app, logplex_channel, logplex_cred,
                     logplex_db, logplex_http_client, logplex_leak,
                     logplex_logs_rest, logplex_mon_serv, logplex_queue,
                     logplex_realtime, logplex_redis_writer, logplex_shard,
                     logplex_tail_buffer, logplex_udpsyslog_drain,
                     logplex_worker, nsync_callback, tcp_proxy]],

  application:set_env(logplex, git_branch, "v69.2"),
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


