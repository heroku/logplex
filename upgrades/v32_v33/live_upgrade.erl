f(ChildPids).
ChildPids = fun (Sup) ->
              [ element(2, R) || R <- supervisor:which_children(Sup) ]
            end.

f(UpgradeNode).
UpgradeNode = fun () ->
  io:format(whereis(user), "at=upgrade_start~n", []),
  %% New modules
  l(logplex_shard_info),
  l(logplex_logging),

  %% Logging changes
  l(logplex_api),
  l(logplex_drain),
  l(logplex_app),
  l(logplex_queue),
  l(logplex_realtime),
  l(logplex_redis_writer),
  l(logplex_utils),

  l(logplex_stats),
  l(logplex_tcpsyslog_drain),
  
  %% logplex_shard_info stuff
  l(logplex_channel),
  l(logplex_worker),

  Pid = whereis(logplex_shard),

  sys:suspend(Pid),
  l(logplex_shard),
  sys:change_code(Pid, logplex_shard, v32, undefined),
  sys:resume(logplex_shard),

  Proxies = ChildPids(tcp_proxy_sup),
  lists:map(fun sys:suspend/1, Proxies),
  l(tcp_proxy),
  [ sys:change_code(P, tcp_proxy, v32, undefined) || P <- Proxies ],
  lists:map(fun sys:resume/1, Proxies),

  io:format(whereis(user), "at=upgrade_end~n", []),
  gen_server:call(Pid, consistency_check)
end.
