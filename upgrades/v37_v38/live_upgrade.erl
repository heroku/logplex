f(UpgradeNode).
UpgradeNode = fun () ->
  io:format(whereis(user), "at=upgrade_start cur_vsn=37~n", []),
  %% Tail changes:
  %% Stateless
  l(logplex_tail_buffer),
  l(logplex_drain_buffer),
  l(logplex_syslog_utils),

  %% No state change
  l(logplex_tcpsyslog_drain2),
  l(logplex_stats),
  l(logplex_api),

  %% Shard changes
  l(logplex_shard_info),

  sys:suspend(logplex_shard),
  l(logplex_shard),
  sys:change_code(logplex_shard, logplex_shard, v37, undefined),
  sys:resume(logplex_shard),

  io:format(whereis(user), "at=upgrade_end cur_vsn=38~n", []),
  application:set_env(logplex, git_branch, "v38")
end.
