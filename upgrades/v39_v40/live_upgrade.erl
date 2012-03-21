f(UpgradeNode).
UpgradeNode = fun () ->
  io:format(whereis(user), "at=upgrade_start cur_vsn=38~n", []),

  %% Stateless
  l(logplex_api),
  l(logplex_syslog_utils),
  l(logplex_utils),

  %% Add ehmon app
  code:add_pathz("deps/ehmon/ebin"),
  application:start(ehmon),

  io:format(whereis(user), "at=upgrade_end cur_vsn=40~n", []),
  application:set_env(logplex, git_branch, "v40")
end.
