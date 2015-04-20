f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v71" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=71~n", []);
      "v72" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=72 old_vsn=71~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless
  l(logplex_api),
  l(logplex_tcpsyslog_drain),
  application:set_env(logplex, no_redis_warning,
                      "Warning: log tail history discarded due to excessive volume."),

  io:format(whereis(user), "at=upgrade_end cur_vsn=72~n", []),
  application:set_env(logplex, git_branch, "v72"),
  ok
end.
