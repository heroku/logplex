f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v39" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=39~n", []);
      "v40" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=40 old_vsn=39~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

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
