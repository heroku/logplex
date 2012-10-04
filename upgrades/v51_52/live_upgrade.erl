f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v51" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=51~n", []);
      "v52" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=51 old_vsn=51~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless
  l(redo),
  l(logplex_redis_writer),

  io:format(whereis(user), "at=upgrade_end cur_vsn=52~n", []),
  application:set_env(logplex, git_branch, "v52"),
  ok
end.
