f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v50" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=50~n", []);
      "v51" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=51 old_vsn=50~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless
  l(logplex_drain),
  l(logplex_channel),
  l(logplex_tail),

  io:format(whereis(user), "at=upgrade_end cur_vsn=51~n", []),
  application:set_env(logplex, git_branch, "v51"),
  ok
end.
