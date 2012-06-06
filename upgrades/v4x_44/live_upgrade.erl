f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "buildpackization" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=v44 old_vsn=buildpackization~n", []);
      "v43" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=v44 old_vsn=43~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless
  l(logplex_shard),
  l(logplex_api),

  io:format(whereis(user), "at=upgrade_end cur_vsn=41~n", []),
  application:set_env(logplex, git_branch, "v44"),
  ok
end.
