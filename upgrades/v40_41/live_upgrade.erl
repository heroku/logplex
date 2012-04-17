f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v40" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=39~n", []);
      "v41" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=40 old_vsn=39~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Stateless
  l(http_uri_r15b),
  l(logplex_drain),
  l(logplex_channel),
  l(logplex_api),

  io:format(whereis(user), "at=upgrade_end cur_vsn=41~n", []),
  application:set_env(logplex, git_branch, "v41"),
  ok
end.
