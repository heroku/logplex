f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v49" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=v50 old_vsn=v49~n", []);
      "v50" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=v50 old_vsn=49~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% Http drains
  Drains = [ Pid || {Pid, http} <- gproc:lookup_local_properties(drain_type)],
  [ sys:suspend(Pid) || Pid <- Drains ],
  l(logplex_http_drain),
  [ sys:change_code(Pid, logplex_http_drain, "v49", undefined)
    || Pid <- Drains],
  [ sys:resume(Pid) || Pid <- Drains ],

  io:format(whereis(user), "at=upgrade_end cur_vsn=v50~n", []),
  application:set_env(logplex, git_branch, "v50"),
  ok
end.
