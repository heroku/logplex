f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v66" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=66~n", []);
      "v67" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=66 old_vsn=67~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% stateless

  %% Create token index table and give away to logplex_db.
  spawn(fun () ->
                ets:new(channel_tokens,
                        [named_table, public, bag,
                         {keypos, 2}, {heir, whereis(logplex_db), undefined}])
        end),
  [Nsync] = [ Pid
              || {nsync, Pid, _, _} <- supervisor:which_children(logplex_sup)],
  sys:suspend(Nsync),
  l(logplex_token),
  Fill = fun (_F, '$end_of_table') -> ok;
             (F, {TokenInfo, Cont}) ->
                 ets:insert(channel_tokens,
                            [{token_idx, Chan, Id}
                             || {Chan, Id} <- TokenInfo,
                                is_integer(Chan), is_binary(Id)]),
                 F(F, ets:select(Cont))
         end,
  Fill(Fill,
       ets:select(tokens,
                  [{{token,'$1','$2','_'},[],[{{'$2','$1'}}]}],
                  100)),
  l(nsync_callback),
  l(logplex_channel),
  sys:resume(Nsync),

  l(logplex_cred),
  l(logplex_db),
  l(logplex_drain),
  l(logplex_session),
  l(logplex_utils),
  l(logplex_syslog_utils),

  application:set_env(logplex, git_branch, "v67"),
  ok
end.

f(NodeVersions).
NodeVersions = fun () ->
                       lists:keysort(3,
                           [ {N,
                              element(2, rpc:call(N, application, get_env, [logplex, git_branch])),
                              rpc:call(N, os, getenv, ["INSTANCE_NAME"])}
                             || N <- [node() | nodes()] ])
               end.

f(NodesAt).
NodesAt = fun (Vsn) ->
                  [ N || {N, V, _} <- NodeVersions(), V =:= Vsn ]
          end.


f(RollingUpgrade).
RollingUpgrade = fun (Nodes) ->
  lists:foldl(fun (N, {good, Upgraded}) ->
    case rpc:call(N, erlang, apply, [ UpgradeNode, [] ]) of
      ok ->
        {good, [N | Upgraded]};
      Else ->
        {{bad, N, Else}, Upgraded}
    end;
    (N, {_, _} = Acc) -> Acc
    end,
    {good, []},
    Nodes)
end.
