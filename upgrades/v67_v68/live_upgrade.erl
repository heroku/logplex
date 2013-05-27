f(UpgradeNode).
UpgradeNode = fun () ->
  case logplex_app:config(git_branch) of
      "v67" ->
          io:format(whereis(user), "at=upgrade_start cur_vsn=67~n", []);
      "v68" ->
          io:format(whereis(user),
                    "at=upgrade type=retry cur_vsn=67 old_vsn=68~n", []);
      Else ->
          io:format(whereis(user),
                    "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
          erlang:error({wrong_version, Else})
  end,

  %% stateless
  l(logplex_channel),
  l(logplex_drain_buffer),
  l(logplex_http_drain),
  %% stateful
  %% extract TCP syslog drains
  Pids = [Pid || {_,Pid,_,[logplex_tcpsyslog_drain]} <- supervisor:which_children(logplex_drain_sup)],
  %% We freeze 'em up before sorting them. At heroku, sorting by message queue
  %% length takes roughly 500ms in production. It is preferable to freezing before
  %% sorting as it allows to reload the module in a way that won't crash processes
  %% caught in a kind of race condition between the events.
  io:format(whereis(user), "at=upgrade_suspend cur_vsn=67~n", []),
  [sys:suspend(Pid) || Pid <- Pids],
  l(logplex_tcpsyslog_drain),
  Sorted = [Pid || {_,Pid} <- lists:reverse(lists:sort([{process_info(Pid,message_queue_len),Pid} || Pid <- Pids]))],
  io:format(whereis(user), "at=upgrade_change_code cur_vsn=67~n", []),
  [sys:change_code(Pid, logplex_tcpsyslog_drain, v67, [])
     || Pid <- Sorted],
  %% unfreeze the procs. We'll probably have a lot of garbage report for
  %% unexpected messages based on the messages received while frozen, but too bad.
  %% We're still losing fewer than if we restarted.
  io:format(whereis(user), "at=upgrade_resume cur_vsn=67~n", []),
  [sys:resume(Pid) || Pid <- Pids],
  %% done
  io:format(whereis(user), "at=upgrade_end cur_vsn=68~n", []),
  application:set_env(logplex, git_branch, "v68"),
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

