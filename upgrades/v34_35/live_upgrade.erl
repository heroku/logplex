f(ChildPids).
ChildPids = fun (Sup) ->
              [ Pid || {_, Pid, _, _} <- supervisor:which_children(Sup),
                       erlang:is_process_alive(Pid) ]
            end.

f(UpgradeNode).
UpgradeNode = fun () ->
  io:format(whereis(user), "at=upgrade_start cur_vsn=34~n", []),
  l(logplex_worker),

  Proxies = ChildPids(tcp_proxy_sup),
  lists:map(fun sys:suspend/1, Proxies),
  l(tcp_proxy),
  [ sys:change_code(P, tcp_proxy, v34, undefined) || P <- Proxies, erlang:is_process_alive(P) ],
  lists:map(fun sys:resume/1, Proxies),


  io:format(whereis(user), "at=upgrade_end cur_vsn=35~n", []),
  length(Proxies)
end.
