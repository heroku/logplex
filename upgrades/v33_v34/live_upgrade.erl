f(ChildPids).
ChildPids = fun (Sup) ->
              [ element(2, R) || R <- supervisor:which_children(Sup) ]
            end.

f(UpgradeNode).
UpgradeNode = fun () ->
  io:format(whereis(user), "at=upgrade_start cur_vsn=33~n", []),
  l(nsync_callback),

  Proxies = ChildPids(tcp_proxy_sup),
  lists:map(fun sys:suspend/1, Proxies),
  l(tcp_proxy),
  [ sys:change_code(P, tcp_proxy, v33, undefined) || P <- Proxies ],
  lists:map(fun sys:resume/1, Proxies),

  Drains = [Pid ||
               {Pid, tcpsyslog} <- gproc:lookup_local_properties(drain_type)],
  lists:map(fun sys:suspend/1, Drains),
  l(logplex_tcpsyslog_drain),
  [ sys:change_code(P, logplex_tcpsyslog_drain,
                    v33, undefined) || P <- Drains ],
  lists:map(fun sys:resume/1, Drains),


  io:format(whereis(user), "at=upgrade_end cur_vsn=34~n", []),
  length(Drains ++ Proxies)
end.
