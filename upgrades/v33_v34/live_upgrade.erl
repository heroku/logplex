f(ChildPids).
ChildPids = fun (Sup) ->
              [ Pid || {_, Pid, _, _} <- supervisor:which_children(Sup),
                       erlang:is_process_alive(Pid) ]
            end.

f(UpgradeNode).
UpgradeNode = fun () ->
  io:format(whereis(user), "at=upgrade_start cur_vsn=33~n", []),
  l(nsync_callback),
  l(logplex_worker),
  %% Clean up bogus stats entries.
  ets:select_delete(logplex_stats, 
                    ets:fun2ms(fun ({{drain_stat, K, _, _}, 0}) when is_tuple(K) -> true end)),

  Proxies = ChildPids(tcp_proxy_sup),
  [ sys:suspend(P, timer:seconds(10)) || P <- Proxies, erlang:is_process_alive(P) ],
  l(tcp_proxy),
  [ sys:change_code(P, tcp_proxy, v33, undefined) || P <- Proxies, erlang:is_process_alive(P) ],
  lists:map(fun sys:resume/1, Proxies),

  Drains = [Pid ||
               {Pid, tcpsyslog} <- gproc:lookup_local_properties(drain_type)],
  [ sys:suspend(P, timer:seconds(10)) || P <- Drains, erlang:is_process_alive(P) ],
  l(logplex_tcpsyslog_drain),
  [ sys:change_code(P, logplex_tcpsyslog_drain,
                    v33, undefined) || P <- Drains, erlang:is_process_alive(P) ],
  lists:map(fun sys:resume/1, Drains),


  io:format(whereis(user), "at=upgrade_end cur_vsn=34~n", []),
  length(Drains ++ Proxies)
end.
