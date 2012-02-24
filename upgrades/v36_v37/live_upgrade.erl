f(UpgradeNode).
UpgradeNode = fun () ->
  io:format(whereis(user), "at=upgrade_start cur_vsn=36~n", []),
  l(logplex_tcpsyslog_drain),
  l(logplex_tcpsyslog2_drain),
  l(logplex_drain),
  l(logplex_api),
  l(logplex_drain_buffer),
  l(logplex_drain_stats),
  l(logplex_sup),
  l(logplex_syslog_utils),
  l(logplex_tcpsyslog_drain),
  l(logplex_tcpsyslog2_drain),

  Drains = supervisor:which_children(logplex_drain_sup),

  [ try
        case logplex_drain:lookup(Id) of
            {drain,Id,Ch,Tok,
             _,Host,Port,true} ->
                logplex_drain:stop(Id),
                {Id, logplex_drain:start(tcpsyslog2, Id,
                                         [Ch, Id, Tok, Host, Port])};
            _ ->
                {ignored, Id}
        end
    catch
        Class:Err -> {Id, Class, Err}
    end
    ||
      {Id, Pid, _, _} <- Drains],

  io:format(whereis(user), "at=upgrade_end cur_vsn=37~n", []),
  length(Drains)
end.
