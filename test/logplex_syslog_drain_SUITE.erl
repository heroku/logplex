-module(logplex_syslog_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-compile(export_all).

-include("logplex_test_helpers.hrl").

all() -> [{group, tcp_syslog},
          {group, tls_syslog}].

groups() ->
  [{tcp_syslog, [], [{group, full_suite}]},
   {tls_syslog, [], [{group, full_suite}]},
   {full_suite, [], [writes_to_drain,
                     close_if_idle,
                     close_if_old,
                     backoff,
                     shrinks]}].

init_per_suite(Config0) ->
  set_os_vars(),
  ok = logplex_app:a_start(logplex, temporary),
  CertsPath = filename:join([code:lib_dir(ssl), "examples", "certs", "etc", "server"]),
  [{certs_path, CertsPath} | Config0].

end_per_suite(_Config) ->
  application:stop(logplex),
  meck:unload().

init_per_group(DrainType, Config0) 
  when DrainType =:= tcp_syslog;
       DrainType =:= tls_syslog ->
  [{drain_type, DrainType} | Config0];
init_per_group(_Group, Config0) ->
  Config0.

end_per_group(_Group, _Config0) ->
  ok.

init_per_testcase(shrinks, Config0) ->
  MaxBufferSize = 100,
  application:set_env(logplex, tcp_drain_buffer_size, MaxBufferSize),
  application:set_env(logplex, tcp_syslog_shrink_after, 1),
  init_per_testcase('_', [{max_buffer_size, MaxBufferSize} | Config0]);
init_per_testcase(_TestCase, Config0) ->
  Config1 = init_drain_endpoint(Config0),
  init_logplex_drain(Config1).

end_per_testcase(_TestCase, Config) ->
  end_drain_endpoint(),
  end_logplex_drain(Config),
  ok.

wait_for_drain_error(Error) ->
  {ok, Error} = wait_for_drain_(drain_error).

wait_for_log() ->
  wait_for_drain_(drain_data).

wait_for_drain_(Prefix) ->
  receive
    {Prefix, State} -> {ok, State}
  after
    15000 ->
      erlang:error({wait_timeout, Prefix})
  end.

init_drain_endpoint(Config) ->
  DrainType = ?config(drain_type, Config),
  init_drain_endpoint(DrainType, Config).

init_drain_endpoint(tcp_syslog, Config) ->
  Port = 9601,
  DrainURI = "syslog://127.0.0.1:" ++ integer_to_list(Port) ++ "/",
  init_drain_endpoint(ranch_tcp, [{port, Port}], DrainURI, Config);
init_drain_endpoint(tls_syslog, Config) ->
  Port = 9601,
  DrainURI = "syslog+tls://127.0.0.1:" ++ integer_to_list(Port) ++ "/",
  CertsPath = ?config(certs_path, Config),
  init_drain_endpoint(ranch_ssl,
                      [{port, Port},
                       {certfile, filename:join([CertsPath, "cert.pem"])},
                       {keyfile, filename:join([CertsPath, "key.pem"])},
                       {reuseaddr, true}],
                      DrainURI, Config).

init_drain_endpoint(Transport, TransportOpts, URI, Config0) ->
  {ok, ExURI, _} = ex_uri:decode(URI),
  {ok, _} = ranch:start_listener(drain_endpoint, 1,
                                 Transport, TransportOpts,
                                 drain_test_protocol, [{send_to, self()}]),
  [{drain_uri, ExURI} | Config0].

end_drain_endpoint() ->
  ranch:stop_listener(drain_endpoint).

drain_mod_for(tcp_syslog) ->
  logplex_tcpsyslog_drain;
drain_mod_for(tls_syslog) ->
  logplex_tlssyslog_drain.

init_logplex_drain(Config) ->
  DrainType = ?config(drain_type, Config),
  init_logplex_drain(drain_mod_for(DrainType), Config).

init_logplex_drain(DrainMod, Config0) ->
  ChannelID = 1337,
  DrainID = 31337,
  DrainTok = "d.12930-321-312213-12321",
  URI = ?config(drain_uri, Config0),
  {ok, Pid} = DrainMod:start_link(ChannelID, DrainID, DrainTok, URI),
  unlink(Pid),
  [{channel_id, ChannelID}, {drain_id, DrainID}, {drain_token, DrainTok}, {drain_pid, Pid} | Config0].

end_logplex_drain(Config0) ->
  Drain = ?config(drain_pid, Config0),
  erlang:monitor(process, Drain),
  Drain ! shutdown,
  receive
    {'DOWN', _, _, Drain, {shutdown,call}} -> ok;
    {'DOWN', _, _, Drain, Other} -> ct:pal("DRAIN DIED OF REASON: ~p",[Other])
  after 2000 ->
          case Drain of
            undefined -> ok;
            _ -> error({not_dead, sys:get_status(Drain)})
          end
  end.

writes_to_drain(Config) ->
  ChannelID = ?config(channel_id, Config),

  % triggers the drain to connect
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("ping")),
  {ok, Log} = wait_for_log(),

  {match, _} = re:run(Log, "ping"),
  ok.

close_if_idle(Config) ->
  IdleTimeout = 2000,
  IdleFuzz = 1,
  ChannelID = ?config(channel_id, Config),
  application:set_env(logplex, tcp_syslog_idle_timeout, IdleTimeout),
  application:set_env(logplex, tcp_syslog_idle_fuzz, IdleFuzz),

  % triggers the drain to connect
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("idle 1")),
  {ok, _Log} = wait_for_log(),

  % triggers idle timeout on next log line
  timer:sleep(IdleFuzz + IdleTimeout + 10),

  logplex_channel:post_msg({channel, ChannelID}, fake_msg("idle 2")),
  wait_for_drain_error(closed),
  ok.

close_if_old(Config) ->
  ChannelID = ?config(channel_id, Config),
  MaxTTL = 500,
  application:set_env(logplex, tcp_syslog_max_ttl, MaxTTL),

  Pid = spawn_link(?MODULE, send_logs_loop, [ChannelID, 100]),
  wait_for_log(),

  % triggers too old on next log line
  timer:sleep(MaxTTL + 10),
  wait_for_drain_error(closed),
  unlink(Pid),
  exit(Pid, kill),
  ok.

backoff(Config) ->
  ChannelID = ?config(channel_id, Config),

  logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 1: spawn")),
  wait_for_log(),

  end_drain_endpoint(),
  %% logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 2: detects down")),
  timer:sleep(timer:seconds(1 bsl 1)-500),

  init_drain_endpoint(Config),
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 3: detects up")),
  {Time1, _} = timer:tc(fun wait_for_log/0),

  end_drain_endpoint(),
  %% logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 4: detects down")),
  timer:sleep(timer:seconds(1 bsl 2)-500),

  init_drain_endpoint(Config),
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 5: detects up")),
  {Time2, _} = timer:tc(fun wait_for_log/0),

  end_drain_endpoint(),
  %% logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 6: detects down")),
  timer:sleep(timer:seconds(1 bsl 3)-500),

  init_drain_endpoint(Config),
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 7: detects up")),
  {Time3, _} = timer:tc(fun wait_for_log/0),

  ct:pal("time1=~p time2=~p time3=~p", [Time1, Time2, Time3]),
  true = Time1 < Time2,
  true = Time2 < Time3,
  ok.

shrinks(Config) ->
  ChannelID = ?config(channel_id, Config),
  MaxBufferSize = ?config(max_buffer_size, Config),

  logplex_channel:post_msg({channel, ChannelID}, fake_msg("shrink 1: spawn")),
  wait_for_log(),

  end_drain_endpoint(),
  timer:sleep(timer:seconds(1)),

  BufFiller = fun (N) ->
                  logplex_channel:post_msg({channel, ChannelID}, fake_msg("shrink " ++ integer_to_list(N) ++ ": filling"))
             end,

  [BufFiller(N) || N <- lists:seq(1, MaxBufferSize*2)],
  timer:sleep(timer:seconds(1 bsl 2)+500),

  init_drain_endpoint(Config),

  {ok, L10Error} = wait_for_log(),
  {match, _} = re:run(L10Error, "Error L10 "),

  Logs = [wait_for_log() || _ <- lists:seq(1, 10)],
  true = length(Logs) =:= 10,

  try wait_for_log() of
    {ok, _Log} ->
      ct:fail("Got unexpected log, when buffer should have shrunk");
    Other ->
      ct:fail("Got unexpected msg (~p), when buffer should have shrunk", [Other])
  catch
    error:{wait_timeout, _} ->
      % expecting no more logs, since the buffer should have shrunk
      ok
  end.

% ----------------
% Helper Functions
% ----------------

fake_msg(M) ->
    {user, debug, logplex_syslog_utils:datetime(now), "fakehost", "erlang", M}.

send_logs_loop(ChannelID, Timeout) when is_float(Timeout) ->
  send_logs_loop(ChannelID, trunc(Timeout));
send_logs_loop(ChannelID, Timeout) when is_integer(Timeout) ->
  send_logs_loop(ChannelID, Timeout, 1).

send_logs_loop(ChannelID, Timeout, N) ->
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("logs_loop " ++ integer_to_list(N))),
  timer:sleep(Timeout),
  send_logs_loop(ChannelID, Timeout, N+1).

