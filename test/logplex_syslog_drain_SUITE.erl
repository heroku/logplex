-module(logplex_syslog_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-compile(export_all).

-include("logplex_test_helpers.hrl").

all() -> [{group, tcp_syslog},
          {group, tls_syslog}].

groups() ->
  [{tcp_syslog, [], [{group, full_suite}]},
   {tls_syslog, [], [{group, full_suite}, searches_to_max_depth, insecure_drain]},
   {full_suite, [], [writes_to_drain,
                     flushes_on_shutdown,
                     close_if_idle,
                     close_if_old,
                     backoff,
                     shrinks]}].

init_per_suite(Config) ->
  set_os_vars(),
  CertsPath = filename:join([?config(data_dir, Config), "client"]),
  ok = logplex_app:a_start(logplex, temporary),
  application:set_env(logplex, tls_cacertfile, filename:join([CertsPath, "cacerts.pem"])),
  Config.

end_per_suite(_Config) ->
  application:stop(logplex),
  meck:unload().

init_per_group(DrainType, Config0) 
  when DrainType =:= tcp_syslog;
       DrainType =:= tls_syslog ->
  [{drain_type, DrainType}, {port, 9071} | Config0];
init_per_group(_Group, Config0) ->
  Config0.

end_per_group(_Group, _Config0) ->
  ok.

init_per_testcase(flushes_on_shutdown, Config0) ->
  init_per_testcase('_', [{drain_endpoint_args, [{delay, 2000}]} | Config0]);
init_per_testcase(shrinks, Config0) ->
  MaxBufferSize = 100,
  application:set_env(logplex, tcp_drain_buffer_size, MaxBufferSize),
  application:set_env(logplex, tcp_syslog_shrink_after, 1),
  init_per_testcase('_', [{max_buffer_size, MaxBufferSize} | Config0]);
init_per_testcase(TestCase, Config0) ->
  % flushes session cache between runs
  ssl:stop(),
  ssl:start(),
  Config1 = init_drain_endpoint(TestCase, Config0),
  init_logplex_drain(Config1).


end_per_testcase(flushes_on_shutdown, _Config) ->
  end_drain_endpoint(),
  ok;
end_per_testcase(_TestCase, Config) ->
  end_drain_endpoint(),
  end_logplex_drain(Config),
  ok.

wait_for_drain_error(Error) ->
  {ok, Error} = wait_for_drain_(drain_error, 5000).

wait_for_log() ->
  wait_for_log(5000).

wait_for_log(Timeout) ->
  wait_for_drain_(drain_data, Timeout).

wait_for_drain_(Prefix, Timeout) ->
  receive
    {Prefix, State} -> {ok, State}
  after
    Timeout ->
      erlang:error({wait_timeout, Prefix})
  end.

init_drain_endpoint(TestCase, Config) ->
  {Transport, TransportOpts} = transport_for_testcase(Config),
  DrainURI = uri_for_testcase(TestCase, Config),
  init_drain_endpoint(Transport, TransportOpts, DrainURI, Config).

init_drain_endpoint(Transport, TransportOpts, URI, Config0) ->
  {ok, ExURI, _} = ex_uri:decode(URI),
  Args = [{send_to, self()} | ct:get_config(drain_endpoint_args, Config0, [])],
  {ok, _} = ranch:start_listener(drain_endpoint, 1,
                                 Transport, TransportOpts,
                                 drain_test_protocol, Args),
  [{drain_uri, ExURI} | Config0].

end_drain_endpoint() ->
  ranch:stop_listener(drain_endpoint).

uri_for_testcase(TestCase, Config) ->
  Port = ?config(port, Config),
  Scheme = case ?config(drain_type, Config) of
               tcp_syslog -> "syslog://";
               tls_syslog -> "syslog+tls://"
           end,
  Fragment = case TestCase of
                 insecure_drain -> "#insecure";
                 _              -> ""
             end,
  Scheme ++ "127.0.0.1:" ++ integer_to_list(Port) ++ "/" ++ Fragment.

transport_for_testcase(Config) ->
    transport_for_testcase(?config(drain_type, Config), Config).

transport_for_testcase(tcp_syslog, Config) ->
    {ranch_tcp, [{port, ?config(port, Config)}]};
transport_for_testcase(tls_syslog, Config) ->
    CertsPath = filename:join([?config(data_dir, Config), "server"]),
    {ranch_ssl, [{port, ?config(port, Config)},
                 {cacertfile, filename:join([CertsPath, "cacerts.pem"])},
                 {certfile, filename:join([CertsPath, "cert.pem"])},
                 {keyfile, filename:join([CertsPath, "key.pem"])},
                 {reuseaddr, true}]}.


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
  after 5500 ->
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

searches_to_max_depth(Config) ->
  ChannelID = ?config(channel_id, Config),
  DrainID = ?config(drain_id, Config),

  %% expects root ca
  application:set_env(logplex, tls_max_depth, 0),

  % triggers the drain to connect
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("tls_max_depth")),

  try
    wait_for_log(1000)
  catch
    error:{wait_timeout, _} -> expected
  end,

  Pid = logplex_drain:whereis(DrainID),
  {disconnected, _} = recon:get_state(Pid),
  ok.

insecure_drain(Config) ->
  ChannelID = ?config(channel_id, Config),

  %% expects root ca
  application:set_env(logplex, tls_max_depth, 0),

  % triggers the drain to connect
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("ping")),
  {ok, Log} = wait_for_log(),

  {match, _} = re:run(Log, "ping"),
  ok.

flushes_on_shutdown(Config) ->
  NumLogs = 10,
  ChannelID = ?config(channel_id, Config),
  Delay = ?config(delay, ?config(drain_endpoint_args, Config)),

  BufFiller = fun (N) ->
                  logplex_channel:post_msg({channel, ChannelID}, fake_msg("flushes_on_shutdown " ++ integer_to_list(N) ++ ": filling"))
              end,
  % triggers the drain to connect
  [BufFiller(N) || N <- lists:seq(1, NumLogs)],

  ok = end_logplex_drain(Config),

  Logs = [wait_for_log(Delay+100) || _ <- lists:seq(1, NumLogs)],
  NumLogs = length(Logs),

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
  BackoffTime1 = timer:seconds(1 bsl 1)-500,
  timer:sleep(BackoffTime1),

  init_drain_endpoint(backoff, Config),
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 3: detects up")),
  {Time1, _} = timer:tc(fun wait_for_log/1, [BackoffTime1]),

  end_drain_endpoint(),
  BackoffTime2 = timer:seconds(1 bsl 2)-500,
  timer:sleep(BackoffTime2),

  init_drain_endpoint(backoff, Config),
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 5: detects up")),
  {Time2, _} = timer:tc(fun wait_for_log/1, [BackoffTime2]),

  end_drain_endpoint(),
  BackoffTime3 = timer:seconds(1 bsl 3)-500,
  timer:sleep(BackoffTime3),

  init_drain_endpoint(backoff, Config),
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("backoff 7: detects up")),
  {Time3, _} = timer:tc(fun wait_for_log/1, [BackoffTime3]),

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

  init_drain_endpoint(shrinks, Config),

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

