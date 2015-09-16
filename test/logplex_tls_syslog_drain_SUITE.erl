-module(logplex_tls_syslog_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-include("../src/logplex_tlssyslog_drain.hrl").
-compile(export_all).

-include("logplex_test_helpers.hrl").

all() -> [{group, tcp_syslog},
          {group, tls_syslog}].

groups() ->
  [{tcp_syslog, [], [{group, full_suite}]},
   {tls_syslog, [], [{group, full_suite}]},
   {full_suite, [], [ensure_drain_endpoint, close_if_idle, close_if_old]}].

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
  Config1 = init_drain_endpoint(DrainType, Config0),
  init_logplex_drain(drain_mod_for(DrainType), Config1);
init_per_group(_Group, Config0) ->
  Config0.

end_per_group(DrainType, Config)
  when DrainType =:= tcp_syslog;
       DrainType =:= tls_syslog ->
  end_drain_endpoint(),
  end_logplex_drain(Config),
  ok;
end_per_group(_Group, _Config0) ->
  ok.

init_per_testcase(_TestCase, Config0) ->
  IdleTimeout = 50,
  IdleFuzz = 1,
  MaxTTL = 50,
  application:set_env(logplex, tcp_syslog_max_ttl, MaxTTL),
  application:set_env(logplex, tcp_syslog_idle_timeout, IdleTimeout),
  application:set_env(logplex, tcp_syslog_idle_fuzz, IdleFuzz),
  application:set_env(logplex, tcp_syslog_reconnect_min, 1),
  ranch:set_protocol_options(drain_endpoint, [{send_to, self()}]),
  Config0.

end_per_testcase(_TestCase, _Config) ->
  ok.

wait_for_drain_error(Error) ->
  {ok, Error} = wait_for_drain_(drain_error).

wait_for_log() ->
  wait_for_drain_(drain_data).

wait_for_drain_(Prefix) ->
  receive
    {Prefix, State} -> {ok, State}
  after
    5000 ->
      ct:fail("Timeout while waiting for drain ~p", [Prefix]),
      {error, {no_logs_after, 5000}}
  end.

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
                                 drain_test_protocol, []),
  [{drain_uri, ExURI} | Config0].

end_drain_endpoint() ->
  ranch:stop_listener(drain_endpoint).

drain_mod_for(tcp_syslog) ->
  logplex_tcpsyslog_drain;
drain_mod_for(tls_syslog) ->
  logplex_tlssyslog_drain.

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

ensure_drain_endpoint(Config) ->
  URI = ?config(drain_uri, Config),
  {ok, Socket, Transport} = connect_to_endpoint(URI, Config),
  ok = Transport:send(Socket, <<"ping\n">>),
  {ok, <<"ping\n">>} = wait_for_log(),
  ok.

connect_to_endpoint(#ex_uri{scheme="syslog+tls", authority=#ex_uri_authority{host=Host, port=Port}}, Config) ->
  CertsPath = ?config(certs_path, Config),
  connect_to_endpoint(ranch_ssl, Host, Port, [{cacertfile, filename:join([CertsPath, "cacerts.pem"])}]);
connect_to_endpoint(#ex_uri{scheme="syslog", authority=#ex_uri_authority{host=Host, port=Port}}, _Config) ->
  connect_to_endpoint(ranch_tcp, Host, Port, []).

connect_to_endpoint(Transport, Host, Port, Opts) ->
  {ok, Socket} = Transport:connect(Host, Port, Opts),
  {ok, Socket, Transport}.

close_if_idle(Config) ->
  ChannelID = ?config(channel_id, Config),
  IdleTimeout = logplex_app:config(tcp_syslog_idle_timeout),
  IdleFuzz = logplex_app:config(tcp_syslog_idle_fuzz),

  % triggers the drain to connect
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("mymsg1")),
  {ok, _Log} = wait_for_log(),

  % triggers idle timeout on next log line
  timer:sleep(IdleFuzz + IdleTimeout + 10),

  logplex_channel:post_msg({channel, ChannelID}, fake_msg("mymsg2")),
  wait_for_drain_error(closed),
  ok.

close_if_old(Config) ->
  ChannelID = ?config(channel_id, Config),
  IdleTimeout = logplex_app:config(tcp_syslog_idle_timeout),
  MaxTTL = logplex_app:config(tcp_syslog_max_ttl),

  spawn_link(?MODULE, send_logs_loop, [ChannelID, IdleTimeout/2]),
  wait_for_log(),

  % triggers too old on next log line
  timer:sleep(MaxTTL + 10),
  wait_for_drain_error(closed),
  ok.

send_logs_loop(ChannelID, Timeout) when is_float(Timeout) ->
  send_logs_loop(ChannelID, trunc(Timeout));
send_logs_loop(ChannelID, Timeout) ->
  logplex_channel:post_msg({channel, ChannelID}, fake_msg("mymsg")),
  timer:sleep(Timeout),
  send_logs_loop(ChannelID, Timeout).

% ----------------
% Helper Functions
% ----------------

fake_msg(M) ->
    {user, debug, logplex_syslog_utils:datetime(now), "fakehost", "erlang", M}.
