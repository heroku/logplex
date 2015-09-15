-module(logplex_tls_syslog_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-include("../src/logplex_tlssyslog_drain.hrl").
-compile(export_all).

-include("logplex_test_helpers.hrl").

all() -> [ensure_drain_endpoint, close_if_idle, close_if_old].

init_per_suite(Config0) ->
  set_os_vars(),
  ok = logplex_app:a_start(logplex, temporary),
  CertsPath = filename:join([code:lib_dir(ssl), "examples", "certs", "etc", "server"]),
  [{certs_path, CertsPath} | Config0].

end_per_suite(_Config) ->
  application:stop(logplex),
  meck:unload().

init_per_testcase(_TestCase, Config0) ->
  application:set_env(logplex, tcp_syslog_idle_timeout, 50),
  application:set_env(logplex, tcp_syslog_idle_fuzz, 1),
  init_drain_endpoint(Config0).

end_per_testcase(_TestCase, _Config) ->
  end_drain_endpoint(),
  ok.

wait_for_log() ->
  receive
    {msg, Line} ->
      {ok, Line};
    Other ->
      {error, {unknown, Other}}
  after
    5000 ->
      {error, {no_logs_after, 5000}}
  end.

init_drain_endpoint(Config0) ->
  _CertsPath = ?config(certs_path, Config0),
  Port = 9601,
  {ok, _} = ranch:start_listener(drain_endpoint, 1,
                                 ranch_tcp, [{port, Port}],
                                 drain_test_protocol, [{send_to, self()}]),
  [{drain_uri, "syslog://127.0.0.1:" ++ integer_to_list(Port) ++ "/"} | Config0].

end_drain_endpoint() ->
  ranch:stop_listener(drain_endpoint).


ensure_drain_endpoint(Config) ->
  URI = ?config(drain_uri, Config),
  {ok, Socket, Transport} = connect_to_endpoint(ex_uri:decode(URI)),
  ok = Transport:send(Socket, <<"ping\n">>),
  {ok, <<"ping\n">>} = wait_for_log(),
  ok.

connect_to_endpoint({ok, #ex_uri{scheme="syslog", authority=#ex_uri_authority{host=Host, port=Port}}, _Rest}) ->
  connect_to_endpoint(ranch_tcp, Host, Port, []).

connect_to_endpoint(Transport, Host, Port, Opts) ->
  {ok, Socket} = Transport:connect(Host, Port, Opts),
  {ok, Socket, Transport}.

close_if_idle(_Config) ->
  ct:fail("unimplemented").

close_if_old(_Config) ->
  ct:fail("unimplemented").
