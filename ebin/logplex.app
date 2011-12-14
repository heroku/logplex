{application, logplex,
 [
  {description, "Log multiplexer"},
  {vsn, "1.0"},
  {modules, [
    http_handler
   ,logplex_api
   ,logplex_api_tests
   ,logplex_app
   ,logplex_channel
   ,logplex_db
   ,logplex_drain
   ,logplex_drain_writer
   ,logplex_drain_writer_mon
   ,logplex_queue
   ,logplex_queue_sup
   ,logplex_realtime
   ,logplex_redis_writer
   ,logplex_report_handler
   ,logplex_session
   ,logplex_shard
   ,logplex_stats
   ,logplex_tail
   ,logplex_token
   ,logplex_utils
   ,logplex_worker
   ,logplex_worker_sup
   ,nsync_callback
   ,redis_helper
   ,syslog_parser
   ,tcp_acceptor
   ,tcp_proxy
   ,tcp_proxy_sup
   ,udp_acceptor
   ,uuid
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, inets, crypto, public_key, ssl]},
  {mod, {logplex_app, []}}
 ]}.
 
