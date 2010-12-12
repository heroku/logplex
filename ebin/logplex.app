{application, logplex,
 [
  {description, "Log multiplexer"},
  {vsn, "1.0"},
  {modules, [
    logplex_api,
    logplex_app,
    logplex_channel,
    logplex_drain,
    logplex_drain_buffer,
    logplex_drain_writer,
    logplex_grid,
    logplex_rate_limit,
    logplex_read_queue,
    logplex_reader,
    logplex_realtime,
    logplex_redis_buffer,
    logplex_redis_writer,
    logplex_session,
    logplex_shard,
    logplex_stats,
    logplex_sup,
    logplex_tail,
    logplex_token,
    logplex_utils,
    logplex_worker,
    logplex_work_queue,
    redis_helper,
    syslog_acceptor
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto, ssl]},
  {mod, {logplex_app, []}}
 ]}.
 
 
 



