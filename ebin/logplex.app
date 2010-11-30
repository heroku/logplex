{application, logplex,
 [
  {description, "Log multiplexer"},
  {vsn, "1.0"},
  {modules, [
    logplex_api,
    logplex_app,
    logplex_buffer,
    logplex_channel,
    logplex_drain,
    logplex_drain_pool,
    logplex_grid,
    logplex_queue,
    logplex_session,
    logplex_stats,
    logplex_tail,
    logplex_token,
    logplex_utils,
    logplex_worker,
    logplex_worker_mgr,
    logplex_writer,
    redis_helper,
    syslog_acceptor
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto, ssl]},
  {mod, {logplex_app, []}}
 ]}.
 
 
 



