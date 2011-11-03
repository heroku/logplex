{application, logplex,
 [
  {description, "Log multiplexer"},
  {vsn, "1.0"},
  {modules, [
    logplex_api,
    logplex_app,
    logplex_channel,
    logplex_drain,
    logplex_drain_writer,
    logplex_queue_sup,
    logplex_realtime,
    logplex_redis_writer,
    logplex_session,
    logplex_shard,
    logplex_stats,
    logplex_tail,
    logplex_token,
    logplex_utils,
    logplex_worker,
    logplex_worker_sup,
    redis_helper,
    udp_acceptor 
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, inets, crypto, public_key, ssl]},
  {mod, {logplex_app, []}}
 ]}.
 
