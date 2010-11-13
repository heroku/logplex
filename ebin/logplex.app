{application, logplex,
 [
  {description, "Log multiplexer"},
  {vsn, "1.0"},
  {modules, [
    logplex,
    logplex_api,
    logplex_app,
    logplex_channel,
    logplex_drain,
    logplex_grid,
    logplex_session,
    logplex_stats,
    logplex_tail,
    logplex_token,
    logplex_utils,
    syslog_server
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto, ssl]},
  {mod, {logplex_app, []}}
 ]}.
 
 
 



