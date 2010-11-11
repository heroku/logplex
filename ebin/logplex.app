{application, logplex,
 [
  {description, "Log multiplexer"},
  {vsn, "1.0"},
  {modules, [
    logplex,
    logplex_api,
    logplex_app,
    logplex_channel,
    logplex_grid,
    logplex_session,
    logplex_token,
    syslog_server
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto, ssl]},
  {mod, {logplex_app, []}}
 ]}.
 
 
 



