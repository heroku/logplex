{application, logplex,
 [
  {description, "Log multiplexer"},
  {vsn, "1.0"},
  {modules, [
    logplex_app,
    logplex
  ]},
  {registered, []},
  {applications, [kernel, stdlib, sasl, crypto]},
  {mod, {logplex_app, []}}
 ]}.
 
 
 



