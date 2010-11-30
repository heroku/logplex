#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name logplex_console@`hostname` -env HTTP_PORT 8001 -env LOGPLEX_AUTH_KEY secret -pa ebin -pa deps/mochiweb/ebin -pa deps/redis_pool/ebin

main(_) ->
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(logplex),

    {ok,{{_,200,_},_H,_B}} = httpc:request(get, {"http://localhost:8001/healthcheck", [{"Authorization", "secret"}]}, [], []),

    %% write more tests... %%

    io:format("OK: tests passed.~n").
