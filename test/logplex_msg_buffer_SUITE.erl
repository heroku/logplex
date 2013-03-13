-module(logplex_msg_buffer_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [run_eunit].

run_eunit(_Config) ->
    ok = eunit:test(logplex_msg_buffer).
