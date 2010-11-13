-module(logplex_stats).
-export([healthcheck/0, incr/1]).

healthcheck() ->
    {ok, Count} = redis:q([<<"INCR">>, <<"healthcheck">>]),
    Count.

incr(Key) when is_atom(Key) ->
    ok;

incr({Key, Value}) when is_atom(Key), is_atom(Value) ->
    ok.