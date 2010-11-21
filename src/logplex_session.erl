-module(logplex_session).
-export([create/1, lookup/1]).

create(Body) when is_binary(Body) ->
    Session = string:strip(os:cmd("uuidgen"), right, $\n),
    redis_helper:create_session(Session, Body),
    iolist_to_binary([<<"/sessions/">>, Session]).

lookup(Session) when is_binary(Session) ->
    redis_helper:lookup_session(Session).
