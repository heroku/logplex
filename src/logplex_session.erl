-module(logplex_session).
-export([create/1, lookup/1]).

create(Body) when is_binary(Body) ->
    Session = string:strip(os:cmd("uuidgen"), right, $\n),
    redis:q([<<"SETEX">>, Session, <<"360">>, Body]),
    "/sessions/" ++ Session.

lookup(Session) when is_list(Session) ->
    case redis:q([<<"GET">>, Session]) of
        {ok, Data} -> Data;
        Error -> Error
    end.