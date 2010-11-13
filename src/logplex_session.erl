-module(logplex_session).
-export([create/1, lookup/1]).

create(Body) when is_binary(Body) ->
    Session = string:strip(os:cmd("uuidgen"), right, $\n),
    redis:q([<<"SETEX">>, Session, <<"360">>, Body]),
    iolist_to_binary([<<"/sessions/">> ++ Session]).

lookup(Session) when is_binary(Session) ->
    case redis:q([<<"GET">>, Session]) of
        {ok, Data} -> Data;
        Error -> Error
    end.