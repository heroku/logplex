%%% Author  : Geoff Cant <geoff@heroku.com>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(logging_macros).
-define(logging_macros, true).

-compile([{parse_transform, lager_transform}]).

-define(INFO(Format, Args),
        lager:info(Format ++ "~n", Args)).
-define(WARN(Format, Args),
        lager:warn(Format ++ "~n", Args)).
-define(ERR(Format, Args), 
        lager:error(Format ++ "~n", Args)).

-endif. %logging
