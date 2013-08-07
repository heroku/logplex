%%% Author  : Geoff Cant <geoff@heroku.com>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(logging_macros).
-define(logging_macros, true).
-compile([{parse_transform, lager_transform}]).

-define(INFO(Format, Args),
        lager:info("pid=~p m=~p ln=~p class=info " ++ Format,
                   [self(), ?MODULE, ?LINE | Args])).
-define(WARN(Format, Args),
        lager:warning("pid=~p m=~p ln=~p class=warn " ++ Format,
                      [self(), ?MODULE, ?LINE | Args])).
-define(ERR(Format, Args),
        lager:error("pid=~p m=~p ln=~p class=err " ++ Format,
                    [self(), ?MODULE, ?LINE | Args])).

-endif. %logging
