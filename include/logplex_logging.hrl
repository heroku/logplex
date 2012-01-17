%%% Author  : Geoff Cant <geoff@heroku.com>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(logging_macros).
-define(logging_macros, true).

-define(INFO(Format, Args),
        io:format("(~p info ~p:~p) " ++ Format,
                  [self(), ?MODULE, ?LINE | Args])).
-define(WARN(Format, Args),
        io:format("(~p warn ~p:~p) " ++ Format,
                  [self(), ?MODULE, ?LINE | Args])).
-define(ERR(Format, Args),
        io:format("(~p err ~p:~p) " ++ Format,
                  [self(), ?MODULE, ?LINE | Args])).

-endif. %logging
