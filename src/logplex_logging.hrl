%%% Author  : Geoff Cant <geoff@heroku.com>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(logging_macros).
-define(logging_macros, true).
-compile([{parse_transform, lager_transform}]).

-define(INFO(Format, Args),
        batchio:format("pid=~p m=~p ln=~p class=info " ++ Format ++ "~n",
                       [self(), ?MODULE, ?LINE | Args])).
-define(WARN(Format, Args),
        batchio:format("pid=~p m=~p ln=~p class=warn " ++ Format ++ "~n",
                       [self(), ?MODULE, ?LINE | Args])).
-define(ERR(Format, Args),
        batchio:format("pid=~p m=~p ln=~p class=err " ++ Format ++ "~n",
                       [self(), ?MODULE, ?LINE | Args])).

-endif. %logging
