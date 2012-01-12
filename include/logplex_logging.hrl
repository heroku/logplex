%%% Author  : Geoff Cant <geoff@heroku.com>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(logging_macros).
-define(logging_macros, true).

-define(INFO(Format, Args),
        error_logger:info_msg("(~p ~p:~p) " ++ Format,
                              [self(), ?MODULE, ?LINE | Args])).
-define(WARN(Format, Args),
        error_logger:warning_msg("(~p ~p:~p) " ++ Format,
                                 [self(), ?MODULE, ?LINE | Args])).
-define(ERR(Format, Args),
        error_logger:error_msg("(~p ~p:~p) " ++ Format,
                               [self(), ?MODULE, ?LINE | Args])).

-endif. %logging
