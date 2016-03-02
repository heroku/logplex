%%% Author  : Geoff Cant <geoff@heroku.com>
%%% Description : Logging macros
%%% Created : 13 Jan 2006 by Geoff Cant <nem@lisp.geek.nz>

-ifndef(logging_macros).
-define(logging_macros, true).
-compile([{parse_transform, lager_transform}]).

%% Uncomment this to use batchio logging
%% -define(LOG_TO_SYSLOG, true).

-define(LOGGING_ARGS(LEVEL, FORMAT, ARGS),
        "pid=~p m=~p ln=~p class=~s " ++ FORMAT ++ "~n", [self(), ?MODULE, ?LINE, LEVEL | ARGS]).

-ifdef(LOG_TO_SYSLOG).
-define(INFO(Format, Args),
        syslog_lib:notice(
          syslog_tab, io_lib:format(?LOGGING_ARGS("info", Format, Args)))).
-define(WARN(Format, Args),
        syslog_lib:notice(
          syslog_tab, io_lib:format(?LOGGING_ARGS("warn", Format, Args)))).
-define(ERR(Format, Args),
        syslog_lib:notice(
          syslog_tab, io_lib:format(?LOGGING_ARGS("err", Format, Args)))).

-else.
-define(INFO(Format, Args),
        batchio:format(?LOGGING_ARGS("info", Format, Args))).
-define(WARN(Format, Args),
        batchio:format(?LOGGING_ARGS("warn", Format, Args))).
-define(ERR(Format, Args),
        batchio:format(?LOGGING_ARGS("err", Format, Args))).
-endif. %LOG_TO_SYSLOG

-endif. %logging
