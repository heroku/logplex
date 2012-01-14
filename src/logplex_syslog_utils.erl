%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Syslog message formatting utilities.
%% @end
-module(logplex_syslog_utils).

-export([to_msg/1
         ,to_msg/2
         ,frame/1
         ,datetime/1
         ,facility_to_int/1
         ,severity_to_int/1
         ,fmt/7
        ]).

-type syslog_msg() :: {0..128, 0..7,
                       Time::iolist(), Source::iolist(),
                       Process::iolist(), Msg::iolist()}.

-export_type([ syslog_msg/0 ]).

-spec to_msg(syslog_msg()) -> iolist().
to_msg({Facility, Severity, Time, Source, Process, Msg})
  when is_integer(Facility), is_integer(Severity) ->
    [ <<"<">>, pri(Facility, Severity), <<">1 ">>,
      Time, $\s, Source, $\s, Process, <<" - - ">>, Msg ].

-spec to_msg(syslog_msg(), Token::iolist()) -> iolist().
to_msg({Facility, Severity, Time, Source, Process, Msg}, Token) ->
    [ <<"<">>, pri(Facility, Severity), <<">1 ">>,
      Time, $\s, Token, $\s, Source, $\s, Process, <<" - - ">>, Msg ].

pri(Facility, Severity)
  when is_integer(Facility),
       is_integer(Severity),
       0 =< Severity, Severity =< 7 ->
    integer_to_list(Facility * 8 + Severity).

-spec frame(Msg::iolist()) -> iolist().
frame(Msg) when is_binary(Msg); is_list(Msg) ->
    Length = iolist_size(Msg),
    [ integer_to_list(Length),
      " ",
      Msg ].

datetime(now) ->
    datetime(os:timestamp());
datetime({_,_,_} = Now) ->
    DT = calendar:now_to_universal_time(Now),
    datetime(DT);
datetime({{Y,M,D},{H,MM,S}}) ->
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B"
                  "Z+00:00",
                  [Y,M,D, H,MM,S]).

fmt(Facility, Severity, Time, Source, Process, Fmt, Args) ->
    {facility_to_int(Facility),
     severity_to_int(Severity),
     datetime(Time),
     Source,
     Process,
     io_lib:format(Fmt, Args)}.

facilities() ->
    [ { 0, kernel, "kernel messages"}
     ,{ 1, user, "user-level messages"}
     ,{ 2, mail, "mail system"}
     ,{ 3, system, "system daemons"}
     ,{ 4, security, "security/authorization messages"}
     ,{ 5, internal, "messages generated internally by syslogd"}
     ,{ 6, lp, "line printer subsystem"}
     ,{ 7, news, "network news subsystem"}
     ,{ 8, uucp, "UUCP subsystem"}
     ,{ 9, clock, "clock daemon"}
     ,{10, security2, "security/authorization messages"}
     ,{11, ftp, "FTP daemon"}
     ,{12, ntp, "NTP subsystem"}
     ,{13, audit, "log audit"}
     ,{14, alert, "log alert"}
     ,{15, clock2, "clock daemon (note 2)"}
     ,{16, local0, "local use 0  (local0)"}
     ,{17, local1, "local use 1  (local1)"}
     ,{18, local2, "local use 2  (local2)"}
     ,{19, local3, "local use 3  (local3)"}
     ,{20, local4, "local use 4  (local4)"}
     ,{21, local5, "local use 5  (local5)"}
     ,{22, local6, "local use 6  (local6)"}
     ,{23, local7, "local use 7  (local7)"}].

facility_to_int(I) when is_integer(I) ->
    I;
facility_to_int(A) when is_atom(A) ->
    element(1, lists:keyfind(A, 2, facilities())).

severities() ->
    [ {0, emergency, "Emergency: system is unusable"}
     ,{1, alert, "Alert: action must be taken immediately"}
     ,{2, critical, "Critical: critical conditions"}
     ,{3, error, "Error: error conditions"}
     ,{4, warning, "Warning: warning conditions"}
     ,{5, notice, "Notice: normal but significant condition"}
     ,{6, info, "Informational: informational messages"}
     ,{7, debug, "Debug: debug-level messages"}].

severity_to_int(I) when is_integer(I) ->
    I;
severity_to_int(A) when is_atom(A) ->
    element(1, lists:keyfind(A, 2, severities())).
