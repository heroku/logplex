%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Syslog message formatting utilities.
%% @end
-module(logplex_syslog_utils).

-export([to_msg/1
         ,to_msg/2
         ,frame/1
        ]).

-type syslog_msg() :: {0..128, 0..7,
                       Time::iolist(), Source::iolist(),
                       Process::iolist(), Msg::iolist()}.

-export_type([ syslog_msg/0 ]).

-spec to_msg(syslog_msg()) -> iolist().
to_msg({Facility, Severity, Time, Source, Process, Msg})
  when is_integer(Facility), is_integer(Severity) ->
    [ <<"<">>, pri(Facility, Severity), <<">1 ">>,
      Time, $\s, Source, $\s, Process, <<" -- ">>, Msg ].

-spec to_msg(syslog_msg(), Token::iolist()) -> iolist().
to_msg({Facility, Severity, Time, Source, Process, Msg}, Token) ->
    [ <<"<">>, pri(Facility, Severity), <<">1 ">>,
      Time, $\s, Token, $\s, Source, $\s, Process, <<" -- ">>, Msg ].

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
