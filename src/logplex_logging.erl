%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc logging utility and formatting functions
%% @end
-module(logplex_logging).

-export([dest/2
        ,setup/0]).

-include("logplex_logging.hrl").

-define(SYSLOG_TAB, syslog_tab).
-define(SYSLOG_HOST, {127, 0, 0, 1}).
-define(SYSLOG_PORT, 514).
-define(SYSLOG_FACILITY, local2).

-type host() :: inet:ip4_address() | iolist() | binary().

-spec dest(host(), inet:port_number()) -> binary().
dest(Host, Port) ->
    iolist_to_binary([host_str(Host), ":", integer_to_list(Port)]).

-spec host_str(host()) -> iolist() | binary().
host_str({A,B,C,D}) ->
    Quads = [integer_to_list(Quad) || Quad <- [A,B,C,D]],
    string:join(Quads,".");
host_str(H)
  when is_list(H); is_binary(H) ->
    H.

-ifdef(LOG_TO_SYSLOG).

setup() ->
    io:format("Setting up syslog logging~n"),
    {ok, HostName} = inet:gethostname(),
    syslog_lib:init_table(
      ?SYSLOG_TAB,
      logplex,
      ?SYSLOG_HOST,
      ?SYSLOG_PORT,
      ?SYSLOG_FACILITY,
      HostName).

-else.
setup() ->
    io:format("Using batchio logging~n").
-endif.
