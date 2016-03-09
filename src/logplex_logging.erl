%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc logging utility and formatting functions
%% @end
-module(logplex_logging).

-export([dest/2
        ,setup/0]).

-include("logplex_logging.hrl").

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
    Host = logplex_app:config(logging_syslog_host),
    Port = logplex_app:config(logging_syslog_port),
    io:format("Setting up syslog logging on ~p:~p~n", [Host, Port]),
    {ok, HostName} = inet:gethostname(),
    syslog_lib:init_table(
      logplex_app:config(logging_syslog_tab),
      logplex,
      Host,
      Port,
      logplex_app:config(logging_syslog_facility),
      HostName).

-else.
setup() ->
    io:format("Using batchio logging~n").
-endif.
