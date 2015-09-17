-module(drain_test_protocol).
-behaviour(ranch_protocol).

-include("./../src/logplex_logging.hrl").

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts0) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts0]),
  {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
  ok = ranch:accept_ack(Ref),
  loop(Socket, Transport, Opts).

loop(Socket, Transport, Opts) ->
  Transport:setopts(Socket, [{packet, line}, {active, false}]),
  Pid = proplists:get_value(send_to, Opts),
  case Transport:recv(Socket, 0, 5000) of
    {ok, Data} ->
      ?INFO("at=recv sending={ok, ~p} to=~p", [Data, Pid]),
      Pid ! {drain_data, Data},
      loop(Socket, Transport, Opts);
    {error, closed} ->
      ?INFO("at=recv sending=~p to=~p", [closed, Pid]),
      Pid ! {drain_error, closed},
      ok = Transport:close(Socket);
    {error, timeout} ->
      ?INFO("at=recv sending=~p to=~p", [timeout, Pid]),
      Pid ! {drain_error, timeout},
      ok = Transport:close(Socket);
    Other ->
      ?WARN("at=recv unexpected=~p", [Other]),
      ok = Transport:close(Socket)
  end.
