-module(drain_test_protocol).
-behaviour(ranch_protocol).

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
  case Transport:recv(Socket, 0, 2000) of
    {ok, Data} ->
      Pid ! {drain_data, Data},
      % TODO send the message out somewhere
      loop(Socket, Transport, Opts);
    {error, closed} ->
      Pid ! {drain_error, closed};
    {error, timeout} ->
      Pid ! {drain_error, timeout};
    _ ->
      ok = Transport:close(Socket)
  end.
