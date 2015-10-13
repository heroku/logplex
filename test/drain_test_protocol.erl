-module(drain_test_protocol).
-behaviour(ranch_protocol).

-include("./../src/logplex_logging.hrl").

-export([start_link/4]).
-export([init/4]).

-record(state, {ref :: term(),
                socket :: any(),
                transport :: module(),
                send_to :: pid(),
                delay :: pos_integer(),
                blocked = false :: boolean()}).


start_link(Ref, Socket, Transport, Opts0) ->
  Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts0]),
  {ok, Pid}.

init(Ref, Socket, Transport, Opts) ->
  State = #state{ref=Ref,
                 socket=Socket,
                 transport=Transport,
                 send_to=proplists:get_value(send_to, Opts),
                 delay=proplists:get_value(delay, Opts, 0),
                 blocked=proplists:get_value(blocked, Opts, false)},
  accept_and_loop(Ref, State).

accept_and_loop(_Ref, #state{ blocked=true }=State) ->
  loop(State);
accept_and_loop(Ref, State) ->
  ranch:accept_ack(Ref),
  loop(State).

loop(#state{ socket=Socket, transport=Transport, blocked=true }=State0) ->
  capture({drain_error, closed}, State0),
  Transport:close(Socket);
loop(#state{ socket=Socket, transport=Transport }=State0) ->
  Transport:setopts(Socket, [{packet, line}, {active, false}]),
  Recv = Transport:recv(Socket, 0, 5000),
  case handle_recv(Recv, State0) of
    {ok, State1} ->
      maybe_delay(State0),
      loop(State1);
    _ ->
      ok = Transport:close(Socket)
  end.

handle_recv({error, Reason}, State)
  when Reason =:= timeout;
       Reason =:= closed ->
  capture({drain_error, closed}, State),
  {error, Reason};
handle_recv({ok, _}, #state{ blocked=true }=State) ->
  capture({drain_error, closed}, State),
  {error, closed};
handle_recv({ok, Data}, State) ->
  capture({drain_data, Data}, State),
  {ok, State};
handle_recv(Other, _) ->
  ?WARN("at=recv unexpected=~p", [Other]),
  {error, unexpected}.

capture(Msg, #state{ send_to=Pid }=State) ->
  ?INFO("at=recv state=~p sending=~p to=~p", [State, Msg, Pid]),
  Pid ! Msg.

maybe_delay(#state{ delay=0 }) ->
  no_delay;
maybe_delay(#state{ delay=N }) ->
  timer:sleep(N).

