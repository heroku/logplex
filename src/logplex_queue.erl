%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(logplex_queue).
-export([start_link/0, init/1, loop/1]).

-export([in/1, out/0]).

-record(state, {queue, length}).

-define(MAX_LENGTH, 2000).

%% API functions
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()], 5000).

in(Packet) ->
    whereis(?MODULE) ! {undefined, {in, Packet}}.

out() ->
    Pid = whereis(?MODULE),
    Pid ! {self(), out},
    receive {Pid, Reply} -> Reply after 30000 -> {error, timeout} end.

init(Parent) ->
    Self = self(),
    register(?MODULE, Self),
    spawn_link(fun() -> report_stats(Self) end),
    proc_lib:init_ack(Parent, {ok, self()}),
    ?MODULE:loop(#state{queue=queue:new(), length=0}).

loop(State) ->
    State1 = receive Msg -> handle(Msg, State) end,
    ?MODULE:loop(State1).

handle({undefined, {in, _Packet}}, #state{length=Length}=State) when Length >= ?MAX_LENGTH ->
    logplex_stats:incr(queue_dropped),
    State;

handle({undefined, {in, Packet}}, #state{queue=Queue, length=Length}=State) ->
    Queue1 = queue:in(Packet, Queue),
    State#state{queue=Queue1, length=Length+1};

handle({From, out}, #state{queue=Queue, length=Length}=State) ->
    case queue:out(Queue) of
        {{value, Out}, Queue1} ->
            From ! {self(), Out},
            State#state{queue=Queue1, length=Length-1};
        {empty, _Queue} ->
            From ! {self(), undefined},
            State
    end;

handle({undefined, report_stats}, #state{length=Length}=State) ->
    ets:insert(logplex_stats, {queue_length, Length}),
    State;

handle(_, State) ->
    State.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
report_stats(Pid) ->
    timer:sleep(60000),
    Pid ! {undefined, report_stats},
    report_stats(Pid).