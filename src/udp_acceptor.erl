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
-module(udp_acceptor).
-export([start_link/0, init/1, loop/3]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()], 5000).

init(Parent) ->
    Self = self(),
    register(?MODULE, Self),
    {ok, Socket} = gen_udp:open(?UDP_PORT, [binary, {active, once}]),
    proc_lib:init_ack(Parent, {ok, self()}),
    ?MODULE:loop(Socket, true, undefined).

loop(Socket, Accept, StopTime) ->
    receive
        {udp, Socket, _IP, _InPortNo, Packet} ->
            logplex_stats:incr(message_received),
            logplex_realtime:incr(message_received),
            logplex_queue:in(logplex_work_queue, Packet),
            Accept andalso inet:setopts(Socket, [{active, once}]),
            ?MODULE:loop(Socket, Accept, StopTime);
        {From, stop_accepting} ->
            error_logger:error_msg("[~p] event=stop_accepting from=~p", [?MODULE, From]),
            ?MODULE:loop(Socket, false, now());
        {From, start_accepting} ->
            Diff = timer:now_diff(now(), StopTime),
            error_logger:info_msg("[~p] event=start_accepting from=~p time=~w", [?MODULE, From, Diff]),
            inet:setopts(Socket, [{active, once}]),
            ?MODULE:loop(Socket, true, undefined);
        _ ->
            ?MODULE:loop(Socket, Accept, StopTime)
    end.
