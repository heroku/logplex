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
-module(logplex_drain_writer).
-export([start_link/0, init/1, loop/1]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()], 5000).

init(Parent) ->
    io:format("init ~p~n", [?MODULE]),
    {ok, Socket} = gen_udp:open(0, [binary]),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(Socket).

loop(Socket) ->
    case logplex_drain_buffer:out() of
        undefined -> timer:sleep(10);
        {Host, Port, Msg} ->
            case gen_udp:send(Socket, binary_to_list(Host), Port, Msg) of
                ok -> logplex_stats:incr(message_routed);
                {error, nxdomain} -> error_logger:error_msg("nxdomin ~s:~w~n", [Host, Port]);
                Err -> exit(Err)
            end
    end,
    loop(Socket).