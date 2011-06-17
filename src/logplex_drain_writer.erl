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
-export([start_link/1, init/1, loop/2]).

-include_lib("logplex.hrl").

%% API functions
start_link(_BufferPid) ->
    proc_lib:start_link(?MODULE, init, [self()], 5000).

init(Parent) ->
    io:format("init ~p~n", [?MODULE]),
    {ok, Socket} = gen_udp:open(0, [binary]),
    {ok, RE} = re:compile("^(<\\d+>\\S+) (\\S+) \\S+ (\\S+) (\\S+) \\S+ \\S+ (.*)"),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(RE, Socket).

loop(RE, Socket) ->
    case catch logplex_queue:out(logplex_drain_buffer) of
        timeout -> ok;
        {'EXIT', {noproc, _}} ->
            exit(normal);
        {_, [{_, undefined, _, _}]} ->
            ok;
        {1, [{Token, Host, Port, Msg}]} ->
            case format_packet(RE, Token, Msg) of
                undefined ->
                    io:format("Error poorly formated drain msg=~1000p~n", [Msg]);
                Packet ->
                    case gen_udp:send(Socket, Host, Port, Packet) of
                        ok ->
                            logplex_stats:incr(message_routed),
                            logplex_realtime:incr(message_routed);
                        {error, nxdomain} ->
                            io:format("nxdomin ~s:~w~n", [Host, Port]);
                        Err ->
                            exit(Err)
                    end
            end
    end,
    ?MODULE:loop(RE, Socket).

format_packet(RE, Token, Msg) ->
    case re:run(Msg, RE, [{capture, all_but_first, binary}]) of
        {match, [PriFac, Time, Source, Ps, Content]} ->
            [PriFac, <<" ">>, Time, <<" ">>, Source, <<" ">>, Ps, <<" - - ">>, Token, <<" ">>, Content];
        _ ->
            undefined
    end.
