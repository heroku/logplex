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
-module(logplex_cloudkick).
-export([start_link/0, loop/1]).

-include_lib("logplex.hrl").

-define(HDR, [{"Content-Type", "text/html"}]).

start_link() ->
    mochiweb_http:start([
        {ip, "127.0.0.1"},
        {port, 8008},
        {backlog, 1024},
        {loop, {?MODULE, loop}},
        {name, ?MODULE}
    ]).

loop(Req) ->
    try
        [begin
            Req:respond({200, ?HDR, iolist_to_binary(mochijson2:encode({struct, [
                {status, iolist_to_binary(io_lib:format("Zero ~p child processes running", [Worker]))},
                {state, <<"err">>}
            ]}))}),
            throw(normal)
        end || {Worker, 0} <- logplex_stats:workers()],

        RegisteredMods = [logplex_grid, logplex_rate_limit, logplex_realtime, 
			  logplex_stats, logplex_tail, logplex_shard, udp_acceptor],
        [begin
            case (whereis(Name) == undefined orelse not is_process_alive(whereis(Name))) of
                true ->
                    Req:respond({200, ?HDR, iolist_to_binary(mochijson2:encode({struct, [
                        {status, iolist_to_binary(io_lib:format("Process dead: ~p", [Name]))},
                        {state, <<"err">>}
                    ]}))}),
                    throw(normal);
                false ->
                    ok
            end
        end || Name <- RegisteredMods],

        Cached = logplex_stats:cached(),
        Req:respond({200, ?HDR, iolist_to_binary(mochijson2:encode({struct, [
            {state, <<"ok">>},
            {metrics, [
                [{type, <<"int">>},
                 {name, Key},
                 {value, Value}]
            || {Key, Value} <- Cached, Value > 0]}
        ]}))})
    catch 
        exit:normal ->
            ok;
        _ ->
            Req:respond({500, ?HDR, ""})
    end.
