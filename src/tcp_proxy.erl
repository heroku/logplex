%% Copyright (c) 2011 Jacob Vorreuter <jacob.vorreuter@gmail.com>
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
-module(tcp_proxy).
-behaviour(gen_server).

%% API
-export([start_link/0, set_socket/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {sock, buffer = <<>>}).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

set_socket(Pid, CSock) ->
    gen_server:cast(Pid, {set_socket, CSock}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    {ok, #state{buffer=syslog_parser:new()}}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast({set_socket, CSock}, State) ->
    {noreply, State#state{sock=CSock}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Packet}, #state{sock=Sock, buffer=Buffer}=State) ->
    {Result, Msgs, NewBuf} = syslog_parser:push(Packet, Buffer),
    case Result of
        ok -> ok;
        {error, Err} ->
            io:format("[~p] event=parse_error, txt=\"~p\"",
                      [?MODULE, Err]),
            ok
    end,
    process_msgs(Msgs),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{buffer=NewBuf}};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _ ,_}, State) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(shutdown, _State) ->
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

process_msgs(Msgs) when is_list(Msgs) ->
    lists:foreach(fun process_msg/1, Msgs).

process_msg({msg, _Msg}) ->
    logplex_stats:incr(message_received_tcp),
    logplex_realtime:incr(message_received_tcp);
process_msg({malformed, Msg}) ->
    io:format("[~p] malformed_syslog_message=\"~p\"~n",
              [?MODULE, Msg]),
    logplex_stats:incr(message_received_tcp_malformed).
    %%logplex_queue:in(logplex_work_queue, Msg).
