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
-module(tcp_acceptor).
-behaviour(gen_nb_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         sock_opts/0,
         new_connection/4,
         terminate/2,
         code_change/3]).

-include_lib("logplex.hrl").

-record(state, {accept=true}).

start_link(Port) ->
    gen_nb_server:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port], State) ->
    case gen_nb_server:add_listen_socket({"0.0.0.0", Port}, State) of
        {ok, State1} ->
            {ok, gen_nb_server:store_cb_state(#state{}, State1)};
        Error ->
            {stop, Error, State}
    end.

sock_opts() ->
    [binary, {active, false}, {reuseaddr, true}, {nodelay, true}, {packet, raw}].

new_connection(_IpAddr, _Port, CSock, State) ->
    inet:setopts(CSock, [{active, once}]),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Packet}, State) ->
    inet:setopts(Sock, [{active, once}]),
    logplex_stats:incr(message_received),
    logplex_realtime:incr(message_received),
    case gen_nb_server:get_cb_state(State) of
        #state{accept=true} ->
            logplex_queue:in(logplex_work_queue, Packet);
        #state{accept=false} ->
            logplex_stats:incr(message_dropped),
            logplex_realtime:incr(work_queue_dropped)
    end,
    {noreply, State};

handle_info({_From, stop_accepting}, State) ->
    {noreply, gen_nb_server:store_cb_state(#state{accept=false}, State)}; 

handle_info({_From, start_accepting}, State) ->
    {noreply, gen_nb_server:store_cb_state(#state{accept=true}, State)}; 

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
