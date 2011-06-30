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
-module(logplex_drain_worker).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([start_link/3, send/2]).

-record(state, {regex, token, sock}).

-include_lib("logplex.hrl").

-define(TIMEOUT, 10 * 60 * 1000).

%% API functions
start_link(Token, Host, Port) ->
    gen_server:start_link(?MODULE, [Token, Host, Port], []).

send(Pid, Msg) ->
    gen_server:cast(Pid, {msg, Msg}).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Token, Host, Port]) ->
    {ok, RE} = re:compile("^(<\\d+>\\S+) (\\S+) \\S+ (\\S+) (\\S+) \\S+ \\S+ (.*)"),
    ets:insert(drain_workers, {Token, self()}),
    Sock =
        case gen_tcp:connect(Host, Port, [binary, {active, once}]) of
            {ok, Sock0} -> Sock0;
            Err ->
                error_logger:error_report([?MODULE, connect, Token, Host, Port, Err]),
                undefined
        end,
    {ok, #state{regex=RE, token=Token, sock=Sock}, ?TIMEOUT}.

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

handle_cast({msg, _Msg}, #state{sock=undefined}=State) ->
    {noreply, State, ?TIMEOUT};

handle_cast({msg, Msg}, #state{regex=RE, token=Token, sock=Sock}=State) ->
    case logplex_drain_writer:format_packet(RE, Token, Msg) of
        undefined ->
            ok;
        Packet ->
            case gen_tcp:send(Sock, [integer_to_list(iolist_size(Packet)), "\n", Packet, "\n"]) of
                ok ->
                    inet:setopts(Sock, [{active, once}]),
                    logplex_stats:incr(message_routed),
                    logplex_realtime:incr(message_routed);
                _ ->
                    ok
            end
    end,
    {noreply, State, ?TIMEOUT};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    catch gen_tcp:close(State#state.sock),
    ets:delete(drain_workers, State#state.token),
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State, ?TIMEOUT}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
