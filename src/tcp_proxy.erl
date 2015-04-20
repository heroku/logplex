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

-record(state, {sock :: 'undefined' | port(),
                buffer = syslog_parser:new(),
                peername :: 'undefined' | {inet:ip4_address(),
                                           inet:port_number()},
                connect_time :: 'undefined' | erlang:timestamp()
               }).

-include("logplex_logging.hrl").

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
    {ok, #state{}}.

handle_call(Msg, _From, State) ->
    ?WARN("err=unexpected_call data=~p", [Msg]),
    {noreply, State}.

handle_cast({set_socket, CSock}, State) ->
    case inet:peername(CSock) of
        {ok, PeerName = {PHost, PPort}} ->
            {ok, {Host, Port}} = inet:sockname(CSock),
            ?INFO("at=new_connection peer=~s local=~s",
                  [logplex_logging:dest(PHost, PPort),
                   logplex_logging:dest(Host, Port)]),
            inet:setopts(CSock, [{active, once}]),
            {noreply, State#state{sock=CSock,
                                  peername=PeerName,
                                  connect_time=os:timestamp()}};
        {error, Reason} ->
            ?INFO("at=new_connection err=\"~p\"",
                  [Reason]),
            {stop, normal, State}
    end;

handle_cast(Msg, State) ->
    ?WARN("err=unexpected_cast data=~p", [Msg]),
    {noreply, State}.

handle_info({tcp, Sock, Packet},
            #state{sock=Sock, buffer=Buffer}=State) ->
    {Result, Msgs, NewBuf} = syslog_parser:push(Packet, Buffer),
    case Result of
        ok -> ok;
        {error, Err} ->
            ?INFO("[~p] event=parse_error, txt=\"~p\"",
                      [?MODULE, Err]),
            ok
    end,
    logplex_message:process_msgs(Msgs),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{ buffer=NewBuf }};

handle_info({tcp_closed, Sock}, State = #state{sock = Sock,
                                               peername={H,P}}) ->
    ?INFO("at=close peer=~s duration=~s",
          [logplex_logging:dest(H, P), duration(State)]),
    {stop, normal, State};

handle_info({tcp_error, Sock, Reason},
            State = #state{sock = Sock,
                           peername = {H,P}}) ->
    ?WARN("err=gen_tcp peer=~s data=~p duration=~s",
          [logplex_logging:dest(H,P), Reason, duration(State)]),
    {stop, normal, State};

handle_info(Msg, State) ->
    ?WARN("err=unexpected_cast data=~p", [Msg]),
    {noreply, State}.

terminate(shutdown, _State) ->
    ok;

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

duration(#state{connect_time=undefined}) ->
    "undefined";
duration(#state{connect_time=T0}) ->
    US = timer:now_diff(os:timestamp(), T0),
    io_lib:format("~f", [US / 1000000]).
