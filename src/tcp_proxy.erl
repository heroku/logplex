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

-record(state, {sock, buffer = <<>>, regex}).

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
    process_flag(trap_exit, true),
    {ok, RE} = re:compile("^(\\d+) "),
    {ok, #state{regex=RE}}.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast({set_socket, CSock}, State) ->
    {noreply, State#state{sock=CSock}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Packet}, #state{sock=Sock, buffer=Buffer, regex=RE}=State) ->
    {ok, Rest} = parse(true, RE, <<Buffer/binary, Packet/binary>>),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{buffer=Rest}};

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

%% internal
parse(_Accept, _RE, <<>>) ->
    {ok, <<>>};

parse(Accept, RE, Packet) ->
    case re:run(Packet, RE, [{capture, all_but_first, binary}]) of
        nomatch ->
            {ok, _Line, Rest} = read_line(Packet, []),
            parse(Accept, RE, Rest);
        {match, [Len]} ->
            LSize = size(Len),
            Size = list_to_integer(binary_to_list(Len)),
            case Packet of
                <<Len:LSize/binary, 32/integer, Msg:Size/binary, Rest1/binary>> ->
                    process_msg(Accept, Msg),
                    parse(Accept, RE, Rest1);
                _ ->
                    {ok, Packet}
            end
    end.

read_line(<<$\n, Rest/binary>>, Acc) ->
    {ok, iolist_to_binary(lists:reverse([$\n|Acc])), Rest};

read_line(<<>>, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), <<>>};

read_line(<<C, Rest/binary>>, Acc) ->
    read_line(Rest, [C|Acc]).

process_msg(Accept, Msg) ->
    logplex_stats:incr(message_received),
    logplex_realtime:incr(message_received),
    case Accept of
        true ->
            logplex_queue:in(logplex_work_queue, Msg);
        false ->
            logplex_stats:incr(message_dropped),
            logplex_realtime:incr(message_dropped)
    end.

