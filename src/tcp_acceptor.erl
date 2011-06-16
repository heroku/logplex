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

-record(state, {accept = true, buffer = <<>>, regex}).

start_link(Port) ->
    gen_nb_server:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port], State) ->
    {ok, RE} = re:compile("^(\\d+) "),
    case gen_nb_server:add_listen_socket({"0.0.0.0", Port}, State) of
        {ok, State1} ->
            {ok, gen_nb_server:store_cb_state(#state{regex=RE}, State1)};
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
    MyState = gen_nb_server:get_cb_state(State),
    Accept = MyState#state.accept,
    RE = MyState#state.regex,
    {ok, Rest} = parse(Accept, RE, <<(MyState#state.buffer)/binary, Packet/binary>>),
    inet:setopts(Sock, [{active, once}]),
    {noreply, gen_nb_server:store_cb_state(MyState#state{buffer=Rest}, State)};

handle_info({_From, stop_accepting}, State) ->
    MyState = gen_nb_server:get_cb_state(State),
    {noreply, gen_nb_server:store_cb_state(MyState#state{accept=false}, State)}; 

handle_info({_From, start_accepting}, State) ->
    MyState = gen_nb_server:get_cb_state(State),
    {noreply, gen_nb_server:store_cb_state(MyState#state{accept=true}, State)}; 

handle_info(_Info, State) ->
    {noreply, State}.

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
