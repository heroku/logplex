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
-module(http_handler).
-behaviour(cowboy_http_handler).
-export([opts/0, init/3, handle/2, terminate/2]).

-include_lib("http.hrl").

opts() ->
    [100,
     cowboy_tcp_transport,
     [{port, 8080}],
     cowboy_http_protocol,
     [{dispatch, [{'_', [
        {[<<"channels">>, '_', <<"logs">>], http_handler, []},
        {[<<"logs">>], http_handler, []}
     ]}]}]
    ].

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    case cowboy_http_req:header('Transfer-Encoding', Req) of
        {<<"chunked">>, _Req} ->
            {ContentType, _Req} = cowboy_http_req:header('Content-Type', Req),
            {Token, _Req} = cowboy_http_req:header(<<"Token">>, Req),
            ChannelId =
                case cowboy_http_req:path(Req) of
                    {[<<"channels">>, BinCh, <<"logs">>], _Req} -> list_to_integer(binary_to_list(BinCh));
                    {[<<"logs">>], _Req} -> undefined
                end,
            {ok, Req2} = loop(ChannelId, Token, ContentType, Req, Req#http_req.buffer),
            {ok, Req2#http_req{buffer = <<>>}, State};
        {_TransferEncoding, _Req} ->
            {ok, Req2} = cowboy_http_req:reply(415, [], <<"Incorrect Transfer-Encoding">>, Req),
            {ok, Req2#http_req{buffer = <<>>}, State}
    end.

terminate(_Req, _State) ->
    ok.

loop(ChannelId, Token, ContentType, Req, Buffer) ->
    case parse_msgs(Buffer, Token, ContentType) of
        {ok, Rest} ->
            Sock = Req#http_req.socket,
            inet:setopts(Sock, [{active, once}]),
            receive
                {tcp, Sock, Data} ->
                    loop(ChannelId, Token, ContentType, Req, <<Rest/binary, Data/binary>>);
                {tcp_closed, Sock} ->
                    cowboy_http_req:reply(200, [], <<"OK">>, Req);
                {tcp_error, Sock, _Reason} ->
                    cowboy_http_req:reply(200, [], <<"OK">>, Req);
                _Other ->
                    cowboy_http_req:reply(500, [], <<"Platform Error">>, Req)
            after 5 * 60 * 1000 ->
                cowboy_http_req:reply(200, [], <<"OK">>, Req)
            end;
        {error, closed} ->
            cowboy_http_req:reply(200, [], <<"OK">>, Req);
        _Err ->
            cowboy_http_req:reply(500, [], <<"Platform Error">>, Req)
    end.

process_msg(Props, Token) ->
    logplex_stats:incr(message_received),
    logplex_realtime:incr(message_received),
    logplex_queue:in(logplex_work_queue, iolist_to_binary([
        <<"<1>1 ">>, proplists:get_value(<<"timestamp">>, Props, <<"null">>),
        <<" host ">>, Token, <<" ">>, proplists:get_value(<<"ps">>, Props, <<"null">>),
        <<" - - ">>, proplists:get_value(<<"msg">>, Props, <<>>)
    ])).

parse_msgs(<<>>, _Token, _ContentType) ->
    {ok, <<>>};

parse_msgs(<<"\n">>, _Token, _ContentType) ->
    {ok, <<>>};

parse_msgs(<<"\r\n">>, _Token, _ContentType) ->
    {ok, <<>>};

parse_msgs(Data, Token, ContentType) ->
    case read_size(Data) of
        {ok, 0, _Rest} ->
            {error, closed};
        {ok, Size, Rest} ->
            case read_chunk(Rest, Size) of
                {ok, <<"\n">>, Rest1} ->
                    parse_msgs(Rest1, Token, ContentType);
                {ok, <<"\r\n">>, Rest1} ->
                    parse_msgs(Rest1, Token, ContentType);
                {ok, Chunk, Rest1} ->
                    case (catch mochijson2:decode(Chunk)) of
                        {struct, Props} ->
                            process_msg(Props, Token),
                            parse_msgs(Rest1, Token, ContentType);
                        {'EXIT', Err} ->
                            Err;
                        Err ->
                            Err
                    end;
                eof ->
                    {ok, Data};
                Err ->
                    Err
            end;
        eof ->
            {ok, Data};
        Err ->
            Err
    end.

read_size(Data) ->
    case read_size(Data, [], true) of
        {ok, Line, Rest} ->
            case io_lib:fread("~16u", Line) of
                {ok, [Size], []} ->
                    {ok, Size, Rest};
                _ ->
                    {error, {poorly_formatted_size, Line}}
            end;
        Err ->
            Err
    end.

read_size(<<>>, _, _) ->
    eof;

read_size(<<"\n", Rest/binary>>, Acc, _) ->
    {ok, lists:reverse(Acc), Rest};

read_size(<<"\r\n", Rest/binary>>, Acc, _) ->
    {ok, lists:reverse(Acc), Rest};

read_size(<<$;, Rest/binary>>, Acc, _) ->
    read_size(Rest, Acc, false);

read_size(<<C, Rest/binary>>, Acc, AddToAcc) ->
    case AddToAcc of
        true ->
            read_size(Rest, [C|Acc], AddToAcc);
        false ->
            read_size(Rest, Acc, AddToAcc)
    end.

read_chunk(Data, Size) ->
    case Data of
        <<Chunk:Size/binary, "\n", Rest/binary>> ->
            {ok, Chunk, Rest};
        <<Chunk:Size/binary, "\r\n", Rest/binary>> ->
            {ok, Chunk, Rest};
        <<_Chunk:Size/binary, _Rest/binary>> when size(_Rest) >= 2 ->
            {error, malformed_chunk};
        _ ->
            eof
    end.
