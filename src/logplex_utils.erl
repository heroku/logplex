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
-module(logplex_utils).
-export([rpc/4, set_weight/1, resolve_host/1,
         parse_msg/1, filter/2, formatted_utc_date/0, format/1, field_val/2, field_val/3,
         format/4, format_utc_timestamp/0, format_utc_timestamp/1,
         parse_redis_url/1, nl/1, to_int/1]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

rpc(Node, M, F, A) when is_atom(Node), is_atom(M), is_atom(F), is_list(A) ->
    case net_adm:ping(Node) of
        pong ->
            Res = rpc:call(Node, M, F, A),
            io:format("~100p~n", [Res]);
        pang ->
            io:format("Failed to connect to ~p~n", [Node])
    end.

set_weight(Weight) when is_integer(Weight), Weight < 0 ->
    set_weight(0);

set_weight(Weight) when is_integer(Weight), Weight > 100 ->
    set_weight(100);

set_weight(Weight) when is_integer(Weight) ->
    redgrid:update_meta([{"weight", integer_to_list(Weight)}]).

resolve_host(Host) when is_binary(Host) ->
    case inet:getaddr(binary_to_list(Host), inet) of
        {ok, Ip} -> Ip;
        _ -> undefined
    end;

resolve_host(_) ->
    undefined.

parse_msg(Msg) when is_binary(Msg) ->
    case re:run(Msg, "^<(\\d+)>(\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (-|(?:\\[(?:[^\\]]|\\\\])+\\])+) (.*)",
                [dotall, {capture, all_but_first, binary}]) of
        {match, [_PriFac, _Lines, Time, _Host, Source, Ps, _, _, Content]} ->
            #msg{time=Time, source=Source, ps=Ps, content=Content};
        _ ->
            undefined
    end.

filter(_Msg, []) -> true;
filter(Msg, [Fun|Tail]) ->
    case Fun(Msg) of
        true -> filter(Msg, Tail);
        _ -> false
    end.

format_utc_timestamp() ->
    format_utc_timestamp(os:timestamp()).

format_utc_timestamp({_, _, Micro}=TS) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(TS),
    % 2012-04-23T18:25:43.511Z
    io_lib:format("~w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~6..0wZ",
                  [Year, Month, Day, Hour, Minute, Second, Micro]).

formatted_utc_date() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = Local = erlang:localtime(),
    UTC = erlang:universaltime(),
    {_, {Offset, _, _}} = calendar:time_difference(Local, UTC),
    DateFormat = fun(Int) -> string:right(integer_to_list(Int), 2, $0) end,
    io_lib:format("~w-~s-~sT~s:~s:~s-~s:00", [Year, DateFormat(Month), DateFormat(Day), DateFormat(Hour), DateFormat(Min), DateFormat(Sec), DateFormat(Offset)]).

format(#msg{ps = Ps, time = Time, source = Source, content = Content}) ->
    format(Ps, Time, Source, Content);
format(_Msg) ->
    "".

format(Ps, Time, Source, Content) ->
    Ps1 =
        case Ps of
            undefined -> <<>>;
            _ -> [<<"[">>, Ps, <<"]">>]
        end,
    iolist_to_binary([Time, <<" ">>, Source, Ps1, <<": ">>, nl(Content)]).


-spec nl(iolist() | binary()) -> iolist().
nl(<<>>) -> <<"\n">>;
nl(Msg) when is_binary(Msg) ->
    case binary:at(Msg, byte_size(Msg)-1) of
        $\n -> [Msg];
        _ -> [Msg, $\n]
    end;
nl(Msg) when is_list(Msg) ->
    nl(iolist_to_binary(Msg)).

field_val(Key, Fields) ->
    field_val(Key, Fields, undefined).

field_val(Key, [Key, Val | _Tail], _Default) ->
    Val;

field_val(Key, [_, _ | Tail], Default) ->
    field_val(Key, Tail, Default);

field_val(_Key, _, Default) ->
    Default.

to_int(List) when is_list(List) ->
    list_to_integer(List);
to_int(Bin) when is_binary(Bin) ->
    to_int(binary_to_list(Bin));
to_int(Int) when is_integer(Int) ->
    Int.

parse_redis_url(Url) ->
    case redis_uri:parse(Url) of
        {redis, _User, Pass, Host, Port, _Path, _Query} ->
            {ok, Ip} = inet:getaddr(Host, inet),
            [{ip, Ip}, {port, Port}, {pass, format_password(Pass)}];
        _ ->
            [{ip, "127.0.0.1"}, {port, 6379}]
    end.

format_password([]) -> undefined;
format_password(L=[_|_]) -> iolist_to_binary(L).
