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
-export([resolve_host/1, parse_msg/1, filter/2, formatted_utc_date/0, format/1, field_val/2, field_val/3, parse_redis_url/1]).

-include_lib("logplex.hrl").

resolve_host(Host) when is_binary(Host) ->
    case inet:getaddr(binary_to_list(Host), inet) of
        {ok, Ip} -> Ip;
        _ -> undefined
    end.

parse_msg(Msg) when is_binary(Msg) ->
    case re:run(Msg, "^<(\\d+)>(\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (.*)", [{capture, all_but_first, binary}]) of
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

formatted_utc_date() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = Local = erlang:localtime(),
    UTC = erlang:universaltime(),
    {_, {Offset, _, _}} = calendar:time_difference(Local, UTC),
    DateFormat = fun(Int) -> string:right(integer_to_list(Int), 2, $0) end,
    io_lib:format("~w-~s-~sT~s:~s:~s-~s:00", [Year, DateFormat(Month), DateFormat(Day), DateFormat(Hour), DateFormat(Min), DateFormat(Sec), DateFormat(Offset)]).

format(Msg) when is_record(Msg, msg) ->
    Ps =
        case Msg#msg.ps of
            undefined -> <<>>;
            _ -> [<<"[">>, Msg#msg.ps, <<"]">>]
        end,
    iolist_to_binary([Msg#msg.time, <<" ">>, Msg#msg.source, Ps, <<": ">>, Msg#msg.content, <<"\n">>]);

format(_Msg) ->
    "".

field_val(Key, Fields) ->
    field_val(Key, Fields, undefined).

field_val(Key, [{ok, Key}, {ok, Val} | _Tail], _Default) ->
    Val;

field_val(Key, [_, _ | Tail], Default) ->
    field_val(Key, Tail, Default);

field_val(_Key, _, Default) ->
    Default.

parse_redis_url(Url) ->
    case redis_uri:parse(Url) of
        {redis, UserInfo, Host, Port, _Path, _Query} ->
            Pass = 
                case UserInfo of
                    "" -> undefined;
                    Val -> list_to_binary(Val)
                end,
            {ok, Ip} = inet:getaddr(Host, inet),
            [{ip, Ip}, {port, Port}, {pass, Pass}];
        _ ->
            [{ip, "127.0.0.1"}, {port, 6379}]
    end.