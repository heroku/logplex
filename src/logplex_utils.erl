-module(logplex_utils).
-export([parse_msg/1, filter/2, format/1, field_val/2, field_val/3]).

-include_lib("logplex.hrl").

parse_msg(Msg) when is_binary(Msg) ->
    case re:run(Msg, "^<(\\d+)>(\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (\\S+) (.*)", [{capture, all_but_first, list}]) of
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

format(Msg) when is_record(Msg, msg) ->
    Ps =
        case Msg#msg.ps of
            undefined -> "";
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