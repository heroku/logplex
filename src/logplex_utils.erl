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
-export([rpc/4, set_weight/1, setup_test_channel/2, resolve_host/1,
         parse_msg/1, filter/2, formatted_utc_date/0, format/1, field_val/2, field_val/3,
         empty_token/0, redis_opts/1, parse_redis_url/1, instance_name/0, heroku_domain/0]).

-include_lib("logplex.hrl").

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

setup_test_channel(ChannelName, AppId) when is_binary(ChannelName), is_integer(AppId) ->
    ChannelId = logplex_channel:create(ChannelName, AppId),
    timer:sleep(100),
    logplex_token:create(ChannelId, <<"app">>).

resolve_host(Host) when is_binary(Host) ->
    case inet:getaddr(binary_to_list(Host), inet) of
        {ok, Ip} -> Ip;
        _ -> undefined
    end;

resolve_host(_) ->
    undefined.

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

field_val(Key, [Key, Val | _Tail], _Default) ->
    Val;

field_val(Key, [_, _ | Tail], Default) ->
    field_val(Key, Tail, Default);

field_val(_Key, _, Default) ->
    Default.

empty_token() ->
    setelement(1, erlang:make_tuple(length(record_info(fields, token))+1, '_'), token).

redis_opts(ConfigVar) when is_list(ConfigVar) ->
    case os:getenv(ConfigVar) of
        false ->
            [{ip, "127.0.0.1"}, {port, 6379}];
        Url ->
            logplex_utils:parse_redis_url(Url)
    end.

parse_redis_url(Url) ->
    case redis_uri:parse(Url) of
        {redis, _User, Pass, Host, Port, _Path, _Query} ->
            {ok, Ip} = inet:getaddr(Host, inet),
            [{ip, Ip}, {port, Port}, {pass, list_to_binary(Pass)}];
        _ ->
            [{ip, "127.0.0.1"}, {port, 6379}]
    end.

instance_name() ->
    case get(instance_name) of
        undefined ->
            InstanceName = os:getenv("INSTANCE_NAME"),
            put(instance_name, InstanceName),
            InstanceName;
        InstanceName -> InstanceName
    end.

heroku_domain() ->
    case get(heroku_domain) of
        undefined ->
            Domain = 
                case os:getenv("HEROKU_DOMAIN") of
                    false -> <<"">>;
                    Val -> list_to_binary(Val)
                end,
            put(heroku_domain, Domain),
            Domain;
        Domain -> Domain
    end.
