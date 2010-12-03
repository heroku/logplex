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
-module(logplex_api).
-export([loop/1, start_link/0, stop/0]).

-include_lib("logplex.hrl").

-define(HDR, [{"Content-Type", "text/html"}]).

start_link() ->
    Port =
        case os:getenv("HTTP_PORT") of
            false -> 80;
            Val -> list_to_integer(Val)
        end,
    Opts = [
        {ip, "0.0.0.0"},
        {port, Port},
        {backlog, 1024},
        {loop, {logplex_api, loop}},
        {name, logplex_api}
    ],
    io:format("START API~n"),
    mochiweb_http:start(Opts).

stop() ->
    LSock = mochiweb_socket_server:get(?MODULE, listen),
    gen_tcp:close(LSock),
    ok.

loop(Req) ->
    Method = Req:get(method),
    Path = Req:get(path),
    try
        {Code, Body} = serve(handlers(), Method, Path, Req),
        io:format("logpex_api method=~p path=~s resp_code=~w body=~1000p~n", [Method, Path, Code, Body]),
        Req:respond({Code, ?HDR, Body})
    catch 
        exit:normal ->
            ok;
        Class:Exception ->
            io:format("logplex_api method=~p path=~s exception=~1000p:~1000p~n", [Method, Path, Class, Exception]),
            Req:respond({500, ?HDR, ""})
    end.

handlers() ->
    [{['GET', "/healthcheck"], fun(Req, _Match) ->
        authorize(Req),
        Count = logplex_stats:healthcheck(),
        not is_integer(Count) andalso exit({expected_integer, Count}),
        {200, integer_to_list(Count)}
    end},

    {['POST', "/channels$"], fun(Req, _Match) ->
        authorize(Req),
        {struct, Params} = mochijson2:decode(Req:recv_body()),

        ChannelName = proplists:get_value(<<"name">>, Params),
        ChannelName == undefined andalso error_resp(400, <<"'name' post param missing">>),

        AppId = proplists:get_value(<<"app_id">>, Params),
        AppId == undefined andalso error_resp(400, <<"'app_id' post param missing">>),

        Addon = proplists:get_value(<<"addon">>, Params),
        Addon == undefined andalso error_resp(400, <<"'addon' post param missing">>),

        ChannelId = logplex_channel:create(ChannelName, AppId, Addon),
        not is_integer(ChannelId) andalso exit({expected_integer, ChannelId}),

        {201, integer_to_list(ChannelId)}
    end},

    {['DELETE', "/channels/(\\d+)$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        case logplex_channel:delete(list_to_integer(ChannelId)) of
            ok -> {200, ""};
            {error, not_found} -> {404, <<"not found">>}
        end
    end},

    {['POST', "/channels/(\\d+)/token$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        {struct, Params} = mochijson2:decode(Req:recv_body()),

        TokenName = proplists:get_value(<<"name">>, Params),
        TokenName == undefined andalso error_resp(400, <<"'name' post param missing">>),

        Token = logplex_token:create(list_to_integer(ChannelId), TokenName),
        not is_binary(Token) andalso exit({expected_binary, Token}),

        {201, Token}
    end},

    {['POST', "/sessions$"], fun(Req, _Match) ->
        authorize(Req),
        Body = Req:recv_body(),
        Session = logplex_session:create(Body),
        not is_binary(Session) andalso exit({expected_binary, Session}),
        {201, Session}
    end},

    {['GET', "/sessions/([\\w-]+)$"], fun(Req, [Session]) ->
        Body = logplex_session:lookup(list_to_binary("/sessions/" ++ Session)),
        not is_binary(Body) andalso error_resp(404, <<"not found">>),

        {struct, Data} = mochijson2:decode(Body),
        ChannelId0 = proplists:get_value(<<"channel_id">>, Data),
        not is_binary(ChannelId0) andalso error_resp(400, <<"'channel_id' missing">>),
        ChannelId = list_to_integer(binary_to_list(ChannelId0)),
        
        Filters = filters(Data),
        Num0 =
            case proplists:get_value(<<"num">>, Data) of
                undefined -> 20;
                BinNum -> list_to_integer(binary_to_list(BinNum))
            end,
        Num = ternary(Filters == [], Num0 - 1, -1),

        logplex_stats:incr(session_accessed),

        Logs = logplex_channel:logs(ChannelId, Num),
        Socket = Req:get(socket),
        Req:start_response({200, ?HDR}),

        [begin
            Msg1 = logplex_utils:parse_msg(Msg),
            logplex_utils:filter(Msg1, Filters) andalso gen_tcp:send(Socket, logplex_utils:format(Msg1))
        end || {ok, Msg} <- lists:reverse(Logs)],

        case proplists:get_value(<<"tail">>, Data) of
            undefined ->
                gen_tcp:close(Socket);
            _ ->
                logplex_stats:incr(session_tailed),
                logplex_tail:register(ChannelId),
                tail_loop(Socket, Filters)
        end,

        {200, ""}
    end},

    {['GET', "/channels/(\\d+)/info$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        Info = logplex_channel:info(list_to_integer(ChannelId)),
        not is_list(Info) andalso exit({expected_list, Info}),

        {200, iolist_to_binary(mochijson2:encode({struct, Info}))}
    end},

    {['POST', "/channels/(\\d+)/drains$"], fun(Req, [ChannelId]) ->
        authorize(Req),

        {struct, Data} = mochijson2:decode(Req:recv_body()),
        Host = proplists:get_value(<<"host">>, Data),
        Port = proplists:get_value(<<"port">>, Data),
        not is_binary(Host) andalso error_resp(400, <<"'host' param is missing">>),

        DrainId = logplex_drain:create(list_to_integer(ChannelId), Host, Port),
        case DrainId of
            Int when is_integer(Int) ->
                {201, ""};
            {error, already_exists} ->
                {400, <<"Drain already exists">>};
            {error, invalid_drain} ->
                {400, <<"Invalid drain">>}
        end        
    end},

    {['GET', "/channels/(\\d+)/drains$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        Drains = logplex_drain:lookup(list_to_integer(ChannelId)),
        not is_list(Drains) andalso exit({expected_list, Drains}),
        {200, mochijson2:encode({struct, Drains})}
    end},

    {['DELETE', "/channels/(\\d+)/drains$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        
        Data = Req:parse_qs(),
        Host = proplists:get_value("host", Data),
        Port = proplists:get_value("port", Data),
        Host == "" andalso error_resp(400, <<"'host' param is missing">>),
        
        case logplex_drain:delete(list_to_integer(ChannelId), list_to_binary(Host), Port) of
            ok -> {200, ""};
            {error, not_found} -> {404, <<"not found">>}
        end
    end}].

serve([], _Method, _Path, _Req) ->
    {404, <<"Not Found.">>};

serve([{[HMethod, Regexp], Fun}|Tail], Method, Path, Req) ->
    case re:run(Path, Regexp, [{capture, all_but_first, list}]) of
        {match, Captured} when HMethod == Method ->
            case catch Fun(Req, Captured) of
                {'EXIT', {Code, Body}} when is_integer(Code), is_binary(Body) ->
                    {Code, Body};
                {'EXIT', {Code, Body}} when is_integer(Code), is_list(Body) ->
                    {Code, Body};
                {'EXIT', Err} ->
                    exit(Err);
                {Code, Body} when is_integer(Code), is_binary(Body) ->
                    {Code, Body};
                {Code, Body} when is_integer(Code), is_list(Body) ->
                    {Code, Body};
                Other ->
                    exit({unexpected, Other})
            end;
        _ ->
            serve(Tail, Method, Path, Req)
    end.

authorize(Req) ->
    AuthKey = os:getenv("LOGPLEX_AUTH_KEY"),
    case Req:get_header_value("Authorization") of
        AuthKey ->
            true;
        _ ->
            throw({401, <<"Not Authorized">>})
    end.

error_resp(RespCode, Body) ->
    error_resp(RespCode, Body, undefined).

error_resp(RespCode, Body, Error) ->
    Error =/= undefined andalso io:format("server exception: ~1000p~n", [Error]),
    throw({RespCode, Body}).

tail_loop(Socket, Filters) ->
    inet:setopts(Socket, [{packet, raw}, {active, once}]),
    receive
        {log, Msg} ->
            Msg1 = logplex_utils:parse_msg(Msg),
            logplex_utils:filter(Msg1, Filters) andalso gen_tcp:send(Socket, logplex_utils:format(Msg1)),
            tail_loop(Socket, Filters);
        {tcp_closed, Socket} ->
            ok
    end.

filters(Data) ->
    filters(Data, []).

filters([], Filters) ->
    Filters;

filters([{"ps", Ps}|Tail], Filters) ->
    Filters1 = [
        fun(Msg) ->
            Msg#msg.ps == Ps
        end | Filters],
    filters(Tail, Filters1);

filters([{"source", Source}|Tail], Filters) ->
    Filters1 = [
        fun(Msg) ->
            Msg#msg.source == Source
        end | Filters],
    filters(Tail, Filters1);

filters([_|Tail], Filters) ->
    filters(Tail, Filters).

ternary(true, A, _B) -> A;
ternary(_, _A, B) -> B.
