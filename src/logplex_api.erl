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

-include("logplex.hrl").
-include("logplex_logging.hrl").

-define(HDR, [{"Content-Type", "text/html"}]).

start_link() ->
    Port =
        case os:getenv("HTTP_PORT") of
            false -> ?HTTP_PORT;
            Val -> list_to_integer(Val)
        end,
    Opts = [
        {ip, "0.0.0.0"},
        {port, Port},
        {backlog, 1024},
        {max, 100000},
        {loop, {logplex_api, loop}},
        {name, logplex_api}
    ],

    wait_for_nsync(),
    ?INFO("at=start", []),
    mochiweb_http:start(Opts).

stop() ->
    LSock = mochiweb_socket_server:get(?MODULE, listen),
    gen_tcp:close(LSock),
    ok.

-spec loop(term()) -> no_return().
loop(Req) ->
    Start = os:timestamp(),
    Method = Req:get(method),
    Path = Req:get(path),
    ChannelId = header_value(Req, "Channel", ""),
    try
        {Code, Body} = serve(handlers(), Method, Path, Req),
        Time = timer:now_diff(os:timestamp(), Start) div 1000,
        ?INFO("at=request channel_id=~s method=~p path=~s"
              " resp_code=~w time=~w body=~s",
              [ChannelId, Method, Path, Code, Time, Body]),
        Req:respond({Code, ?HDR, Body}),
        exit(normal)
    catch
        exit:normal ->
            exit(normal);
        Class:Exception ->
            Time1 = timer:now_diff(os:timestamp(), Start) div 1000,
            ?ERR("channel_id=~s method=~p path=~s "
                 "time=~w exception=~1000p:~1000p stack=~1000p",
                 [ChannelId, Method, Path, Time1, Class, Exception,
                  erlang:get_stacktrace()]),
            Req:respond({500, ?HDR, ""}),
            exit(normal)
    end.

handlers() ->
    [{['GET', "/healthcheck"], fun(Req, _Match) ->
        authorize(Req),

        [throw({500, io_lib:format("Zero ~p child processes running", [Worker])}) || {Worker, 0} <- logplex_stats:workers()],

        RegisteredMods = [logplex_realtime, logplex_stats, logplex_tail, logplex_shard, tcp_acceptor],
        [(whereis(Name) == undefined orelse not is_process_alive(whereis(Name))) andalso throw({500, io_lib:format("Process dead: ~p", [Name])}) || Name <- RegisteredMods],

        Count = logplex_stats:healthcheck(),
        not is_integer(Count) andalso throw({500, io_lib:format("Increment healthcheck counter failed: ~p", [Count])}),

        {200, <<"OK">>}
    end},

    {['POST', "/load$"], fun(Req, _Match) ->
        authorize(Req),
        Body = Req:recv_body(),
        Modules = mochijson2:decode(Body),
        not is_list(Modules) andalso exit({expected_list}),

        {RespCode, Json} = lists:foldl(fun(Module, {Code, Acc}) ->
            Module1 = binary_to_atom(Module, latin1),
            case c:l(Module1) of
                {module, _} when Code == 200 -> {200, Acc};
                {module, _} -> {Code, Acc};
                {error, Reason} -> {400, [{Module, atom_to_binary(Reason, latin1)}|Acc]}
            end
        end, {200, []}, Modules),

        {RespCode, iolist_to_binary(mochijson2:encode(Json))}
    end},

    {['POST', "/channels$"], fun(Req, _Match) ->
        authorize(Req),
        Body = Req:recv_body(),
        {struct, Params} = mochijson2:decode(Body),

        ChannelId = logplex_channel:create(),
        not is_integer(ChannelId) andalso exit({expected_integer, ChannelId}),

        case proplists:get_value(<<"tokens">>, Params) of
            List when length(List) > 0 ->
                Tokens = [
                    {TokenName, logplex_token:create(ChannelId, TokenName)}
                || TokenName <- List],

                Info = [{channel_id, ChannelId}, {tokens, Tokens}],
                {201, iolist_to_binary(mochijson2:encode({struct, Info}))};
            _ ->
                {201, integer_to_list(ChannelId)}
        end
    end},

    {['POST', "/channels/(\\d+)/addon$"], fun(Req, [_ChannelId]) ->
        authorize(Req),
        {200, <<"OK">>}
    end},

    {['DELETE', "/channels/(\\d+)$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        case logplex_channel:delete(list_to_integer(ChannelId)) of
            ok -> {200, <<"OK">>};
            {error, not_found} -> {404, <<"Not found">>}
        end
    end},

    {['POST', "/channels/(\\d+)/token$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        {struct, Params} = mochijson2:decode(Req:recv_body()),

        TokenName = proplists:get_value(<<"name">>, Params),
        TokenName == undefined andalso error_resp(400, <<"'name' post param missing">>),

        A = os:timestamp(),
        Token = logplex_token:create(list_to_integer(ChannelId), TokenName),
        B = os:timestamp(),
        not is_binary(Token) andalso exit({expected_binary, Token}),

        ?INFO("at=create_token name=~s channel_id=~s time=~w~n",
            [TokenName, ChannelId, timer:now_diff(B,A) div 1000]),

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
        proplists:get_value("srv", Req:parse_qs()) == undefined
            andalso error_resp(400, <<"[Error]: Please update your Heroku client to the most recent version. If this error message persists then uninstall the Heroku client gem completely and re-install.\n">>),
        Body = logplex_session:lookup(list_to_binary("/sessions/" ++ Session)),
        not is_binary(Body) andalso error_resp(404, <<"Not found">>),

        {struct, Data} = mochijson2:decode(Body),
        ChannelId0 = proplists:get_value(<<"channel_id">>, Data),
        not is_binary(ChannelId0) andalso error_resp(400, <<"'channel_id' missing">>),
        ChannelId = list_to_integer(binary_to_list(ChannelId0)),

        logplex_stats:incr(session_accessed),

        Filters = filters(Data),

        Num0 =
            case proplists:get_value(<<"num">>, Data) of
                undefined -> 100;
                BinNum -> list_to_integer(binary_to_list(BinNum))
            end,
        Num = ternary(Filters == [], Num0 - 1, -1),

        Logs = logplex_channel:logs(ChannelId, Num),
        Logs == {error, timeout} andalso error_resp(500, <<"timeout">>),
        not is_list(Logs) andalso exit({expected_list, Logs}),

        Socket = Req:get(socket),
        Req:start_response({200, ?HDR}),

        inet:setopts(Socket, [{nodelay, true}, {packet_size, 1024 * 1024}, {recbuf, 1024 * 1024}]),

        filter_and_send_logs(Socket, Logs, Filters, Num0),

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

    {['POST', "/channels/(\\d+)/drains/tokens$"], fun(Req, [ChannelId]) ->
        authorize(Req),

        {ok, DrainId, Token} = logplex_drain:reserve_token(),
        logplex_drain:cache(DrainId, Token, list_to_integer(ChannelId)),
        Resp = [
            {id, DrainId},
            {token, Token},
            {msg, <<"Successfully reserved drain token">>}],
        {201, iolist_to_binary(mochijson2:encode({struct, Resp}))}
    end},

    {['POST', "/channels/(\\d+)/drains/(\\d+)$"], fun(Req, [ChannelId, DrainId]) ->
        authorize(Req),

        {struct, Data} = mochijson2:decode(Req:recv_body()),
        Host = proplists:get_value(<<"host">>, Data),
        Port = proplists:get_value(<<"port">>, Data),
        not is_binary(Host) andalso error_resp(400, <<"'host' param is missing">>),
        Host == <<"localhost">> andalso error_resp(400, <<"Invalid drain">>),
        Host == <<"127.0.0.1">> andalso error_resp(400, <<"Invalid drain">>),

        case logplex_drain:create(list_to_integer(DrainId), list_to_integer(ChannelId), Host, Port) of
            #drain{id=Id} ->
                Resp = [
                    {id, Id},
                    {msg, list_to_binary(io_lib:format("Successfully added drain syslog://~s:~p", [Host, Port]))}
                ],
                {201, iolist_to_binary(mochijson2:encode({struct, Resp}))};
            {error, not_found} ->
                {404, <<"Failed to create drain">>};
            {error, invalid_drain} ->
                {400, io_lib:format("Invalid drain syslog://~s:~p", [Host, Port])}
        end  
    end},

    {['POST', "/channels/(\\d+)/drains$"], fun(Req, [ChannelId]) ->
        authorize(Req),

        {struct, Data} = mochijson2:decode(Req:recv_body()),
        Host = proplists:get_value(<<"host">>, Data),
        Port = proplists:get_value(<<"port">>, Data),
        not is_binary(Host) andalso error_resp(400, <<"'host' param is missing">>),
        Host == <<"localhost">> andalso error_resp(400, <<"Invalid drain">>),
        Host == <<"127.0.0.1">> andalso error_resp(400, <<"Invalid drain">>),

        case logplex_channel:lookup_drains(list_to_integer(ChannelId)) of
            List when length(List) >= ?MAX_DRAINS ->
                {400, "You have already added the maximum number of drains allowed"};
            _ ->
                {ok, DrainId, Token} = logplex_drain:reserve_token(),
                case logplex_drain:create(DrainId, Token, list_to_integer(ChannelId), Host, Port) of
                    #drain{} ->
                        Resp = [
                            {id, DrainId},
                            {token, Token},
                            {msg, list_to_binary(io_lib:format("Successfully added drain syslog://~s:~p", [Host, Port]))}
                        ],
                        {201, iolist_to_binary(mochijson2:encode({struct, Resp}))};
                    {error, already_exists} ->
                        {400, io_lib:format("Drain syslog://~s:~p already exists", [Host, Port])};
                    {error, invalid_drain} ->
                        {400, io_lib:format("Invalid drain syslog://~s:~p", [Host, Port])}
                end
        end
    end},

    {['GET', "/channels/(\\d+)/drains$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        Drains = logplex_channel:lookup_drains(list_to_integer(ChannelId)),
        not is_list(Drains) andalso exit({expected_list, Drains}),
        
        Drains1 = [{struct, [{host, Host}, {port, Port}]} || #drain{host=Host, port=Port} <- Drains],
        {200, iolist_to_binary(mochijson2:encode(Drains1))}
    end},

    {['DELETE', "/channels/(\\d+)/drains$"], fun(Req, [ChannelId]) ->
        authorize(Req),

        Data = Req:parse_qs(),
        Host = proplists:get_value("host", Data),
        Port = proplists:get_value("port", Data),
        Host == "" andalso error_resp(400, <<"'host' param is empty">>),

        case Host == undefined andalso Port == undefined of
            true ->
                logplex_drain:clear_all(list_to_integer(ChannelId)),
                {200, <<"Cleared all drains">>};
            false ->
                case logplex_drain:delete(list_to_integer(ChannelId), list_to_binary(Host), Port) of
                    ok -> {200, io_lib:format("Successfully removed drain syslog://~s:~s", [Host, Port])};
                    {error, not_found} -> {404, io_lib:format("Drain syslog://~s:~s does not exist", [Host, Port])}
                end
        end
    end}].

serve([], _Method, _Path, _Req) ->
    {404, <<"Not found">>};

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
    throw({RespCode, Body}).

filter_and_send_logs(Socket, Logs, [], _Num) ->
    [gen_tcp:send(Socket, logplex_utils:format(logplex_utils:parse_msg(Msg))) || Msg <- lists:reverse(Logs)];

filter_and_send_logs(Socket, Logs, Filters, Num) ->
    filter_and_send_logs(Socket, Logs, Filters, Num, []).

filter_and_send_logs(Socket, Logs, _Filters, Num, Acc) when Logs == []; Num == 0 ->
    gen_tcp:send(Socket, Acc);

filter_and_send_logs(Socket, [Msg|Tail], Filters, Num, Acc) ->
    Msg1 = logplex_utils:parse_msg(Msg),
    case logplex_utils:filter(Msg1, Filters) of
        true ->
            filter_and_send_logs(Socket, Tail, Filters, Num-1, [logplex_utils:format(Msg1)|Acc]);
        false ->
            filter_and_send_logs(Socket, Tail, Filters, Num, Acc)
    end.
    
tail_loop(Socket, Filters) ->
    receive
        {log, Msg} ->
            Msg1 = logplex_utils:parse_msg(Msg),
            logplex_utils:filter(Msg1, Filters) andalso gen_tcp:send(Socket, logplex_utils:format(Msg1)),
            tail_loop(Socket, Filters);
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, _Reason} ->
            ok
    end.

filters(Data) ->
    filters(Data, []).

filters([], Filters) ->
    Filters;

filters([{<<"ps">>, Ps}|Tail], Filters) ->
    Size = size(Ps),
    Filters1 = [
        fun(Msg) ->
            case Msg#msg.ps of
                Ps -> true;
                <<Ps:Size/binary, ".", _/binary>> -> true;
                _ -> false
            end
        end | Filters],
    filters(Tail, Filters1);

filters([{<<"source">>, Source}|Tail], Filters) ->
    Filters1 = [
        fun(Msg) ->
            Msg#msg.source == Source
        end | Filters],
    filters(Tail, Filters1);

filters([_|Tail], Filters) ->
    filters(Tail, Filters).

ternary(true, A, _B) -> A;
ternary(_, _A, B) -> B.

header_value(Req, Key, Default) ->
    case Req:get_header_value(Key) of
        undefined -> Default;
        Val -> Val
    end.

wait_for_nsync() ->
    case application:get_env(logplex, nsync_loaded) of
        {ok, true} -> ok;
        _ ->
            timer:sleep(1000),
            wait_for_nsync()
    end.
