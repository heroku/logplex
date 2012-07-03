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
-define(JSON_CONTENT, [{"Content-Type", "application/json"}]).

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
        {Code, Hdr, Body} = case serve(handlers(), Method, Path, Req) of
                                {C, B} ->
                                    {C, ?HDR, B};
                                {_C, _H, _B} = Resp -> Resp
                            end,
        Time = timer:now_diff(os:timestamp(), Start) div 1000,
        ?INFO("at=request channel_id=~s method=~p path=~s"
              " resp_code=~w time=~w body=~s",
              [ChannelId, Method, Path, Code, Time, Body]),
        Req:respond({Code, Hdr, Body}),
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

    {['POST', "^/channels$"], fun(Req, _Match) ->
        authorize(Req),
        Body = Req:recv_body(),
        {struct, Params} = mochijson2:decode(Body),

        ChannelId = logplex_channel:create(),
        not is_integer(ChannelId) andalso exit({expected_integer, ChannelId}),

        Tokens =
            case proplists:get_value(<<"tokens">>, Params) of
                List when length(List) > 0 ->
                    [{TokenName, logplex_token:create(ChannelId, TokenName)} || TokenName <- List];
                _ ->
                    []
            end,
        Info = [{channel_id, ChannelId}, {tokens, Tokens}],
        {201, iolist_to_binary(mochijson2:encode({struct, Info}))}
    end},

    %% V2
    {['GET', "^/v2/channels/(\\d+)$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        case channel_info(api_v2, ChannelId) of
            not_found -> not_found_json();
            Info ->
                {200, ?JSON_CONTENT, mochijson2:encode({struct, Info})}
        end
    end},

    {['DELETE', "^/channels/(\\d+)$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        case logplex_channel:delete(list_to_integer(ChannelId)) of
            ok -> {200, <<"OK">>};
            {error, not_found} -> {404, <<"Not found">>}
        end
    end},

    %% V2
    {['DELETE', "^/v2/channels/(\\d+)$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        case logplex_channel:delete(list_to_integer(ChannelId)) of
            ok ->
                {200, <<>>};
            {error, not_found} ->
                json_error(404, <<"Not Found">>)
        end
    end},

    {['POST', "^/channels/(\\d+)/token$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        {struct, Params} = mochijson2:decode(Req:recv_body()),

        TokenName = proplists:get_value(<<"name">>, Params),
        TokenName == undefined andalso error_resp(400, <<"'name' post param missing">>),

        {Time, Token} = timer:tc(fun logplex_token:create/2,
                                 [list_to_integer(ChannelId), TokenName]),
        not is_binary(Token) andalso exit({expected_binary, Token}),

        ?INFO("at=create_token name=~s channel_id=~s time=~w~n",
            [TokenName, ChannelId, Time div 1000]),

        {201, Token}
    end},

    %% V2
    {['POST', "^/v2/channels/(\\d+)/tokens$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        {struct, Params} = mochijson2:decode(Req:recv_body()),

        Name = proplists:get_value(<<"name">>, Params),
        Name == undefined andalso
            error_resp(422, iolist_to_binary(mochijson2:encode({struct, [
                {error, <<"NAME is a required field">>}
            ]}))),

       {Time, Token} = timer:tc(fun logplex_token:create/2,
                                 [list_to_integer(ChannelId), Name]),
        not is_binary(Token) andalso exit({expected_binary, Token}),

        ?INFO("at=create_token name=~s channel_id=~s time=~w~n",
            [Name, ChannelId, Time div 1000]),

        {201, ?JSON_CONTENT,
         mochijson2:encode({struct, [{name, Name}, {token, Token}]})}
    end},

    {['POST', "^/sessions$"], fun(Req, _Match) ->
        authorize(Req),
        Body = Req:recv_body(),
        UUID = logplex_session:publish(Body),
        not is_binary(UUID) andalso exit({expected_binary, UUID}),
        {201, api_relative_url(api_v1, UUID)}
    end},

    %% V2
    {['POST', "^/v2/sessions$"], fun(Req, _Match) ->
        authorize(Req),
        Body = Req:recv_body(),
        UUID = logplex_session:publish(Body),
        not is_binary(UUID) andalso exit({expected_binary, UUID}),
        {201, ?JSON_CONTENT,
         mochijson2:encode({struct, [{url, api_relative_url(api_v2, UUID)}]})}
    end},

    {['GET', "^/sessions/([\\w-]+)$"], fun(Req, [Session]) ->
        proplists:get_value("srv", Req:parse_qs()) == undefined
            andalso error_resp(400, <<"[Error]: Please update your Heroku client to the most recent version. If this error message persists then uninstall the Heroku client gem completely and re-install.\n">>),
        Timeout = timer:seconds(logplex_app:config(session_lookup_timeout_s,
                                                   5)),
        Body = logplex_session:poll(list_to_binary(Session),
                                    Timeout),
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
                {ok, Buffer} =
                    logplex_tail_buffer:start_link(ChannelId, self()),
                try
                    tail_init(Socket, Buffer, Filters)
                after
                    exit(Buffer, shutdown)
                end
        end,

        {200, ""}
    end},

    %% V1
    {['GET', "^/channels/(\\d+)/info$"], fun(Req, [ChannelId]) ->
        authorize(Req),
        Info = channel_info(api_v1, ChannelId),
        not is_list(Info) andalso exit({expected_list, Info}),

        {200, iolist_to_binary(mochijson2:encode({struct, Info}))}
    end},

    {['POST', "^/channels/(\\d+)/drains/tokens$"], fun(Req, [ChannelId]) ->
        authorize(Req),

        {ok, DrainId, Token} = logplex_drain:reserve_token(),
        logplex_drain:cache(DrainId, Token, list_to_integer(ChannelId)),
        Resp = [{id, DrainId},
                {token, Token},
                {msg, <<"Successfully reserved drain token">>}],
        {201, iolist_to_binary(mochijson2:encode({struct, Resp}))}
    end},

    {['POST', "^/channels/(\\d+)/drains/(\\d+)$"], fun(Req, [_ChannelIdStr, _DrainIdStr]) ->
        authorize(Req),
        {501, <<"V1 Drain Creation API deprecated">>}
    end},

    {['POST', "^/v2/channels/(\\d+)/drains/(\\d+)$"], fun(Req, [ChannelIdStr, DrainIdStr]) ->
        authorize(Req),

        DrainId = list_to_integer(DrainIdStr),
        ChannelId = list_to_integer(ChannelIdStr),
        case logplex_drain:poll_token(DrainId) of
            {error, timeout} ->
                json_error(404, <<"Unknown drain.">>);
            Token when is_binary(Token) ->
                case logplex_drain:valid_uri(req_drain_uri(Req)) of
                    {error, What} ->
                        logplex_drain:delete_partial_drain(DrainId, Token),
                        Err = io_lib:format("Invalid drain destination: ~p",
                                            [What]),
                        json_error(422, Err);
                    {valid, _, URI} ->
                        case logplex_channel:can_add_drain(ChannelId) of
                            cannot_add_drain ->
                                logplex_drain:delete_partial_drain(DrainId, Token),
                                json_error(422, <<"You have already added the maximum number of drains allowed">>);
                            can_add_drain ->
                                case logplex_drain:create(DrainId, Token, ChannelId, URI) of
                                    {error, already_exists} ->
                                        json_error(409, <<"Already exists">>);
                                    {drain, _Id, Token} ->
                                        Resp = [
                                                {id, DrainId},
                                                {token, Token},
                                                {url, uri:to_binary(URI)}
                                               ],
                                        {201,?JSON_CONTENT,
                                         mochijson2:encode({struct, Resp})}
                                end
                        end
                end
        end
    end},

    {['POST', "^/channels/(\\d+)/drains$"], fun(Req, [_ChannelId]) ->
        authorize(Req),

        {501, <<"V1 Drain Creation API deprecated.">>}
    end},

    %% V2
    {['POST', "^/v2/channels/(\\d+)/drains$"], fun(Req, [ChannelIdStr]) ->
        ChannelId = list_to_integer(ChannelIdStr),
        case logplex_drain:valid_uri(req_drain_uri(Req)) of
            {error, What} ->
                Err = io_lib:format("Invalid drain destination: ~p",
                                    [What]),
                json_error(422, Err);
            {valid, _, URI} ->
                case logplex_channel:can_add_drain(ChannelId) of
                    cannot_add_drain ->
                        json_error(422, <<"You have already added the maximum number of drains allowed">>);
                    can_add_drain ->
                        {ok, DrainId, Token} = logplex_drain:reserve_token(),
                        case logplex_drain:create(DrainId, Token, ChannelId, URI) of
                            {error, already_exists} ->
                                json_error(409, <<"Already exists">>);
                            {drain, _Id, Token} ->
                                Resp = [
                                        {id, DrainId},
                                        {token, Token},
                                        {url, uri:to_binary(URI)}
                                       ],
                                {201,?JSON_CONTENT,
                                 mochijson2:encode({struct, Resp})}
                        end
                end
        end
    end},

    {['GET', "^/channels/(\\d+)/drains$"], fun(Req, [_ChannelId]) ->
        authorize(Req),
        {501, <<"V1 Drain API Deprecated.">>}
    end},

    {['DELETE', "^/channels/(\\d+)/drains$"], fun(Req, [_ChannelId]) ->
        authorize(Req),
       {501, <<"V1 Drain API Deprecated.">>}
    end},

    %% V2
    {['DELETE', "^/v2/channels/(\\d+)/drains/(\\S+)$"], fun(Req, [ChannelId, DrainId]) ->
        authorize(Req),
        Deletable = try
            ChannelIdInt = list_to_integer(ChannelId),
            Drain = logplex_drain:lookup(list_to_integer(DrainId)),
            ChannelIdInt =:= logplex_drain:channel_id(Drain)
            catch _:_ -> false end,
        case Deletable of
            false ->
                json_error(404, <<"Not found">>);
            true ->
                ok = logplex_drain:delete(list_to_integer(DrainId)),
                {200, <<>>}
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
                {Code, Body}
                  when is_integer(Code),
                       is_binary(Body) orelse is_list(Body) ->
                    {Code, Body};
                {Code, Hdrs, Body}
                  when is_integer(Code), is_list(Hdrs),
                       is_binary(Body) orelse is_list(Body) ->
                    {Code, Hdrs, Body};
                Other ->
                    exit({unexpected, Other})
            end;
        _ ->
            serve(Tail, Method, Path, Req)
    end.

authorize(Req) ->
    AuthKey = os:getenv("LOGPLEX_AUTH_KEY"),
    case Req:get_header_value("Authorization") of
        [$B, $a, $s, $i, $c, $  | Encoded] ->
            TrustedValues = [os:getenv("LOGPLEX_CORE_USERPASS"),
                             os:getenv("LOGPLEX_ION_USERPASS")],
            case basic_auth_is_valid(binary_to_list(base64:decode(list_to_binary(Encoded))), TrustedValues) of
                true ->
                    true;
                _ ->
                    throw({401, <<"Not Authorized">>})
            end;
        AuthKey ->
            true;
        _ ->
            throw({401, <<"Not Authorized">>})
    end.

basic_auth_is_valid(Val, [Val|_]) when is_list(Val), length(Val) > 0 ->
    true;

basic_auth_is_valid(Val, [_|Tail]) ->
    basic_auth_is_valid(Val, Tail);

basic_auth_is_valid(_, []) ->
    false.

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

tail_init(Socket, Buffer, Filters) ->
    inet:setopts(Socket, [{active, once}]),
    tail_loop(Socket, Buffer, Filters).

tail_loop(Socket, Buffer, Filters) ->
    logplex_tail_buffer:set_active(Buffer,
                                   tail_filter(Filters)),
    receive
        {logplex_tail_data, Buffer, Data} ->
            gen_tcp:send(Socket,
                         Data),
            tail_loop(Socket, Buffer, Filters);
        {tcp_data, Socket, _} ->
            inet:setopts(Socket, [{active, once}]),
            tail_loop(Socket, Buffer, Filters);
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, _Reason} ->
            ok
    end.

tail_filter(Filters) ->
    fun (Item) ->
            M = case Item of
                    {loss_indication, N, When} ->
                        #msg{time = logplex_syslog_utils:datetime(now),
                             source = <<"logplex">>,
                             ps = <<"1">>,
                             content=io_lib:format("Error L11 (Tail buffer overflow) -> "
                                                   "This tail session dropped ~p messages since ~s.",
                                                   [N, logplex_syslog_utils:datetime(When)]
                                                  )};
                    {msg, Msg} when is_binary(Msg) ->
                        logplex_utils:parse_msg(Msg)
                end,
            case logplex_utils:filter(M, Filters) of
                false -> skip;
                true -> {frame, logplex_utils:format(M)}
            end
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

channel_info(ApiVsn, ChannelId) when is_list(ChannelId) ->
    channel_info(ApiVsn, list_to_integer(ChannelId));
channel_info(ApiVsn, ChannelId) when is_integer(ChannelId) ->
    case logplex_channel:info(ChannelId) of
        {ChannelId, Tokens, Drains} ->
            [{channel_id, ChannelId},
             {tokens, lists:sort([ token_info(ApiVsn, Token)
                                   || Token = #token{} <- Tokens])},

             {drains, lists:sort([drain_info(ApiVsn, Drain)
                                  || Drain <- Drains,
                                     logplex_drain:has_valid_uri(Drain)])}];
        not_found -> not_found
    end.

token_info(api_v1, #token{name=Name, id=Token}) ->
    {Name, Token};
token_info(api_v2, #token{name=Name, id=Token}) ->
    [{name, Name},
     {token, Token}].

drain_info(api_v1, Drain) ->
    uri:to_binary(logplex_drain:uri(Drain));
drain_info(api_v2, Drain) ->
    [{id, logplex_drain:id(Drain)},
     {token, logplex_drain:token(Drain)},
     {url, uri:to_binary(logplex_drain:uri(Drain))}].

not_found_json() ->
    Json = {struct, [{error, <<"Not found">>}]},
    {404, iolist_to_binary(mochijson2:encode(Json))}.

req_drain_uri(Req) ->
    {struct, Data} = mochijson2:decode(Req:recv_body()),
    case proplists:get_value(<<"url">>, Data) of
        undefined ->
            Port = proplists:get_value(<<"port">>, Data),
            case proplists:get_value(<<"host">>, Data) of
                undefined ->
                    {error, missing_host_param};
                Host ->
                    logplex_tcpsyslog_drain:uri(Host, Port)
            end;
        UrlString ->
            logplex_drain:parse_url(UrlString)
    end.

json_error(Code, Err) ->
    {Code, ?JSON_CONTENT,
     mochijson2:encode({struct, [{error, iolist_to_binary(Err)}]})}.

api_relative_url(_APIVSN, UUID) when is_binary(UUID) ->
    iolist_to_binary([<<"/sessions/">>, UUID]).
