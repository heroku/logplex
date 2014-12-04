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
-export([loop/1, start_link/0, child_spec/0, stop/0]).
-export([status/0, set_status/1]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-define(HDR, [{"Content-Type", "text/html"}]).
-define(JSON_CONTENT, [{"Content-Type", "application/json"}]).
-define(API_READONLY, <<"Logplex API temporarily in read-only mode">>).
-define(API_DISABLED, <<"Service Temporarily Disabled">>).

start_link() ->
    Port = logplex_utils:to_int(logplex_app:config(http_port)),
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

child_spec() ->
    {?MODULE, {?MODULE, start_link, []},
     permanent, 2000, worker, [?MODULE]}.

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
        Served = case serve(handlers(), Method, Path, Req, status()) of
                     {done,{C,D}} -> {done,{C,D}};
                     {C, B} -> {C, ?HDR, B};
                     {_C, _H, _B} = Resp -> Resp
                 end,
        Time = timer:now_diff(os:timestamp(), Start) div 1000,
        case Served of
            {Code, Hdr, Body} ->
                Req:respond({Code, Hdr, Body}),
                ?INFO("at=request channel_id=~s method=~p path=~s"
                    " resp_code=~w time=~w body=~s",
                    [ChannelId, Method, Path, Code, Time, Body]);
            {done,{Code,Details}} ->
                ?INFO("at=request channel_id=~s method=~p path=~s "
                      "resp_code=~w time=~w body=~s",
                      [ChannelId, Method, Path, Code, Time, Details])
        end,
        exit(normal)
    catch
        exit:normal ->
            exit(normal);
        Class:Exception ->
            Time1 = timer:now_diff(os:timestamp(), Start) div 1000,
            Req:respond({500, ?HDR, ""}),
            ?ERR("channel_id=~s method=~p path=~s "
                 "time=~w exception=~1000p:~1000p stack=~1000p",
                 [ChannelId, Method, Path, Time1, Class, Exception,
                  erlang:get_stacktrace()]),
            exit(normal)
    end.

handlers() ->
    [{['GET', "/healthcheck"], fun(Req, _Match, _Status) ->
        authorize(Req),

        RegisteredMods = [logplex_stats, logplex_tail, logplex_shard, tcp_acceptor],
        [(whereis(Name) == undefined orelse not is_process_alive(whereis(Name))) andalso throw({500, io_lib:format("Process dead: ~p", [Name])}) || Name <- RegisteredMods],

        Count = logplex_stats:healthcheck(),
        not is_integer(Count) andalso throw({500, io_lib:format("Increment healthcheck counter failed: ~p", [Count])}),

        {200, <<"OK">>}
    end},

    {['POST', "/load$"], fun(Req, _Match, _Status) ->
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

    {['POST', "^/channels$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                 (Req, _Match, _Status) ->
        authorize(Req),
        Body = Req:recv_body(),
        {struct, Params} = mochijson2:decode(Body),

        ChannelId = logplex_channel:create_id(),
        not is_integer(ChannelId) andalso exit({expected_integer, ChannelId}),

        Tokens =
            case proplists:get_value(<<"tokens">>, Params) of
                List when length(List) > 0 ->
                    [{TokenName, logplex_token:create(ChannelId, TokenName)}
                     || TokenName <- List];
                _ ->
                    []
            end,
        Info = [{channel_id, ChannelId}, {tokens, Tokens}],
        {201, iolist_to_binary(mochijson2:encode({struct, Info}))}
    end},

    %% V2
    {['GET', "^/v2/channels/(\\d+)$"], fun(Req, [ChannelId], _Status) ->
        authorize(Req),
        case channel_info(api_v2, ChannelId) of
            not_found -> not_found_json();
            Info ->
                {200, ?JSON_CONTENT, mochijson2:encode({struct, Info})}
        end
    end},

    {['DELETE', "^/channels/(\\d+)$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                          (Req, [ChannelId], _) ->
        authorize(Req),
        case logplex_channel:delete(list_to_integer(ChannelId)) of
            ok -> {200, <<"OK">>};
            {error, not_found} -> {404, <<"Not found">>}
        end
    end},

    %% V2
    {['DELETE', "^/v2/channels/(\\d+)$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                             (Req, [ChannelId], _) ->
        authorize(Req),
        case logplex_channel:delete(list_to_integer(ChannelId)) of
            ok ->
                {200, <<>>};
            {error, not_found} ->
                json_error(404, <<"Not Found">>)
        end
    end},

    {['POST', "^/channels/(\\d+)/token$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                              (Req, [ChannelId], _) ->
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
    {['POST', "^/v2/channels/(\\d+)/tokens$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                                  (Req, [ChannelId], _) ->
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

    {['POST', "^/sessions$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                 (Req, _Match, _) ->
        authorize(Req),
        Body = Req:recv_body(),
        UUID = logplex_session:publish(Body),
        not is_binary(UUID) andalso exit({expected_binary, UUID}),
        {201, iolist_to_binary([logplex_app:config(api_endpoint_url, ""),
                                <<"/sessions/">>, UUID, <<"?srv=srv">>])}
    end},

    %% V2
    {['POST', "^/v2/sessions$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                    (Req, _Match, _) ->
        authorize(Req),
        Body = Req:recv_body(),
        UUID = logplex_session:publish(Body),
        not is_binary(UUID) andalso exit({expected_binary, UUID}),
        {201, ?JSON_CONTENT,
         mochijson2:encode({struct, [{url, api_relative_url(api_v2, UUID)}]})}
    end},

    {['POST', "^/v2/canary-sessions$"], fun(Req, _Match, _) -> % canaries remain available
        authorize(Req),
        Body = Req:recv_body(),
        UUID = logplex_session:publish(Body),
        not is_binary(UUID) andalso exit({expected_binary, UUID}),
        {201, ?JSON_CONTENT,
         mochijson2:encode({struct, [{url, api_relative_url(canary, UUID)}]})}
    end},

    {['GET', "^/sessions/([\\w-]+)$"], fun(Req, [Session], _) ->
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
        NumBin = proplists:get_value(<<"num">>, Data, <<"100">>),
        Num = list_to_integer(binary_to_list(NumBin)),

        Logs = logplex_channel:logs(ChannelId, Num),
        Logs == {error, timeout} andalso error_resp(500, <<"timeout">>),
        not is_list(Logs) andalso exit({expected_list, Logs}),

        Socket = Req:get(socket),
        Header = case logplex_channel:lookup_flag(no_redis, ChannelId) of
                     no_redis -> ?HDR ++ [{"X-Heroku-Warning",
                                           logplex_app:config(no_redis_warning)}];
                     _ -> ?HDR
                 end,
        Req:start_response({200, Header}),

        inet:setopts(Socket, [{nodelay, true}, {packet_size, 1024 * 1024}, {recbuf, 1024 * 1024}]),

        filter_and_send_logs(Socket, Logs, Filters, Num),

        case {proplists:get_value(<<"tail">>, Data, tail_not_requested),
              logplex_channel:lookup_flag(no_tail, ChannelId)} of
            {tail_not_requested, _} ->
                end_chunked_response(Socket);
            {_, no_tail} ->
                gen_tcp:send(Socket, no_tail_warning()),
                end_chunked_response(Socket);
            _ ->
                ?INFO("at=tail_start channel_id=~p filters=~100p",
                      [ChannelId, Filters]),
                logplex_stats:incr(session_tailed),
                {ok, Buffer} =
                    logplex_tail_buffer:start_link(ChannelId, self()),
                try
                    tail_init(Socket, Buffer, Filters, ChannelId)
                after
                    ?INFO("at=tail_end channel_id=~p",
                          [ChannelId]),
                    exit(Buffer, shutdown)
                end
        end,

        {200, ""}
    end},

    {['GET', "^/v2/canary-fetch/([\\w-]+)$"], fun(Req, [Session], _) ->
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
        NumBin = proplists:get_value(<<"num">>, Data, <<"100">>),
        Num = list_to_integer(binary_to_list(NumBin)),

        Logs = logplex_channel:logs(ChannelId, Num),
        Logs == {error, timeout} andalso error_resp(500, <<"timeout">>),
        not is_list(Logs) andalso exit({expected_list, Logs}),

        Socket = Req:get(socket),
        inet:setopts(Socket, [{nodelay, true}, {packet_size, 1024 * 1024}, {recbuf, 1024 * 1024}]),
        Resp = Req:respond({200, ?HDR, chunked}),

        filter_and_send_chunked_logs(Resp, Logs, Filters, Num),

        Resp:write_chunk(<<>>),

        {done, {200, "<chunked>"}}
    end},

    %% V1
    {['GET', "^/channels/(\\d+)/info$"], fun(Req, [ChannelId], _) ->
        authorize(Req),
        Info = channel_info(api_v1, ChannelId),
        not is_list(Info) andalso exit({expected_list, Info}),

        {200, iolist_to_binary(mochijson2:encode({struct, Info}))}
    end},

    {['POST', "^/channels/(\\d+)/drains/tokens$"], fun(_Req, _, read_only) -> {503, ?API_READONLY};
                                                      (Req, [ChannelId], _) ->
        authorize(Req),

        %% Drain reservation occurs in order to ensure DrainId is propgated
        %% back to ETS.
        {ok, DrainId, Token} = logplex_drain:reserve_token(),
        logplex_drain:cache(DrainId, Token, list_to_integer(ChannelId)),
        Resp = [{id, DrainId},
                {token, Token},
                {msg, <<"Successfully reserved drain token">>}],
        {201, iolist_to_binary(mochijson2:encode({struct, Resp}))}
    end},

    {['POST', "^/channels/(\\d+)/drains/(\\d+)$"], fun(Req, [_ChannelIdStr, _DrainIdStr], _) ->
        authorize(Req),
        {501, <<"V1 Drain Creation API deprecated">>}
    end},

    {['POST', "^/v2/channels/(\\d+)/drains/(\\d+)$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                                         (Req, [ChannelIdStr, DrainIdStr], _) ->
        authorize(Req),

        DrainId = list_to_integer(DrainIdStr),
        ChannelId = list_to_integer(ChannelIdStr),
        RequestId = header_value(Req, "Request-Id", ""),
        case logplex_drain:poll_token(DrainId) of
            {error, timeout} ->
                ?INFO("drain_id=~p channel_id=~p request_id=~p at=poll_token result=timeout",
                      [DrainId, ChannelId, RequestId]),
                json_error(404, <<"Unknown drain.">>);
            Token when is_binary(Token) ->
                case valid_uri(Req) of
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
                                                {url, uri_to_binary(URI)}
                                               ],
                                        {201,?JSON_CONTENT,
                                         mochijson2:encode({struct, Resp})}
                                end
                        end
                end
        end
    end},

    {['POST', "^/channels/(\\d+)/drains$"], fun(Req, [_ChannelId], _) ->
        authorize(Req),

        {501, <<"V1 Drain Creation API deprecated.">>}
    end},

    %% V2
    {['POST', "^/v2/channels/(\\d+)/drains$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                                  (Req, [ChannelIdStr], _) ->
        ChannelId = list_to_integer(ChannelIdStr),
        case valid_uri(Req) of
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
                                        {url, uri_to_binary(URI)}
                                       ],
                                {201,?JSON_CONTENT,
                                 mochijson2:encode({struct, Resp})}
                        end
                end
        end
    end},

    {['GET', "^/channels/(\\d+)/drains$"], fun(Req, [_ChannelId], _) ->
        authorize(Req),
        {501, <<"V1 Drain API Deprecated.">>}
    end},

    {['DELETE', "^/channels/(\\d+)/drains$"], fun(Req, [_ChannelId], _) ->
        authorize(Req),
       {501, <<"V1 Drain API Deprecated.">>}
    end},

    %% V2
    {['DELETE', "^/v2/channels/(\\d+)/drains/(\\d+)$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                                           (Req, [ChannelId, DrainId], _) ->
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

serve(_Handlers, _Method, _Path, _Req, disabled) ->
    {503, ?API_DISABLED};

serve([], _Method, _Path, _Req, _Status) ->
    {404, <<"Not found">>};

serve([{[HMethod, Regexp], Fun}|Tail], Method, Path, Req, Status) ->
    case re:run(Path, Regexp, [{capture, all_but_first, list}]) of
        {match, Captured} when HMethod == Method ->
            case catch Fun(Req, Captured, Status) of
                {'EXIT', {Code, Body}} when is_integer(Code), is_binary(Body) ->
                    {Code, Body};
                {'EXIT', {Code, Body}} when is_integer(Code), is_list(Body) ->
                    {Code, Body};
                {'EXIT', Err} ->
                    exit(Err);
                {done, Details} ->
                    {done, Details};
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
            serve(Tail, Method, Path, Req, Status)
    end.

authorize(Req) ->
    try
        AuthKey = logplex_app:config(auth_key),
        "Basic " ++ Encoded = Req:get_header_value("Authorization"),
        case Encoded of
            AuthKey -> true;
            _ ->
                {authorized, Cred} = logplex_cred:verify_basic(Encoded),
                permitted = logplex_cred:has_perm(full_api, Cred),
                true
        end
    catch
        error:{badmatch, {incorrect_pass, CredId}} ->
            ?INFO("at=authorize cred_id=~p error=incorrect_pass",
                  [CredId]),
            error_resp(401, <<"Not Authorized">>);
        Class:Ex ->
            Stack = erlang:get_stacktrace(),
            ?WARN("at=authorize exception=~1000p",
                  [{Class, Ex, Stack}]),
            error_resp(401, <<"Not Authorized">>)
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

filter_and_send_chunked_logs(Resp, Logs, [], _Num) ->
    [Resp:write_chunk(logplex_utils:format(logplex_utils:parse_msg(Msg))) || Msg <- lists:reverse(Logs)];

filter_and_send_chunked_logs(Resp, Logs, Filters, Num) ->
    filter_and_send_chunked_logs(Resp, Logs, Filters, Num, []).

filter_and_send_chunked_logs(Resp, Logs, _Filters, Num, Acc) when Logs == []; Num == 0 ->
    Resp:write_chunk(Acc);

filter_and_send_chunked_logs(Resp, [Msg|Tail], Filters, Num, Acc) ->
    Msg1 = logplex_utils:parse_msg(Msg),
    case logplex_utils:filter(Msg1, Filters) of
        true ->
            filter_and_send_chunked_logs(Resp, Tail, Filters, Num-1, [logplex_utils:format(Msg1)|Acc]);
        false ->
            filter_and_send_chunked_logs(Resp, Tail, Filters, Num, Acc)
    end.


no_tail_warning() ->
    logplex_utils:format(undefined,
                         logplex_utils:formatted_utc_date(),
                         <<"Logplex">>,
                         logplex_app:config(no_tail_warning)).

tail_init(Socket, Buffer, Filters, ChannelId) ->
    inet:setopts(Socket, [{active, once}]),
    tail_loop(Socket, Buffer, Filters, ChannelId, 0).

tail_loop(Socket, Buffer, Filters, ChannelId, BytesSent) ->
    logplex_tail_buffer:set_active(Buffer,
                                   tail_filter(Filters)),
    receive
        {logplex_tail_data, Buffer, Data} ->
            gen_tcp:send(Socket,
                         Data),
            tail_loop(Socket, Buffer, Filters, ChannelId, iolist_size(Data) + BytesSent);
        {tcp_data, Socket, _} ->
            inet:setopts(Socket, [{active, once}]),
            tail_loop(Socket, Buffer, Filters, ChannelId, BytesSent);
        {tcp_closed, Socket} ->
            ?INFO("at=tail_loop event=tail_close reason=tcp_closed channel_id=~p bytes_sent=~p", [ChannelId, BytesSent]),
            ok;
        {tcp_error, Socket, Reason} ->
            ?INFO("at=tail_loop event=tail_close reason=tcp_closed channel_id=~p reason=~p bytes_sent=~p", [ChannelId, Reason, BytesSent]),
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
    Size = byte_size(Ps),
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
    uri_to_binary(logplex_drain:uri(Drain));
drain_info(api_v2, Drain) ->
    [{id, logplex_drain:id(Drain)},
     {token, logplex_drain:token(Drain)},
     {url, uri_to_binary(logplex_drain:uri(Drain))}].

not_found_json() ->
    Json = {struct, [{error, <<"Not found">>}]},
    {404, iolist_to_binary(mochijson2:encode(Json))}.

json_error(Code, Err) ->
    {Code, ?JSON_CONTENT,
     mochijson2:encode({struct, [{error, iolist_to_binary(Err)}]})}.

api_relative_url(canary, UUID) when is_binary(UUID) ->
    iolist_to_binary([<<"/v2/canary-fetch/">>, UUID]);
api_relative_url(_APIVSN, UUID) when is_binary(UUID) ->
    iolist_to_binary([<<"/sessions/">>, UUID]).

end_chunked_response(Socket) ->
    gen_tcp:close(Socket),
    ok.

uri_to_binary(Uri) ->
    iolist_to_binary(ex_uri:encode(Uri)).

valid_uri(Req) ->
    {struct, Data} = mochijson2:decode(Req:recv_body()),
    Uri = case proplists:get_value(<<"url">>, Data) of
              undefined ->
                  Port = proplists:get_value(<<"port">>, Data),
                  case proplists:get_value(<<"host">>, Data) of
                      undefined ->
                          {error, missing_host_param};
                      Host ->
                          logplex_tcpsyslog_drain:uri(Host, Port)
            end;
              UrlString ->
                  try
                      logplex_drain:parse_url(UrlString)
                  catch
                      error:badarg ->
                          {error, invalid_url}
                  end
          end,
    logplex_drain:valid_uri(Uri).

%% Checks whether the API state
-spec status() -> 'normal' | 'read_only' | 'disabled'.
status() ->
    logplex_app:config(api_status, normal).

set_status(Term) ->
    Old = status(),
    case Term of
        normal -> io:format("Fully Enabling API~n");
        read_only -> io:format("API in read-only mode: only GET requests and "
                               "canary operations allowed.~n");
        disabled -> io:format("API entirely disabled, **INCLUDING HEALTHCHECKS**~n")
    end,
    logplex_app:set_config(api_status, Term),
    Old.

