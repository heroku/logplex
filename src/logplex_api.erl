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
    Host = Req:get_header_value('Host'),
    Peer = Req:get(peer),
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
                Req:respond({status_io(Code), Hdr, Body}),
                ?INFO("at=request channel_id=~s host=~p peer=~p method=~p path=~s"
                    " resp_code=~w time=~w body=~s",
                    [ChannelId, Host, Peer, Method, Path, Code, Time, Body]);
            {done,{Code,Details}} ->
                ?INFO("at=request channel_id=~s host=~p peer=~p method=~p path=~s "
                      "resp_code=~w time=~w body=~s",
                      [ChannelId, Host, Peer, Method, Path, Code, Time, Details])
        end
    catch
        exit:normal ->
            exit(normal);
        Class:Exception ->
            Time1 = timer:now_diff(os:timestamp(), Start) div 1000,
            Req:respond({500, ?HDR, ""}),
            ?ERR("channel_id=~s method=~p path=~s "
                 "time=~w exception=~1000p:~1000p stack=~1000p",
                 [ChannelId, Method, Path, Time1, Class, Exception,
                  erlang:get_stacktrace()])
    end.

handlers() ->
    [{['GET', "/healthcheck"], fun(Req, _Match, _Status) ->
        authorize(Req),

        RegisteredMods = [logplex_stats, logplex_tail, logplex_shard, tcp_acceptor],
        [(whereis(Name) == undefined orelse not is_process_alive(whereis(Name))) andalso throw({500, io_lib:format("Process dead: ~p", [Name])}) || Name <- RegisteredMods],

        Count = logplex_stats:healthcheck(),
        not is_integer(Count) andalso throw({500, io_lib:format("Increment healthcheck counter failed: ~p", [Count])}),

        Info =[{status, status()}],

        {200, ?JSON_CONTENT, iolist_to_binary(mochijson2:encode(Info))}
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

        Name = proplists:get_value(<<"name">>, Params, <<"">>),
        is_binary(Name) orelse error_resp(400, <<"Channel name must be a string.">>),
        Channel = logplex_channel:create(Name),
        ChannelId = logplex_channel:id(Channel),
        Tokens =
            case proplists:get_value(<<"tokens">>, Params) of
                List when length(List) > 0 ->
                    [{TokenName, logplex_token:create(ChannelId, TokenName)}
                     || TokenName <- List];
                _ ->
                    []
            end,
        Info = [{channel_id, ChannelId}, {tokens, Tokens}],
        {201, ?JSON_CONTENT, iolist_to_binary(mochijson2:encode({struct, Info}))}
    end},

    %% V2
    {['GET', "^/v2/channels/(\\d+)$"], fun(Req, [ChannelId], _Status) ->
        authorize(Req),
        case channel_info(api_v2, ChannelId) of
            not_found -> json_error(404, "Not Found");
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
                                <<"/sessions/">>, UUID])}
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
        Header0 = case logplex_channel:lookup_flag(no_redis, ChannelId) of
                     no_redis -> ?HDR ++ [{"X-Heroku-Warning",
                                           logplex_app:config(no_redis_warning)}];
                     _ -> ?HDR
                 end,
        Header = Header0 ++ [{"connection", "close"}],
        Resp = Req:respond({200, Header, chunked}),

        inet:setopts(Socket, [{nodelay, true}, {packet_size, 1024 * 1024}, {recbuf, 1024 * 1024}]),

        filter_and_send_chunked_logs(Resp, Logs, Filters, Num),

        case {proplists:get_value(<<"tail">>, Data, tail_not_requested),
              logplex_channel:lookup_flag(no_tail, ChannelId)} of
            {tail_not_requested, _} ->
                write_chunk(Resp, close);
            {_, no_tail} ->
                write_chunk(Resp, no_tail_warning()),
                write_chunk(Resp, close);
            _ ->
                ?INFO("at=tail_start channel_id=~p filters=~100p",
                      [ChannelId, Filters]),
                logplex_stats:incr(session_tailed),
                {ok, Buffer} =
                    logplex_tail_buffer:start_link(ChannelId, self()),
                try
                    tail_init(Socket, Resp, Buffer, Filters, ChannelId)
                after
                    ?INFO("at=tail_end channel_id=~p",
                          [ChannelId]),
                    exit(Buffer, shutdown)
                end
        end,

        {200, ""}
    end},

    {['GET', "^/v2/canary-fetch/([\\w-]+)$"], fun(Req, [Session], _) ->
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

        write_chunk(Resp, close),

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

    {['POST', "^/channels/(\\d+)/drains$"], fun(Req, [_ChannelId], _) ->
        authorize(Req),

        {501, <<"V1 Drain Creation API deprecated.">>}
    end},

    %% V2
    {['POST', "^/v2/channels/(\\d+)/drains$"], fun(_Req, _Match, read_only) -> {503, ?API_READONLY};
                                                  (Req, [ChannelIdStr], _) ->
        authorize(Req),
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

write_chunk(Resp, close) ->
    Resp:write_chunk(<<>>);
write_chunk(Resp, Data) ->
    % FIXME not sure why we would get an empty data packet
    % but we do not want to write it, since this would tell
    % HTTP client that the chunked response is finished
    case iolist_size(Data) of
        0 -> skip;
        _ ->
            Resp:write_chunk(Data)
    end.

filter_and_send_chunked_logs(Resp, Logs, [], _Num) ->
    [write_chunk(Resp, logplex_utils:format(logplex_utils:parse_msg(Msg))) || Msg <- lists:reverse(Logs)];

filter_and_send_chunked_logs(Resp, Logs, Filters, Num) ->
    filter_and_send_chunked_logs(Resp, Logs, Filters, Num, []).

filter_and_send_chunked_logs(Resp, Logs, _Filters, Num, Acc) when Logs == []; Num == 0 ->
    write_chunk(Resp, Acc);

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

tail_init(Socket, Resp, Buffer, Filters, ChannelId) ->
    inet:setopts(Socket, [{active, once}]),
    tail_loop(Socket, Resp, Buffer, Filters, ChannelId, 0).

tail_loop(Socket, Resp, Buffer, Filters, ChannelId, BytesSent) ->
    logplex_tail_buffer:set_active(Buffer,
                                   tail_filter(Filters)),
    receive
        {logplex_tail_data, Buffer, Data} ->
            write_chunk(Resp, Data),
            tail_loop(Socket, Resp, Buffer, Filters, ChannelId, iolist_size(Data) + BytesSent);
        {tcp_data, Socket, _} ->
            inet:setopts(Socket, [{active, once}]),
            tail_loop(Socket, Resp, Buffer, Filters, ChannelId, BytesSent);
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

json_error(Code, Err) ->
    Body = mochijson2:encode({struct, [{error, iolist_to_binary(Err)}]}),
    {Code, ?JSON_CONTENT, iolist_to_binary(Body)}.

api_relative_url(canary, UUID) when is_binary(UUID) ->
    iolist_to_binary([<<"/v2/canary-fetch/">>, UUID]);
api_relative_url(_APIVSN, UUID) when is_binary(UUID) ->
    iolist_to_binary([logplex_app:config(api_endpoint_url, ""), <<"/sessions/">>, UUID]).

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

-spec status_io(pos_integer()) -> binary().
status_io(100) -> <<"100 Continue">>;
status_io(101) -> <<"101 Switching Protocols">>;
status_io(102) -> <<"102 Processing">>;
status_io(200) -> <<"200 OK">>;
status_io(201) -> <<"201 Created">>;
status_io(202) -> <<"202 Accepted">>;
status_io(203) -> <<"203 Non-Authoritative Information">>;
status_io(204) -> <<"204 No Content">>;
status_io(205) -> <<"205 Reset Content">>;
status_io(206) -> <<"206 Partial Content">>;
status_io(207) -> <<"207 Multi-Status">>;
status_io(226) -> <<"226 IM Used">>;
status_io(300) -> <<"300 Multiple Choices">>;
status_io(301) -> <<"301 Moved Permanently">>;
status_io(302) -> <<"302 Found">>;
status_io(303) -> <<"303 See Other">>;
status_io(304) -> <<"304 Not Modified">>;
status_io(305) -> <<"305 Use Proxy">>;
status_io(306) -> <<"306 Switch Proxy">>;
status_io(307) -> <<"307 Temporary Redirect">>;
status_io(400) -> <<"400 Bad Request">>;
status_io(401) -> <<"401 Unauthorized">>;
status_io(402) -> <<"402 Payment Required">>;
status_io(403) -> <<"403 Forbidden">>;
status_io(404) -> <<"404 Not Found">>;
status_io(405) -> <<"405 Method Not Allowed">>;
status_io(406) -> <<"406 Not Acceptable">>;
status_io(407) -> <<"407 Proxy Authentication Required">>;
status_io(408) -> <<"408 Request Timeout">>;
status_io(409) -> <<"409 Conflict">>;
status_io(410) -> <<"410 Gone">>;
status_io(411) -> <<"411 Length Required">>;
status_io(412) -> <<"412 Precondition Failed">>;
status_io(413) -> <<"413 Request Entity Too Large">>;
status_io(414) -> <<"414 Request-URI Too Long">>;
status_io(415) -> <<"415 Unsupported Media Type">>;
status_io(416) -> <<"416 Requested Range Not Satisfiable">>;
status_io(417) -> <<"417 Expectation Failed">>;
status_io(418) -> <<"418 I'm a teapot">>;
status_io(422) -> <<"422 Unprocessable Entity">>;
status_io(423) -> <<"423 Locked">>;
status_io(424) -> <<"424 Failed Dependency">>;
status_io(425) -> <<"425 Unordered Collection">>;
status_io(426) -> <<"426 Upgrade Required">>;
status_io(428) -> <<"428 Precondition Required">>;
status_io(429) -> <<"429 Too Many Requests">>;
status_io(431) -> <<"431 Request Header Fields Too Large">>;
status_io(500) -> <<"500 Internal Server Error">>;
status_io(501) -> <<"501 Not Implemented">>;
status_io(502) -> <<"502 Bad Gateway">>;
status_io(503) -> <<"503 Service Unavailable">>;
status_io(504) -> <<"504 Gateway Timeout">>;
status_io(505) -> <<"505 HTTP Version Not Supported">>;
status_io(506) -> <<"506 Variant Also Negotiates">>;
status_io(507) -> <<"507 Insufficient Storage">>;
status_io(510) -> <<"510 Not Extended">>;
status_io(511) -> <<"511 Network Authentication Required">>;
status_io(B) when is_binary(B) -> B.
