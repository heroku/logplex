-module(logplex_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

groups() ->
    [{v2_canary, [sequence],
      [v2_canary_session
       ,v2_canary_fetch
      ]}
    ,{read_only, [],
      [{group, v2_canary} % canaries keep working when API read-only
      ]}
    ,{disabled, [], % nothing works with API disabled
      [unavailable_v2_canary_session
      ,unavailable_v2_canary_fetch
      ]}
    ].

all() ->
    [{group, v2_canary}
    ,{group, read_only}
    ,{group, disabled}
    ,channel_create_with_app_name
    ,channel_int_compat
    ].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    [{api, "http://localhost:"++integer_to_list(logplex_app:config(http_port))}
     ,{auth, "Basic " ++ logplex_app:config(auth_key)} | Config].

end_per_suite(_Config) ->
    application:stop(logplex).

init_per_group(read_only, Config) ->
    InitialStatus = application:get_env(logplex, legacy_api_status),
    logplex_api:set_status(read_only),
    read_only = logplex_api:status(),
    [{initial_api_status, InitialStatus} | Config];
init_per_group(disabled, Config) ->
    InitialStatus = application:get_env(logplex, legacy_api_status),
    logplex_api:set_status(disabled),
    disabled = logplex_api:status(),
    [{initial_api_status, InitialStatus} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(read_only, Config) ->
    case ?config(initial_api_status, Config) of
        undefined -> application:unset_env(logplex, legacy_api_status);
        {ok,Val} -> application:set_env(logplex, legacy_api_status, Val)
    end;
end_per_group(disabled, Config) ->
    case ?config(initial_api_status, Config) of
        undefined -> application:unset_env(logplex, legacy_api_status);
        {ok,Val} -> application:set_env(logplex, legacy_api_status, Val)
    end;
end_per_group(_, _Config) ->
    ok.

init_per_testcase(channel_create_with_app_name=_Case, Config) ->
    ChannelName = <<"test-app">>,
    Token = <<"test">>,
    [{channel_name, ChannelName}, {channel_token, Token} | Config];
init_per_testcase(channel_int_compat, Config) ->
    ChannelName = <<"test-app">>,
    Token = <<"test">>,
    [{channel_name, ChannelName}, {channel_token, Token} | Config];
init_per_testcase(v2_canary_session=Case, Config) ->
    Channel = logplex_channel:create(atom_to_binary(Case, latin1)),
    ChannelId = logplex_channel:id(Channel),
    logplex_SUITE:wait_for_chan(Channel),
    logplex_channel:register({channel, ChannelId}),
    logplex_SUITE:wait_for_registration({channel, ChannelId}),
    [{channel_id, ChannelId}|Config];
init_per_testcase(v2_canary_fetch, Config) ->
    % Create a token and hook up to the channel
    {v2_canary_session, SavedConfig} = ?config(saved_config, Config),
    ChannelId = ?config(channel_id, SavedConfig),
    TokenId = logplex_token:create(ChannelId, <<"ct">>),
    Token = logplex_SUITE:get_token(TokenId),
    [ok = logplex_SUITE:send_msg(make_msg(Token, N)) ||
        N <- lists:seq(1, 10)],
    timer:sleep(1000),
    Config;
init_per_testcase(unavailable_v2_canary_session=Case, Config) ->
    Channel = logplex_channel:create(atom_to_binary(Case, latin1)),
    ChannelId = logplex_channel:id(Channel),
    logplex_SUITE:wait_for_chan(Channel),
    logplex_channel:register({channel, ChannelId}),
    logplex_SUITE:wait_for_registration({channel, ChannelId}),
    [{channel_id, ChannelId}|Config];
init_per_testcase(unavailable_v2_canary_fetch=Case, Config) ->
    % Create a token and hook up to the channel
    Channel = logplex_channel:create(atom_to_binary(Case, latin1)),
    ChannelId = logplex_channel:id(Channel),
    logplex_SUITE:wait_for_chan(Channel),
    logplex_channel:register({channel, ChannelId}),
    logplex_SUITE:wait_for_registration({channel, ChannelId}),
    %% Create a working session to simulate a failure after having gotten auth
    UUID = logplex_session:publish(<<"{\"channel_id\":\"",
                                     ChannelId/binary,
                                     "\"}">>),
    TokenId = logplex_token:create(ChannelId, <<"ct">>),
    Token = logplex_SUITE:get_token(TokenId),
    [ok = logplex_SUITE:send_msg(make_msg(Token, N)) ||
        N <- lists:seq(1, 10)],
    [{canary_session, UUID} | Config];
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

channel_create_with_app_name(Config) ->
    Api = ?config(api, Config) ++ "/channels",
    BasicAuth = ?config(auth, Config),
    ChannelName = ?config(channel_name, Config),
    TokenName = ?config(channel_token, Config),

    Channel = logplex_channel:create(ChannelName),
    ChannelId = logplex_channel:id(Channel),
    Token = logplex_token:create(ChannelId, TokenName),

    meck:new([logplex_channel, logplex_token], [passthrough]),
    meck:expect(logplex_channel, create, [{[ChannelName], Channel}]),
    meck:expect(logplex_token, create, [{[ChannelId, TokenName], Token}]),

    JsonBody = ["{\"name\":\"", ChannelName, "\","
                "\"tokens\": [\"", TokenName, "\"]}"],

    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, iolist_to_binary(JsonBody)}]),
    201 = proplists:get_value(status_code, Res),
    {struct, Props} = mochijson2:decode(proplists:get_value(body, Res)),

    ChannelId = list_to_binary(integer_to_list(proplists:get_value(<<"channel_id">>, Props))),
    {struct, [{TokenName, Token}]} = proplists:get_value(<<"tokens">>, Props),

    meck:unload([logplex_channel, logplex_token]),
    ok.

channel_int_compat(Config) ->
    %% Queries for channels are only valid if the id is a string representable
    %% as an integer.
    ApiV1 = ?config(api, Config) ++ "/channels/",
    ApiV2 = ?config(api, Config) ++ "/v2/channels/",
    BasicAuth = ?config(auth, Config),
    ChannelName = ?config(channel_name, Config),
    TokenName = ?config(channel_token, Config),

    Channel = logplex_channel:create(ChannelName),
    ChannelId = logplex_channel:id(Channel),
    Token = logplex_token:create(ChannelId, TokenName),
    Opts = [{headers, [{"Authorization", BasicAuth}]}],

    meck:new([logplex_channel, logplex_token], [passthrough]),
    meck:expect(logplex_channel, create, [{[ChannelName], Channel}]),
    meck:expect(logplex_token, create, [{[ChannelId, TokenName], Token}]),

    200 = proplists:get_value(status_code, get_(ApiV1 ++ binary_to_list(ChannelId) ++ "/info", Opts)),
    200 = proplists:get_value(status_code, get_(ApiV2 ++ binary_to_list(ChannelId), Opts)),
    %% Result is not found since the regexes used for api v2 only match integers
    404 = proplists:get_value(status_code, get_(ApiV1 ++ "123badchannel/info", Opts)),
    404 = proplists:get_value(status_code, get_(ApiV2 ++ "123badchannel", Opts)),

    meck:unload([logplex_channel, logplex_token]),
    ok.

v2_canary_session(Config) ->
    Api = ?config(api, Config) ++ "/v2/canary-sessions",
    BasicAuth = ?config(auth, Config),
    ChannelId = ?config(channel_id, Config),
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "{\"channel_id\":\"" ++ binary_to_list(ChannelId) ++ "\"}"}]),
    201 = proplists:get_value(status_code, Res),
    {struct, [{<<"url">>, <<"/v2/canary-fetch/", Session:36/binary>>}]} =
        mochijson2:decode(proplists:get_value(body, Res)),
    {save_config, [{canary_session, Session},
                   {channel_id, ChannelId}]}.

v2_canary_fetch(Config) ->
    {v2_canary_session, SavedConfig} = ?config(saved_config, Config),
    Session = proplists:get_value(canary_session, SavedConfig),
    Api = ?config(api, Config) ++ "/v2/canary-fetch/"
        ++ binary_to_list(Session),
    Res = get_(Api, []),
    Headers = proplists:get_value(headers, Res),
    "text/html" = proplists:get_value("content-type", Headers),
    %% httpc is a shifty bastard and transforms a chunked response back
    %% into a regular one for you, and replaces the header for a regular
    %% one with the right content-length. I'm somewhat full of hatred for
    %% the time lost right now
    %% "chunked" = proplists:get_value("transfer-encoding", Headers),
    %
    %% messages can be split by linebreak when being streamed. We should
    %% have 10, each with the right numbers, in order, at the last position
    Body = proplists:get_value(body, Res),
    Logs = string:tokens(Body, "\n"),
    ct:pal("zips: ~p",[lists:zip(lists:seq(1,10),Logs)]),
    10 = length(Logs),
    [1,2,3,4,5,6,7,8,9,10] = lists:sort(
    [begin
      {match, [N]} = re:run(Log, "[0-9]{1,2}$", [{capture,first,list}]),
      list_to_integer(N)
     end || Log <- Logs]),
    Config.

unavailable_v2_canary_session(Config) ->
    Api = ?config(api, Config) ++ "/v2/canary-sessions",
    BasicAuth = ?config(auth, Config),
    ChannelId = ?config(channel_id, Config),
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "{\"channel_id\":\"" ++ binary_to_list(ChannelId) ++ "\"}"}]),
    503 = proplists:get_value(status_code, Res).

unavailable_v2_canary_fetch(Config) ->
    Session = proplists:get_value(canary_session, Config),
    Api = ?config(api, Config) ++ "/v2/canary-fetch/"
        ++ binary_to_list(Session),
    Res = get_(Api, []),
    503 = proplists:get_value(status_code, Res).

%% Other helpers
get_(Url, Opts) ->
    request(get, Url, Opts).

post(Url, Opts) ->
    ct:pal("POST url=~p, opts=~p", [Url, Opts]),
    request(post, Url, Opts).

put_(Url, Opts) ->
    ct:pal("PUT url=~p, opts=~p", [Url, Opts]),
    request(put, Url, Opts).

request(Method, Url, Opts) ->
    Headers = proplists:get_value(headers, Opts, []),
    Timeout = proplists:get_value(timeout, Opts, 1000),
    Request =
        case Method of
            PostOrPut when PostOrPut == post; PostOrPut == put ->
                ContentType = proplists:get_value(content_type, Opts, "application/json"),
                BodyToSend = proplists:get_value(body, Opts, []),
                {Url, Headers, ContentType, BodyToSend};
            _ ->
                {Url, Headers}
        end,
    HttpOpts = proplists:get_value(http_opts, Opts, []),
    HttpcOpts = proplists:get_value(opts, Opts, []),
    case httpc:request(Method, Request, [{timeout, Timeout}| HttpOpts], HttpcOpts) of
        {ok, {{HttpVersion, StatusCode, HttpReason}, Headers0, Body}} ->
            [{status_code, StatusCode},
             {http_version, HttpVersion},
             {http_reason, HttpReason},
             {headers, Headers0},
             {body, Body}];
        {ok, {StatusCode, Body}} ->
            [{status_code, StatusCode},
             {body, Body}];
        {ok, ReqId} when is_reference(ReqId) ->
            {Headers0, Body} = wait_for_http(ReqId, [], []),
            [{headers, Headers0},
             {body, Body}];
        Other -> ct:pal("~p", [Other]),
                 exit(bad_case)
    end.

wait_for_http(RequestId, Headers, Body) ->
    receive
        {http, {RequestId0, stream_start, Headers0}} ->
            wait_for_http(RequestId0, Headers0, Body);
        {http, {RequestId, stream, BinBody}} ->
            Body0 = binary_to_list(BinBody),
            wait_for_http(RequestId, Headers, Body++Body0);
        {http, {RequestId, stream_end, Headers0}} ->
            {Headers++Headers0, Body};
        {http, Msg} ->
            ct:fail("unexpected http message: ~p~n", [Msg]),
            {error, unexpected_http_msg, Msg}
    end.

wait_for_messages(Channel, MessageAmount, MaxWait, StartTime) ->
    case  timer:now_diff(os:timestamp(), StartTime) / 1000 > MaxWait of
        true ->
            {error, timeout};
        _ ->
            Msg = logplex_SUITE:read_logs(Channel),
            if
                length(Msg) >= MessageAmount ->
                    Msg;
                true ->
                    timer:sleep(10),
                    wait_for_messages(Channel, MessageAmount, MaxWait, StartTime)
            end
    end.


%% Startup/Teardown helpers
set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", net_adm:localhost()},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_NODE_NAME", atom_to_list(node())}
        ]].

make_msg(Token, N) ->
    logplex_syslog_utils:rfc5424(
        user,
        debug,
        logplex_syslog_utils:datetime(now),
        "fakehost",
        logplex_token:id(Token),
        "erlang",
        "web.1 -", % the - is a trick because we suck at rfc5424 generation
        integer_to_list(N)
    ).
