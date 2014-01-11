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
    ].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    [{api, "http://localhost:"++integer_to_list(logplex_app:config(http_port))}
     ,{auth, "Basic " ++ logplex_app:config(auth_key)} | Config].

end_per_suite(_Config) ->
    application:stop(logplex).

init_per_group(read_only, Config) ->
    InitialStatus = application:get_env(logplex, api_status),
    logplex_api:set_status(read_only),
    read_only = logplex_api:status(),
    [{initial_api_status, InitialStatus} | Config];
init_per_group(disabled, Config) ->
    InitialStatus = application:get_env(logplex, api_status),
    logplex_api:set_status(disabled),
    disabled = logplex_api:status(),
    [{initial_api_status, InitialStatus} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(read_only, Config) ->
    case ?config(initial_api_status, Config) of
        undefined -> application:unset_env(logplex, api_status);
        {ok,Val} -> application:set_env(logplex, api_status, Val)
    end;
end_per_group(disabled, Config) ->
    case ?config(initial_api_status, Config) of
        undefined -> application:unset_env(logplex, api_status);
        {ok,Val} -> application:set_env(logplex, api_status, Val)
    end;
end_per_group(_, _Config) ->
    ok.

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
                                     (list_to_binary(integer_to_list(ChannelId)))/binary,
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

v2_canary_session(Config) ->
    Api = ?config(api, Config) ++ "/v2/canary-sessions",
    BasicAuth = ?config(auth, Config),
    ChannelId = ?config(channel_id, Config),
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "{\"channel_id\":\"" ++ integer_to_list(ChannelId) ++ "\"}"}]),
    201 = proplists:get_value(status_code, Res),
    {struct, [{<<"url">>, <<"/v2/canary-fetch/", Session:36/binary>>}]} = 
        mochijson2:decode(proplists:get_value(body, Res)),
    {save_config, [{canary_session, Session},
                   {channel_id, ChannelId}]}.

v2_canary_fetch(Config) ->
    {v2_canary_session, SavedConfig} = ?config(saved_config, Config),
    Session = proplists:get_value(canary_session, SavedConfig),
    Api = ?config(api, Config) ++ "/v2/canary-fetch/"
        ++ binary_to_list(Session) ++ "?srv=ct",
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
                     {body, "{\"channel_id\":\"" ++ integer_to_list(ChannelId) ++ "\"}"}]),
    503 = proplists:get_value(status_code, Res).

unavailable_v2_canary_fetch(Config) ->
    Session = proplists:get_value(canary_session, Config),
    Api = ?config(api, Config) ++ "/v2/canary-fetch/"
        ++ binary_to_list(Session) ++ "?srv=ct",
    Res = get_(Api, []),
    503 = proplists:get_value(status_code, Res).

%% Other helpers
get_(Url, Opts) ->
    request(get, Url, Opts).

post(Url, Opts) ->
    request(post, Url, Opts).

request(Method, Url, Opts) ->
    Headers = proplists:get_value(headers, Opts, []),
    Timeout = proplists:get_value(timeout, Opts, 1000),
    Request = 
        case Method of
            post ->
                ContentType = proplists:get_value(content_type, Opts, "application/json"),
                BodyToSend = proplists:get_value(body, Opts, []),
                {Url, Headers, ContentType, BodyToSend};
            _ ->
                {Url, Headers}
        end,
    HttpcOpts = proplists:get_value(opts, Opts, []),
    case httpc:request(Method, Request, [{timeout, Timeout}], HttpcOpts) of
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
             {body, Body}]
    end.

wait_for_http(RequestId, Headers, Body) ->
    receive
        {http, {RequestId, stream_start, Headers}} ->
            wait_for_http(RequestId, Headers, Body);
        {http, {RequestId, stream, BinBody}} ->
            Body0 = binary_to_list(BinBody),
            wait_for_http(RequestId, Headers, Body++Body0);
        {http, {RequestId, stream_end, Headers0}} ->
            {Headers++Headers0, Body}
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
         {"LOGPLEX_AUTH_KEY", uuid:uuid_to_string(uuid:get_v4())},
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
