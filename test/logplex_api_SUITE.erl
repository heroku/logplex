-module(logplex_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

groups() ->
    [{v2_canary, [sequence],
      [v2_canary_session
       ,v2_canary_fetch
      ]}
    ].

all() ->
    [{group, v2_canary}
    ].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    [{api, "http://localhost:"++integer_to_list(logplex_app:config(http_port))}
     ,{auth, "Basic " ++ logplex_app:config(auth_key)} | Config].

end_per_suite(_Config) ->
    application:stop(logplex).

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
    [ok = logplex_SUITE:send_msg(logplex_SUITE:make_msg(Token, N)) ||
        N <- lists:seq(1, 10)],
    timer:sleep(1000),
    Config;
init_per_testcase(_Case, Config) ->
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
    [] = proplists:get_value(body, Res),
    "text/html" = proplists:get_value("content-type", Headers),
    Config.

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
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_NODE_NAME", atom_to_list(node())}
        ]].
