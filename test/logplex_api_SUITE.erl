-module(logplex_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

groups() ->
    [{v2_canary, [sequence],
      [v2_canary_session
       ,v2_canary_fetch
      ]},
     {v1, [sequence],
      [get_channels
       ,create_token
       ,del_channel]},
     {v2, [sequence],
      [v2_get_channels
       ,v2_create_token
       ,v2_del_channels]}
    ].

all() ->
    [
     healthcheck
     ,load
     ,{group, v1}
     ,{group, v2}
     ,{group, v2_canary}
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
init_per_testcase(v2_get_channels=Case, Config) ->
    Channel = logplex_channel:create(atom_to_binary(Case, latin1)),
    ChannelId = logplex_channel:id(Channel),
    [{channel, ChannelId}|Config];
init_per_testcase(_Case, Config) ->
    Config.

healthcheck(Config) ->
    Api = ?config(api, Config) ++ "/healthcheck",
    BasicAuth = ?config(auth, Config),
    Res = get_(Api, [{headers, [{"Authorization", BasicAuth}]}]),
    200 = proplists:get_value(status_code, Res),
    "OK" = proplists:get_value(body, Res),
    Config.

load(Config) ->
    Api = ?config(api, Config) ++ "/load",
    BasicAuth = ?config(auth, Config),
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "[\"logplex\"]"}]),
    200 = proplists:get_value(status_code, Res),
    "[]" = proplists:get_value(body, Res),
    Config.

get_channels(Config) ->
    Api = ?config(api, Config) ++ "/channels",
    BasicAuth = ?config(auth, Config),
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "{\"tokens\":[\"token1\",\"token2\"]}"}]),
    201 = proplists:get_value(status_code, Res),
    {struct, Json} = mochijson2:decode(proplists:get_value(body, Res)),
    Channel = proplists:get_value(<<"channel_id">>, Json),
    {struct, Tokens} = proplists:get_value(<<"tokens">>, Json),
    Token1 = proplists:get_value(<<"token1">>, Tokens),
    Token2 = proplists:get_value(<<"token2">>, Tokens),
    true = is_tuple(get_token(Token1)),
    true = is_tuple(get_token(Token2)),
    {save_config, [{channel, Channel}]}.

create_token(Config) ->
    {get_channels, SavedConfig} = ?config(saved_config, Config),
    Channel = proplists:get_value(channel, SavedConfig),
    BasicAuth = ?config(auth, Config),
    Api = ?config(api, Config) ++ "/channels/" ++ integer_to_list(Channel) ++ "/token",
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "{\"name\":\"token1\"}"}]),
    201 = proplists:get_value(status_code, Res),
    [$t,$.|_] = proplists:get_value(body, Res),
    {save_config, [{channel, Channel}]}.

del_channel(Config) ->
    {create_token, SavedConfig} = ?config(saved_config, Config),
    Channel = proplists:get_value(channel, SavedConfig),
    BasicAuth = ?config(auth, Config),
    Api = ?config(api, Config) ++ "/channels/" ++ integer_to_list(Channel),
    Res = delete(Api, [{headers, [{"Authorization", BasicAuth}]}]),
    200 = proplists:get_value(status_code, Res),
    "OK" = proplists:get_value(body, Res),
    Config.
    
v2_get_channels(Config) ->
    BasicAuth = ?config(auth, Config),
    Channel = ?config(channel, Config),
    Api = ?config(api, Config) ++ "/v2/channels/" ++ integer_to_list(Channel),
    Res = get_(Api, [{headers, [{"Authorization", BasicAuth}]}]),
    200 = proplists:get_value(status_code, Res),
    {struct, Json} = mochijson2:decode(proplists:get_value(body, Res)),
    Channel = proplists:get_value(<<"channel_id">>, Json),
    [] = proplists:get_value(<<"tokens">>, Json),
    [] = proplists:get_value(<<"drains">>, Json),
    {save_config, [{channel, Channel}]}.

v2_create_token(Config) ->
    {v2_get_channels, SavedConfig} = ?config(saved_config, Config),
    Channel = proplists:get_value(channel, SavedConfig),
    BasicAuth = ?config(auth, Config),
    Api = ?config(api, Config) ++ "/v2/channels/" ++ integer_to_list(Channel) ++ "/tokens",
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "{\"name\":\"token1\"}"}]),
    201 = proplists:get_value(status_code, Res),
    {struct, Json} = mochijson2:decode(proplists:get_value(body, Res)),
    <<"token1">> = proplists:get_value(<<"name">>, Json),
    <<"t.", _/binary>> = proplists:get_value(<<"token">>, Json),
    {save_config, [{channel, Channel}]}.

v2_del_channels(Config) ->
    {v2_create_token, SavedConfig} = ?config(saved_config, Config),
    Channel = proplists:get_value(channel, SavedConfig),
    BasicAuth = ?config(auth, Config),
    error_logger:info_msg("Channel is ~p", [Channel]),
    Api = ?config(api, Config) ++ "/v2/channels/" ++ integer_to_list(Channel),
    Res = delete(Api, [{headers, [{"Authorization", BasicAuth}]}]),
    200 = proplists:get_value(status_code, Res),
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
delete(Url, Opts) ->
    request(delete, Url, Opts).

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

get_token(TokenId) ->
    case logplex_token:lookup(TokenId) of
        undefined ->
            timer:sleep(50),
            get_token(TokenId);
        Token ->
            Token
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
