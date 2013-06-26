-module(logplex_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [v2_canary_session].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    
    [{api, "http://localhost:"++integer_to_list(logplex_app:config(http_port))},
     {auth, "Basic " ++ logplex_app:config(auth_key)} | Config].

end_per_suite(_Config) ->
    application:stop(logplex).

v2_canary_session(Config) ->
    Api = ?config(api, Config) ++ "/v2/canary-sessions",
    BasicAuth = ?config(auth, Config),
    Res = post(Api, [{headers, [{"Authorization", BasicAuth}]},
                     {body, "RandomString"}]),
    201 = proplists:get_value(status_code, Res),
    {struct, [{<<"url">>, <<"/v2/canary-fetch/", _:36/binary>>}]} = 
        mochijson2:decode(proplists:get_value(body, Res)),
    Config.

%% Other helpers
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
    case httpc:request(Method, Request, [{timeout, Timeout}], []) of
        {ok, {{HttpVersion, StatusCode, HttpReason}, Headers0, Body}} ->
            [{status_code, StatusCode},
             {http_version, HttpVersion},
             {http_reason, HttpReason},
             {headers, Headers0},
             {body, Body}];
        {ok, {StatusCode, Body}} ->
            [{status_code, StatusCode},
             {body, Body}];
        Other ->
            error_logger:info_msg("Other is ~p", [Other]),
            Other
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
