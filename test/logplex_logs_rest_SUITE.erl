-module(logplex_logs_rest_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [v2_redirects, v1_redirects_channels, v1_redirects_sessions,
     post_logline, post_logline_compressed].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    [{logs, "http://localhost:"++integer_to_list(logplex_app:config(http_log_input_port))}
     ,{auth, "Basic " ++ logplex_app:config(auth_key)} | Config].

end_per_suite(_Config) ->
    application:stop(logplex).

init_per_testcase(Case, Config)
  when Case =:= v2_redirects;
       Case =:= v1_redirects_channels;
       Case =:= v1_redirects_sessions;
       Case =:= post_logline;
       Case =:= post_logline_compressed ->
    Channel = logplex_channel:create(atom_to_binary(Case, latin1)),
    ChannelId = logplex_channel:id(Channel),
    Token = logplex_token:create(ChannelId, atom_to_binary(Case, latin1)),
    logplex_SUITE:wait_for_chan(Channel),
    logplex_channel:register({channel, ChannelId}),
    logplex_SUITE:wait_for_registration({channel, ChannelId}),
    case Case of
        C when C =:= post_logline; C =:= post_logline_compressed ->
            %% intercept this bit:
            %% logplex_message:process_msgs(Msgs, ChannelId, Token, Name),
            meck:new(logplex_api, [no_link, passthrough]),
            meck:expect(logplex_api, process_msgs, fun(_) -> ok end);
        _ -> ok
    end,
    [{channel_id, ChannelId}, {token, Token} |Config].

end_per_testcase(Case, Config)
  when Case =:= post_logline;
       Case =:= post_logline_compressed ->
    meck:unload(logplex_api),
    Config;
end_per_testcase(_Case, Config) ->
    Config.

v2_redirects(Config) ->
    BasicAuth = ?config(auth, Config),
    Logs = ?config(logs, Config) ++ "/v2/channels/",
    ChannelId = ?config(channel_id, Config),
    Get = Logs ++ integer_to_list(ChannelId),
    %% Get = ?config(logs, Config) ++ "/healthcheck",
    Res = logplex_api_SUITE:get_(Get, [{headers, [{"Authorization", BasicAuth}]},
                                       {http_opts, [{autoredirect, false}]}]),
    302 = proplists:get_value(status_code, Res),
    ok.

v1_redirects_channels(Config) ->
    BasicAuth = ?config(auth, Config),
    ChannelId = ?config(channel_id, Config),
    Get = binary_to_list(iolist_to_binary([?config(logs, Config),
           "/channels/",
           integer_to_list(ChannelId),
           "/info"])),
    Res = logplex_api_SUITE:get_(Get, [{headers, [{"Authorization", BasicAuth}]},
                                       {http_opts, [{autoredirect, false}]}]),
    302 = proplists:get_value(status_code, Res),
    ok.

v1_redirects_sessions(Config) ->
    BasicAuth = ?config(auth, Config),
    Post = binary_to_list(iolist_to_binary([?config(logs, Config),
           "/sessions/"])),
    PostRes = logplex_api_SUITE:post(Post, [{headers, [{"Authorization", BasicAuth}]},
                                       {http_opts, [{autoredirect, false}]}]),
    302 = proplists:get_value(status_code, PostRes),
    ok.

post_logline(Config) ->
    BasicAuth = ?config(auth, Config),
    Post = binary_to_list(iolist_to_binary([?config(logs, Config),
           "/sessions/"])),
    PostRes = logplex_api_SUITE:post(Post, [{headers, [{"Authorization", BasicAuth}]},
                                            {http_opts, [{autoredirect, false}]}]),
    201 = proplists:get_value(status_code, PostRes),

    History = meck:history(logplex_api),
    ct:pal("hist ~p", [History]),

    ok.

post_logline_compressed(Config) ->
    BasicAuth = ?config(auth, Config),
    Post = binary_to_list(iolist_to_binary([?config(logs, Config),
           "/sessions/"])),
    PostRes = logplex_api_SUITE:post(Post, [{headers, [{"Authorization", BasicAuth}]},
                                       {http_opts, [{autoredirect, false}]}]),
    302 = proplists:get_value(status_code, PostRes),
    ok.


set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", net_adm:localhost()},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_NODE_NAME", atom_to_list(node())},
         {"LOGPLEX_API_ENDPOINT_URL", "http://localhost:8001"}
        ]].

