-module(logplex_logs_rest_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

-define(BODY(Token),
        <<"424 <133>1 2016-08-24T21:58:41.445160+00:00 ip-10-69-200-199 ",
          Token/binary,
          " endosome-ssh-1 - [meta sequenceId=\"22984\"] app_pid=7915 slot=endosome cloud=staging.herokudev.com instance_id=192710 endosome version=v74-7-gd7dafe3 source=com.herokudev.staging.endosome.192710 pid=7871 request_id=0e9a8698-b0a7-4925-a8a9-44""3555af5f60 http.health.timer.end measure#endosome.http.health.elapsed.seconds=0.000008 \n487 <133>1 2016-08-24T21:58:41.445323+00:00 ip-10-69-200-199 t.fe244422-4071-48f1-8192-a28aa99f35cf endosome-web-1 - [meta sequenceId=\"22985\"] app_pid=7840 slot=endosome cloud=staging.herokudev.com instance_id=192710 endosome version=v74-7-gd7dafe3 source=com.herokudev.staging.endosome.192710 pid=7871 request_id=0e9a8698-b0a7-4925-a8a9-443555af5f60 http.request.timer.start method=GET path=/health user_agent='ELB-HealthChecker/1.0' content_encoding='' remote='10.183.54.105:48510' fwd=''\n372 <133>1 2016-08-24T21:58:41.445323+00:00 ip-10-69-200-199 t.fe244422-4071-48f1-8192-a28aa99f35cf endosome-web-1 - [meta sequenceId=\"22986\"] app_pid=7840 slot=endosome cloud=staging.herokudev.com instance_id=192710 endosome version=v74-7-gd7dafe3 source=com.herokudev.staging.endosome.192710 pid=7871 request_id=0e9a8698-b0a7-4925-a8a9-443555af5f60 http.health.timer.start \n584 <133>1 2016-08-24T21:58:41.445323+00:00 ip-10-69-200-199 t.fe244422-4071-48f1-8192-a28aa99f35cf endosome-web-1 - [meta sequenceId=\"22987\"] app_pid=7840 slot=endosome cloud=staging.herokudev.com instance_id=192710 endosome version=v74-7-gd7dafe3 source=com.herokudev.staging.endosome.192710 pid=7871 request_id=0e9a8698-b0a7-4925-a8a9-443555af5f60 http.request.timer.end measure#endosome.http.request.elapsed.seconds=0.000049 method=GET path=/health user_agent='ELB-HealthChecker/1.0' content_encoding='' remote='10.183.54.105:48510' fwd='' status=200 count#endosome.http.status.2xx=1\n389 <133>1 2016-08-24T21:58:41.445323+00:00 ip-10-69-200-199 t.fe244422-4071-48f1-8192-a28aa99f35cf endosome-web-1 - [meta sequenceId=\"22988\"] app_pid=7840 slot=endosome cloud=staging.herokudev.com instance_id=192710 endosome version=v74-7-gd7dafe3 source=com.herokudev.staging.endosome.192710 pid=7871 request_id=0e9a8698-b0a7-4925-a8a9-443555af5f60 count#endosome.endosome.workspaces.open=0\n">>).


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
            meck:new(logplex_message, [no_link, passthrough]),
            meck:expect(logplex_message, process_msgs, fun(_,_,_,_) -> ok end),
            meck:new(logplex_logs_rest, [no_link, passthrough]);
        _ -> ok
    end,
    [{channel_id, ChannelId}, {token, Token} |Config].

end_per_testcase(Case, Config)
  when Case =:= post_logline;
       Case =:= post_logline_compressed ->
    meck:unload(logplex_logs_rest),
    meck:unload(logplex_message),
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
           "/logs/"])),
    Token = ?config(token, Config),
    meck:expect(logplex_logs_rest, is_authorized,
                fun(Req, State) ->
                        State1 = setelement(2, State, Token),
                        State2 = setelement(3, State1, <<"dont'care">>),
                        State3 = setelement(4, State2, 3),
                        {true, Req, State3}
                end),
    PostRes = logplex_api_SUITE:post(Post, [{headers, [{"Authorization", BasicAuth}]},
                                            {content_type, "application/logplex-1"},
                                            {http_opts, [{autoredirect, false}]},
                                            {body, ?BODY(Token)}]),
    History = meck:history(logplex_message),
    %% ct:pal("hist ~p", [History]),

    [{_Pid, {logplex_message, process_msgs, [Msgs, _, _, _]}, ok}] = History,

    5 = length(Msgs),

    204 = proplists:get_value(status_code, PostRes),

    ok.

post_logline_compressed(Config) ->
    BasicAuth = ?config(auth, Config),
    Post = binary_to_list(iolist_to_binary([?config(logs, Config),
           "/logs/"])),
    Token = ?config(token, Config),
    meck:expect(logplex_logs_rest, is_authorized,
                fun(Req, State) ->
                        State1 = setelement(2, State, Token),
                        State2 = setelement(3, State1, <<"dont'care">>),
                        State3 = setelement(4, State2, 3),
                        {true, Req, State3}
                end),
    Body = ?BODY(Token),
    CompBody = zlib:gzip(Body),
    PostRes = logplex_api_SUITE:post(Post, [{headers, [{"Authorization", BasicAuth},
                                                       {"content-encoding", "gzip"}]},
                                            {content_type, "application/logplex-1"},
                                            {http_opts, [{autoredirect, false}]},
                                            {body, CompBody}]),
    History = meck:history(logplex_message),
    %% ct:pal("hist ~p", [History]),

    [{_Pid, {logplex_message, process_msgs, [Msgs, _, _, _]}, ok}] = History,

    5 = length(Msgs),

    204 = proplists:get_value(status_code, PostRes),

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
