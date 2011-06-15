-module(logplex_api_tests).
-include_lib("eunit/include/eunit.hrl").

logplex_api_test_() ->
    Port =
        case os:getenv("HTTP_PORT") of
            false -> "80";
            Val -> Val
        end,
    AppId = 1,
    AppName = "app1@logplex.heroku.com",
    MsgBodies = [
        {heroku_token, "web.1", "State changed from down to starting"},
        {app_token, "web.1", "Listening on 0.0.0.0:54548, CTRL+C to stop"},
        {app_token, "web.1", "Thin web server (v1.2.6 codename Crazy Delicious)"},
        {heroku_token, "web.1", "State changed from starting to up"},
        {heroku_token, "router", "GET " ++ AppName ++ ".heroku.com/ dyno=web.1 queue=0 wait=0ms service=10ms bytes=215"}
    ],
    {ok, UdpSock} = gen_udp:open(0),
    {setup,
        fun inets:start/0,
        [{"Healthcheck", 
            ?_assertMatch({ok,{{_,200,_},_,"OK"}}, httpc:request(get, {"http://localhost:" ++ Port ++ "/healthcheck", headers()}, [], []))},
         {"Cloudkick",
            ?_assertMatch({ok,{{_,200,_},_,"{\"state\":\"ok\",\"metrics\":[]}"}}, httpc:request(get, {"http://localhost:8008", headers()}, [], []))},
         {"Missing channel name for create channel",
            ?_assertMatch({ok,{{_,400,_},_,"'name' post param missing"}},
                httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/channels", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"app_id", AppId}
                    ]}))}, [], []))},
        {"Poorly formatted channel name for create channel",
           ?_assertMatch({ok,{{_,400,_},_,"'name' post param missing"}},
               httpc:request(post, 
                   {"http://localhost:" ++ Port ++ "/channels", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                       {"app_id", AppId}
                   ]}))}, [], []))},
         {"Create channel",
            fun() ->
                Ret = httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/channels", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"app_id", AppId},
                        {"name", list_to_binary(AppName)}
                    ]}))}, [], []),
                ?assertMatch({ok,{{_,201,_},_,_}}, Ret),
                {ok,{{_,201,_},_,ChannelId}} = Ret,
                put(channel_id, ChannelId)
            end},
         {"Create app token",
            fun() ->
                Ret = httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/token", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"channel_id", list_to_binary(get(channel_id))},
                        {"name", <<"app">>}
                    ]}))}, [], []),
                ?assertMatch({ok,{{_,201,_},_,_}}, Ret),
                {ok,{{_,201,_},_,Token}} = Ret,
                put(app_token, Token)
            end},
         {"Create Heroku token",
            fun() ->
                Ret = httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/token", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"channel_id", list_to_binary(get(channel_id))},
                        {"name", <<"heroku">>}
                    ]}))}, [], []),
                ?assertMatch({ok,{{_,201,_},_,_}}, Ret),
                {ok,{{_,201,_},_,Token}} = Ret,
                put(heroku_token, Token)
            end},
         {"Create session",
            fun() ->
                Ret = httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/sessions", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"channel_id", list_to_binary(get(channel_id))},
                        {"name", list_to_binary(AppName)}
                    ]}))}, [], []),
                ?assertMatch({ok,{{_,201,_},_,_}}, Ret),
                {ok,{{_,201,_},_,Session}} = Ret,
                put(session, Session)
            end},
         {"Create drain",
            ?_assertMatch({ok,{{_,201,_},_,"Successfully added drain syslog://10.0.0.1:514"}},
                httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/drains", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"host", <<"10.0.0.1">>},
                        {"port", 514}
                    ]}))}, [], []))},
         {"Create duplicate drain",
            ?_assertMatch({ok,{{_,400,_},_,"Drain syslog://10.0.0.1:514 already exists"}},
                httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/drains", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"host", <<"10.0.0.1">>},
                        {"port", 514}
                    ]}))}, [], []))},
         {"Get info",
            fun() ->
                Ret = httpc:request(get, {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/info", headers()}, [], []),
                InfoStr =
                "{\"channel_id\":" ++ get(channel_id) ++ "," ++
                 "\"channel_name\":\"" ++ AppName ++ "\"," ++
                 "\"app_id\":" ++ integer_to_list(AppId) ++ "," ++
                 "\"tokens\":{\"app\":\"" ++ get(app_token) ++ "\",\"heroku\":\"" ++ get(heroku_token) ++ "\"}," ++
                 "\"drains\":[\"syslog://10.0.0.1:514\"]}",
                ?assertMatch({ok,{{_,200,_},_,InfoStr}}, Ret)
            end},
         {"Get drain list",
            ?_assertMatch({ok,{{_,200,_},_,"[{\"host\":\"10.0.0.1\",\"port\":514}]"}},
                httpc:request(get, {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/drains", headers()}, [], []))},
         {"Input logs",
            fun() ->
                [begin
                    gen_udp:send(UdpSock, "127.0.0.1", 9999, iolist_to_binary([
                        "<40>1 ", logplex_utils:formatted_utc_date(), " ", atom_to_list(node()), " ", get(SourceKey), " ", Ps, " - - ", Body
                    ]))
                end || {SourceKey, Ps, Body} <- MsgBodies],
                timer:sleep(100)
            end},
         {"Verify logs",
            fun() ->
                Ret = httpc:request(get, {"http://localhost:" ++ Port ++ get(session), headers()}, [], []),
                ?assertMatch({ok,{{_,200,_},_,_}}, Ret),
                {ok,{{_,200,_},_,Logs}} = Ret,
                Logs1 = string:tokens(Logs, "\n"),
                [begin
                    Match = parse_log(Log),
                    ?assertMatch({match,[_,_,_]}, Match),
                    {match,[Source1, Ps1, Body1]} = Match,
                    ?assertEqual(source_key_to_source(SourceKey), Source1),
                    ?assertEqual(Ps, Ps1),
                    ?assertEqual(Body, Body1)
                end || {Log, {SourceKey, Ps, Body}} <- lists:zip(Logs1, MsgBodies)]
            end},
         {"Create session",
            fun() ->
                Ret = httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/sessions", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"channel_id", list_to_binary(get(channel_id))},
                        {"name", list_to_binary(AppName)},
                        {"source", <<"app">>}
                    ]}))}, [], []),
                ?assertMatch({ok,{{_,201,_},_,_}}, Ret),
                {ok,{{_,201,_},_,Session}} = Ret,
                put(session, Session)
            end},
         {"Verify source filter",
            fun() ->
                Ret = httpc:request(get, {"http://localhost:" ++ Port ++ get(session), headers()}, [], []),
                ?assertMatch({ok,{{_,200,_},_,_}}, Ret),
                {ok,{{_,200,_},_,Logs}} = Ret,
                Logs1 = string:tokens(Logs, "\n"),
                [begin
                    Match = parse_log(Log),
                    ?assertMatch({match,[_,_,_]}, Match),
                    {match,[Source1, Ps1, Body1]} = Match,
                    ?assertEqual(source_key_to_source(SourceKey), Source1),
                    ?assertEqual(Ps, Ps1),
                    ?assertEqual(Body, Body1)
                end || {Log, {SourceKey, Ps, Body}} <- lists:zip(Logs1, [Msg || {app_token, _, _}=Msg <- MsgBodies])]
            end},
         {"Create session",
            fun() ->
                Ret = httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/sessions", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"channel_id", list_to_binary(get(channel_id))},
                        {"name", list_to_binary(AppName)},
                        {"ps", <<"router">>}
                    ]}))}, [], []),
                ?assertMatch({ok,{{_,201,_},_,_}}, Ret),
                {ok,{{_,201,_},_,Session}} = Ret,
                put(session, Session)
            end},
         {"Verify ps filter",
            fun() ->
                Ret = httpc:request(get, {"http://localhost:" ++ Port ++ get(session), headers()}, [], []),
                ?assertMatch({ok,{{_,200,_},_,_}}, Ret),
                {ok,{{_,200,_},_,Logs}} = Ret,
                Logs1 = string:tokens(Logs, "\n"),
                [begin
                    Match = parse_log(Log),
                    ?assertMatch({match,[_,_,_]}, Match),
                    {match,[Source1, Ps1, Body1]} = Match,
                    ?assertEqual(source_key_to_source(SourceKey), Source1),
                    ?assertEqual(Ps, Ps1),
                    ?assertEqual(Body, Body1)
                end || {Log, {SourceKey, Ps, Body}} <- lists:zip(Logs1, [Msg || {_, "router", _}=Msg <- MsgBodies])]
            end},
         {"Create session",
           fun() ->
               Ret = httpc:request(post, 
                   {"http://localhost:" ++ Port ++ "/sessions", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                       {"channel_id", list_to_binary(get(channel_id))},
                       {"name", list_to_binary(AppName)}
                   ]}))}, [], []),
               ?assertMatch({ok,{{_,201,_},_,_}}, Ret),
               {ok,{{_,201,_},_,Session}} = Ret,
               put(session, Session)
           end},
         {"Delete drain",
            ?_assertMatch({ok,{{_,200,_},_,_}},
                httpc:request(delete, {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/drains?host=10.0.0.1&port=514", headers()}, [], []))},
        {"Create drain",
            ?_assertMatch({ok,{{_,201,_},_,"Successfully added drain syslog://10.0.0.1:514"}},
                httpc:request(post, 
                    {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/drains", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
                        {"host", <<"10.0.0.1">>},
                        {"port", 514}
                    ]}))}, [], []))},
        {"Clear drains",
           ?_assertMatch({ok,{{_,200,_},_,_}},
               httpc:request(delete, {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/drains", headers()}, [], []))},
         {"Delete non-existent drain",
            ?_assertMatch({ok,{{_,404,_},_,"Drain syslog://10.0.0.1:514 does not exist"}},
                httpc:request(delete, {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id) ++ "/drains?host=10.0.0.1&port=514", headers()}, [], []))},
         {"Delete channel",
            ?_assertMatch({ok,{{_,200,_},_,_}},
                httpc:request(delete, {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id), headers()}, [], []))},
         {"Delete non-existent channel",
            ?_assertMatch({ok,{{_,404,_},_,"Not found"}},
               httpc:request(delete, {"http://localhost:" ++ Port ++ "/channels/" ++ get(channel_id), headers()}, [], []))}
        ]
    }.

content_type() ->
    "application/json".

headers() ->
    [{"Authorization", os:getenv("LOGPLEX_AUTH_KEY")}, {"Content-Type", content_type()}].

source_key_to_source(app_token) -> "app";
source_key_to_source(heroku_token) -> "heroku".

parse_log(Log) ->
    re:run(Log, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}-\\d{2}:\\d{2} (\\S+)\\[(\\S+)\\]: (.*)$", [{capture, all_but_first, list}]).
