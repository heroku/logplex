-module(logplex_api_v3_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() ->
    [channel_service_unavailable
     , channel_method_not_allowed
     , create_channel_without_tokens
     , create_channel_with_tokens
     , update_channel_with_tokens
     , update_channel_and_remove_some_tokens
     , update_channel_and_nuke_tokens
     , get_channel_without_tokens
     , get_channel_with_tokens
     , delete_channel
    ].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    [{api_v3_url, logplex_app:config(http_v3_url)}
     , {auth, "Basic " ++ logplex_app:config(auth_key)}
     | Config].

end_per_suite(Config) ->
    application:stop(logplex),
    Config.

init_per_testcase(channel_service_unavailable, Config) ->
    logplex_app:set_config(api_status, disabled),
    Config;
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(channel_service_unavailable, Config) ->
    logplex_app:set_config(api_status, normal),
    Config;
end_per_testcase(_, Config) ->
    Config.

channel_service_unavailable(Config) ->
    Channel = new_channel(),
    Url = ?config(api_v3_url, Config) ++ "/v3/channels/" ++ Channel,
    Props = logplex_api_SUITE:request(get, Url, []),
    ?assertEqual(503, proplists:get_value(status_code, Props)),
    ?assertEqual("Service Unavailable", proplists:get_value(http_reason, Props)),
    Config.

channel_method_not_allowed(Config) ->
    Channel = new_channel(),
    Url = ?config(api_v3_url, Config) ++ "/v3/channels/" ++ Channel,
    [begin
         Props = logplex_api_SUITE:request(Method, Url, []),
         ?assertEqual(405, proplists:get_value(status_code, Props)),
         ?assertEqual("Method Not Allowed", proplists:get_value(http_reason, Props))
     end || Method <- [post, head, options]],
    Config.

create_channel_without_tokens(Config) ->
    Channel = new_channel(),
    Props = put_channel(Channel, [], Config),
    ct:pal("put channel resp: ~p~n", [Props]),
    Body = proplists:get_value(body, Props),
    Headers = proplists:get_value(headers, Props),
    Resp = jsx:decode(list_to_binary(Body), [return_maps]),
    ?assertMatch(Channel, binary_to_list(maps:get(<<"channel">>, Resp))),
    ?assertEqual(201, proplists:get_value(status_code, Props)),
    ?assertEqual("Created", proplists:get_value(http_reason, Props)),
    ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
    ?assert(is_list(proplists:get_value("request-id", Headers))),
    [{channel, Channel} | Config].

create_channel_with_tokens(Config) ->
    Channel = new_channel(),
    Tokens = [new_token_name() || _ <- lists:seq(1,5)],
    Props = put_channel(Channel, Tokens, Config),
    Body = proplists:get_value(body, Props),
    Headers = proplists:get_value(headers, Props),
    Resp = jsx:decode(list_to_binary(Body), [return_maps]),
    ReceivedTokens = maps:to_list(maps:get(<<"tokens">>, Resp)),
    ?assertEqual(length(Tokens), length(ReceivedTokens)),
    ?assert(lists:all(fun(Token) -> lists:member(Token, Tokens) end,
                      [binary_to_list(Token) || {Token, _} <- ReceivedTokens])),
    ?assertMatch(Channel, binary_to_list(maps:get(<<"channel">>, Resp))),
    ?assertEqual(201, proplists:get_value(status_code, Props)),
    ?assertEqual("Created", proplists:get_value(http_reason, Props)),
    ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
    ?assert(is_list(proplists:get_value("request-id", Headers))),
    [{channel, Channel}
     , {tokens, Tokens}
     | Config].

update_channel_with_tokens(Config0) ->
    Config = create_channel_without_tokens(Config0),
    Channel = ?config(channel, Config),
    Tokens = [new_token_name() || _ <- lists:seq(1,5)],
    Props = put_channel(Channel, Tokens, Config),
    ct:pal("put channel resp: ~p~n", [Props]),
    Body = proplists:get_value(body, Props),
    Headers = proplists:get_value(headers, Props),
    Resp = jsx:decode(list_to_binary(Body), [return_maps]),
    ReceivedTokens = maps:to_list(maps:get(<<"tokens">>, Resp)),
    ?assertEqual(length(Tokens), length(ReceivedTokens)),
    ?assert(lists:all(fun(Token) -> lists:member(Token, Tokens) end,
                      [binary_to_list(Token) || {Token, _} <- ReceivedTokens])),
    ?assertMatch(Channel, binary_to_list(maps:get(<<"channel">>, Resp))),
    ?assertEqual(200, proplists:get_value(status_code, Props)),
    ?assertEqual("OK", proplists:get_value(http_reason, Props)),
    ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
    ?assert(is_list(proplists:get_value("request-id", Headers))),
    [{tokens, Tokens} | Config].

update_channel_and_remove_some_tokens(Config0) ->
    Config = create_channel_with_tokens(Config0),
    Channel = ?config(channel, Config),
    Tokens = lists:nthtail(2, ?config(tokens, Config)),
    Props = put_channel(Channel, Tokens, Config),
    Body = proplists:get_value(body, Props),
    Headers = proplists:get_value(headers, Props),
    Resp = jsx:decode(list_to_binary(Body), [return_maps]),
    ReceivedTokens = maps:to_list(maps:get(<<"tokens">>, Resp)),
    ?assertEqual(length(Tokens), length(ReceivedTokens)),
    ?assert(lists:all(fun(Token) -> lists:member(Token, Tokens) end,
                      [binary_to_list(Token) || {Token, _} <- ReceivedTokens])),
    ?assertMatch(Channel, binary_to_list(maps:get(<<"channel">>, Resp))),
    ?assertEqual(200, proplists:get_value(status_code, Props)),
    ?assertEqual("OK", proplists:get_value(http_reason, Props)),
    ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
    ?assert(is_list(proplists:get_value("request-id", Headers))),
    [{tokens, Tokens} | Config].

update_channel_and_nuke_tokens(Config0) ->
    Config = create_channel_with_tokens(Config0),
    Channel = ?config(channel, Config),
    Props = put_channel(Channel, [], Config),
    Body = proplists:get_value(body, Props),
    Headers = proplists:get_value(headers, Props),
    Resp = jsx:decode(list_to_binary(Body), [return_maps]),
    ?assertMatch(Channel, binary_to_list(maps:get(<<"channel">>, Resp))),
    ?assertEqual(200, proplists:get_value(status_code, Props)),
    ?assertEqual("OK", proplists:get_value(http_reason, Props)),
    ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
    ?assert(is_list(proplists:get_value("request-id", Headers))),
    ?assert(not maps:is_key(<<"tokens">>, Resp)),
    Config.

get_channel_without_tokens(Config0) ->
    Config = create_channel_without_tokens(Config0),
    Channel = ?config(channel, Config),
    Props = get_channel(Channel, Config),
    Body = proplists:get_value(body, Props),
    Headers = proplists:get_value(headers, Props),
    Resp = jsx:decode(list_to_binary(Body), [return_maps]),
    ?assertMatch(Channel, binary_to_list(maps:get(<<"channel">>, Resp))),
    ?assertEqual(200, proplists:get_value(status_code, Props)),
    ?assertEqual("OK", proplists:get_value(http_reason, Props)),
    ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
    ?assert(is_list(proplists:get_value("request-id", Headers))),
    Config.

get_channel_with_tokens(Config0) ->
    Config = create_channel_with_tokens(Config0),
    Channel = ?config(channel, Config),
    Tokens = ?config(tokens, Config),
    Props = get_channel(Channel, Config),
    Body = proplists:get_value(body, Props),
    Headers = proplists:get_value(headers, Props),
    Resp = jsx:decode(list_to_binary(Body), [return_maps]),
    ReceivedTokens = maps:to_list(maps:get(<<"tokens">>, Resp)),
    ?assertEqual(length(Tokens), length(ReceivedTokens)),
    ?assert(lists:all(fun(Token) -> lists:member(Token, Tokens) end,
                      [binary_to_list(Token) || {Token, _} <- ReceivedTokens])),
    ?assertMatch(Channel, binary_to_list(maps:get(<<"channel">>, Resp))),
    ?assertEqual(200, proplists:get_value(status_code, Props)),
    ?assertEqual("OK", proplists:get_value(http_reason, Props)),
    ?assertEqual("application/json", proplists:get_value("content-type", Headers)),
    ?assert(is_list(proplists:get_value("request-id", Headers))),
    Config.

delete_channel(Config0) ->
    Config = create_channel_without_tokens(Config0),
    Channel = ?config(channel, Config),
    Props = delete_channel(Channel, Config),
    ct:pal("delete channel resp: ~p~n", [Props]),
    ?assertEqual(204, proplists:get_value(status_code, Props)),
    ?assertEqual("No Content", proplists:get_value(http_reason, Props)).

put_channel(Channel, Tokens, Config) ->
    Url = ?config(api_v3_url, Config) ++ "/v3/channels/" ++ Channel,
    Headers = [{"Authorization", ?config(auth, Config)}],
    TokenList = [list_to_binary(Token) || Token <- Tokens],
    JSON = jsx:encode(maps:from_list([{<<"tokens">>, TokenList} || length(TokenList) > 0])),
    Opts = [{headers, Headers}, {body, JSON}, {timeout, timer:seconds(10)}],
    logplex_api_SUITE:put_(Url, Opts).

get_channel(Channel, Config) ->
    Url = ?config(api_v3_url, Config) ++ "/v3/channels/" ++ Channel,
    Headers = [{"Authorization", ?config(auth, Config)}],
    Opts = [{headers, Headers}, {timeout, timer:seconds(10)}],
    logplex_api_SUITE:get_(Url, Opts).

delete_channel(Channel, Config) ->
    Url = ?config(api_v3_url, Config) ++ "/v3/channels/" ++ Channel,
    Headers = [{"Authorization", ?config(auth, Config)}],
    Opts = [{headers, Headers}, {timeout, timer:seconds(10)}],
    logplex_api_SUITE:request(delete, Url, Opts).


new_channel() ->
    "app-" ++ uuid:to_string(uuid:v4()).

new_token_name() ->
    "token-" ++ uuid:to_string(uuid:v4()).

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
