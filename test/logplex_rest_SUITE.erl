-module(logplex_rest_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/logplex_logging.hrl").
-compile(export_all).

groups() ->
    [{v3_channels, [sequence],
      [v3_channel_create
      , v3_channel_get
      , v3_channel_delete
      ]}
    ].

all() ->
    [{group, v3_channels}
    ].

init_per_suite(Config) ->
    logplex_test_helpers:set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    [{api, "http://localhost:"++integer_to_list(logplex_app:config(http_log_input_port))}
     , {auth, "Basic " ++ logplex_app:config(auth_key)} | Config].

end_per_suite(_Config) ->
    application:stop(logplex).

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

create_channel(Config, TokenNames) ->
    Uri = ?config(api, Config) ++ "/v3/channels",
    Body = mochijson2:encode({struct, [{tokens, TokenNames}]}),
    ?INFO("SENDING: ~p", [iolist_to_binary(Body)]),
    Result = logplex_http_helpers:post(Uri, [{content_type, "application/json"},
                                             {body, iolist_to_binary(Body)},
                                             {authorization, proplists:get_value(auth, Config, "")}]),
    Status = proplists:get_value(status_code, Result, error),
    ?INFO("Status: ~p, ~p", [Status, Result]),
    ?INFO("Returned Body: ~p", [proplists:get_value(body, Result, "")]),
    case Status of
        200 ->
            {struct, Info} = mochijson2:decode(proplists:get_value(body, Result, "")),
            ?INFO("Deserialized payload: ~p", [Info]),
            {struct, Tokens} = proplists:get_value(<<"tokens">>, Info, []),
            {proplists:get_value(<<"channel_id">>, Info, undefined), Tokens};
        _ -> failure
    end.

v3_channel_create(Config) ->
    case create_channel(Config, [<<"app">>]) of
        {ChannelId, [{<<"app">>, _Token}]} when is_integer(ChannelId) ->
            ok;
        _ ->
            exit(failed_channel_create)
    end.

v3_channel_get(Config) ->
    {ChannelId, _} = create_channel(Config, [<<"app">>]),
    timer:sleep(5000), %% Wait for ets sync
    Uri = ?config(api, Config) ++ "/v3/channels/" ++ integer_to_list(ChannelId),
    ct:pal("Requesting url: ~p", [Uri]),
    Result = logplex_http_helpers:get_(Uri, [{authorization, proplists:get_value(auth, Config, "")}]),
    case proplists:get_value(status_code, Result, error) of
        200 ->
            ok;
        Status ->
            ct:pal("Response code returned: ~p", [Status]),
            exit(created_channel_not_gotten)
    end.

v3_channel_delete(Config) ->
    {ChannelId, _} = create_channel(Config, [<<"app">>]),
    timer:sleep(5000), %% Wait for ets sync
    Uri = ?config(api, Config) ++ "/v3/channels/" ++ integer_to_list(ChannelId),
    Result = logplex_http_helpers:delete(Uri, [{authorization, proplists:get_value(auth, Config, "")}]),
    Status = proplists:get_value(status_code, Result, error),
    ct:pal("Response code returned: ~p", [Status]),
    204 = Status.
