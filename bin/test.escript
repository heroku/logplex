#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name logplex_console@`hostname` -env HTTP_PORT 8002 -env LOGPLEX_AUTH_KEY secret -env LOGPLEX_WORKERS 1 -env LOGPLEX_DRAIN_WRITERS 1 -env LOGPLEX_REDIS_WRITERS 1 -pa ebin -pa deps/mochiweb/ebin -pa deps/redis_pool/ebin -pa deps/erlang_syslog/ebin

main(_) ->
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok = application:start(logplex),

    {ok,{{_,200,_},_,_}} = httpc:request(get, {"http://localhost:8002/healthcheck", headers()}, [], []),

    {ok,{{_,201,_},_,ChannelId}} = httpc:request(post, 
        {"http://localhost:8002/channels", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
            {"app_id", 1},
            {"name", <<"app1@logplex.heroku.com">>}
        ]}))}, [], []),

    {ok,{{_,201,_},_,Token}} = httpc:request(post, 
        {"http://localhost:8002/channels/" ++ ChannelId ++ "/token", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
            {"addon", <<"advanced">>},
            {"channel_id", list_to_binary(ChannelId)},
            {"name", <<"app">>}
        ]}))}, [], []),

    {ok,{{_,201,_},_,HerokuToken}} = httpc:request(post, 
        {"http://localhost:8002/channels/" ++ ChannelId ++ "/token", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
            {"addon", <<"advanced">>},
            {"channel_id", list_to_binary(ChannelId)},
            {"name", <<"heroku">>}
        ]}))}, [], []),

    {ok,{{_,201,_},_,Session}} = httpc:request(post, 
        {"http://localhost:8002/sessions", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
            {"channel_id", list_to_binary(ChannelId)},
            {"name", <<"app1@logplex.heroku.com">>}
        ]}))}, [], []),

    {ok,{{_,201,_},_,_}} = httpc:request(post, 
        {"http://localhost:8002/channels/" ++ ChannelId ++ "/drains", headers(), content_type(), iolist_to_binary(mochijson2:encode({struct, [
            {"host", <<"127.0.0.1">>},
            {"port", 514}
        ]}))}, [], []),

    {ok,{{_,200,_},_,_}} = httpc:request(get, {"http://localhost:8002" ++ Session, headers()}, [], []),

    {ok,{{_,200,_},_,_}} = httpc:request(delete, {"http://localhost:8002/channels/" ++ ChannelId, headers()}, [], []),

    {ok,{{_,200,_},_,_}} = httpc:request(delete, {"http://localhost:8002/channels/" ++ ChannelId ++ "/drains?host=127.0.0.1&port=514", headers()}, [], []),

    %% write more tests... %%

    io:format("OK: tests passed.~n").

content_type() ->
    "application/json".

headers() ->
    [{"Authorization", "secret"}, {"Content-Type", content_type()}].