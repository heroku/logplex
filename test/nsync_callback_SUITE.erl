-module(nsync_callback_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

all() ->
    [run_eunit,
    find_token_with_missing_channel,
    find_token_with_channel,
    find_token_with_invalid_channel,
    create_token_with_invalid_channel,
    create_or_update_drain_with_missing_channel,
    create_or_update_drain_with_invalid_channel].

init_per_suite(Config) ->
    ok = logplex_app:a_start(logplex, temporary),
    Config.

end_per_suite(_Config) ->
    application:stop(logplex).

run_eunit(_Config) ->
    ok = eunit:test(nsync_callback).

find_token_with_missing_channel(Config) ->
    Result = nsync_callback:find_token("12345", dict:new()),
    ?assertEqual({error, missing_channel}, Result),
    Config.

find_token_with_channel(Config) ->
    Dict1 = dict:store(<<"ch">>, <<"12345">>, dict:new()),
    Dict2 = dict:store(<<"name">>, <<"testing">>, Dict1),
    Result = nsync_callback:find_token(<<"testing">>, Dict2),
    ?assertEqual({ok,{token,<<"testing">>,12345,<<"testing">>}}, Result),
    Config.

find_token_with_invalid_channel(Config) ->
    Dict1 = dict:store(<<"ch">>, <<"abcdefg">>, dict:new()),
    Dict2 = dict:store(<<"name">>, <<"testing">>, Dict1),
    Result = nsync_callback:find_token(<<"testing">>, Dict2),
    ?assertEqual({error, bad_token}, Result),
    Config.

create_token_with_invalid_channel(Config) ->
    Dict1 = dict:store(<<"ch">>, <<"abcdefg">>, dict:new()),
    Dict2 = dict:store(<<"name">>, <<"testing">>, Dict1),
    Result = nsync_callback:create_token(<<"testing">>, Dict2),
    ?assertEqual(ok, Result),
    Config.

create_or_update_drain_with_missing_channel(Config) ->
    Result = nsync_callback:create_or_update_drain("12345", dict:new()),
    ?assertEqual(ok, Result),
    Config.

create_or_update_drain_with_invalid_channel(Config) ->
    Dict1 = dict:store(<<"ch">>, <<"abcdefg">>, dict:new()),
    Dict2 = dict:store(<<"name">>, <<"testing">>, Dict1),
    Dict3 = dict:store(<<"token">>, <<"testing">>, Dict2),
    Result = nsync_callback:create_or_update_drain(<<"testing">>, Dict3),
    ?assertEqual(ok, Result),
    Config.
