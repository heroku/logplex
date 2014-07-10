%%%-------------------------------------------------------------------
%% @copyright Heroku, 2014
%% @author Alex Arnell <alex@heroku.com>
%% @doc CommonTest test suite for logplex_firehose
%% @end
%%%-------------------------------------------------------------------

-module(logplex_firehose_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [master_config, post_msg, distribution].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    Config.

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(_, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    meck:unload(),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

master_config(_Config) ->
    FirehoseChannelId = 21894100,
    ChannelId = 21894200,

    logplex_firehose:create_ets_tables(),
    logplex_firehose:enable(),

    undefined = logplex_firehose:next_shard(ChannelId, <<"app">>),
    undefined = logplex_firehose:next_shard(ChannelId, <<"filtered">>),

    application:set_env(logplex, firehose_channel_ids, lists:concat([FirehoseChannelId])),
    application:set_env(logplex, firehose_filter_tokens, "filtered"),
    logplex_firehose:enable(),

    undefined = logplex_firehose:next_shard(ChannelId, <<"app">>),
    undefined = logplex_firehose:next_shard(ChannelId, <<"app">>),
    FirehoseChannelId = logplex_firehose:next_shard(ChannelId, <<"filtered">>),
    FirehoseChannelId = logplex_firehose:next_shard(ChannelId, <<"filtered">>),

    logplex_firehose:disable(),
    undefined = logplex_firehose:next_shard(ChannelId, <<"app">>),
    undefined = logplex_firehose:next_shard(ChannelId, <<"filtered">>),
    ok.

post_msg(_Config) ->
    FirehoseChannelId = 21894100,
    ChannelId = 21894200,
    Msg1 = term_to_binary(make_ref()),
    Msg2 = term_to_binary(make_ref()),
    Msg3 = term_to_binary(make_ref()),

    meck:expect(logplex_channel, post_msg, [{[{channel, '_'}, '_'], ok}]),

    ok = logplex_firehose:post_msg(ChannelId, <<"app">>, Msg1),
    false = meck:called(logplex_channel, post_msg, [{channel, FirehoseChannelId}, Msg1]),

    ok = logplex_firehose:post_msg(ChannelId, <<"heroku">>, Msg1),
    false = meck:called(logplex_channel, post_msg, [{channel, FirehoseChannelId}, Msg1]),

    application:set_env(logplex, firehose_channel_ids, lists:concat([FirehoseChannelId])),
    application:set_env(logplex, firehose_filter_tokens, "heroku"),
    logplex_firehose:create_ets_tables(),
    logplex_firehose:enable(),

    ok = logplex_firehose:post_msg(ChannelId, <<"heroku">>, Msg1),
    true = meck:called(logplex_channel, post_msg, [{channel, FirehoseChannelId}, Msg1]),
    
    ok = logplex_firehose:post_msg(ChannelId, <<"app">>, Msg2),
    false = meck:called(logplex_channel, post_msg, [{channel, FirehoseChannelId}, Msg2]),

    ok = logplex_firehose:post_msg(FirehoseChannelId, <<"heroku">>, Msg3),
    false = meck:called(logplex_channel, post_msg, [{channel, FirehoseChannelId}, Msg3]),
    
    ok.

distribution(_Config) ->
    FirehoseChannelId = "21894100,21894101",
    ChannelId = 21894200,

    meck:expect(logplex_channel, post_msg, [{[{channel, '_'}, '_'], ok}]),

    application:set_env(logplex, firehose_channel_ids, FirehoseChannelId),
    application:set_env(logplex, firehose_filter_tokens, "heroku"),
    logplex_firehose:create_ets_tables(),
    logplex_firehose:enable(),

    [ send_messages(1000, ChannelId, <<"heroku">>) || _Id <- lists:seq(1, 100) ],

    meck:wait(1000*100, logplex_channel, post_msg, [{channel, '_'}, '_'], 15000),
    Calls1 = meck:num_calls(logplex_channel, post_msg, [{channel,21894100}, '_']),
    Calls2 = meck:num_calls(logplex_channel, post_msg, [{channel,21894101}, '_']),
    ct:pal("~p, ~p", [Calls1, Calls2]),

    % assert random distribution with 0.5% accuracy
    true = calls_within(Calls1, 1000*50, 0.005),
    true = calls_within(Calls2, 1000*50, 0.005),
    ok.

%%%--------------------------------------------------------------------
%%% private functions
%%%--------------------------------------------------------------------

calls_within(Amount, Target, Variance) when is_float(Variance) ->
    Delta = Target * Variance,
    calls_within(Amount, Target, round(Delta));

calls_within(Amount, Target, Delta)
  when is_integer(Delta),
       Amount =< Target + Delta,
       Amount >= Target - Delta ->
    true;
calls_within(_, _, _) ->
    false.

send_messages(Amount, Where, Token) ->
    spawn_link(fun () ->
                       send_messages_(Amount, Where, Token)
               end).

send_messages_(0, _Where, _Token) ->
    done;
send_messages_(Amount, Where, Token) ->
    ok = logplex_firehose:post_msg(Where, Token, make_message()),
    send_messages(Amount -1, Where, Token).

make_message() ->
    term_to_binary(make_ref()).
