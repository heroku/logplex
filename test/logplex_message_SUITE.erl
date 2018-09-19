-module(logplex_message_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/logplex_logging.hrl").

-compile(export_all).

all() -> [ {group, classic},
           {group, batch_redis}
         ].

tests_per_group() -> [ channel_not_found,
                       process_messages,
                       process_messages_no_redis
                     ].

groups() -> [ {classic, [], tests_per_group()},
              {batch_redis, [], tests_per_group()}
            ].

init_per_suite(Config) ->
    application:load(logplex), %% ensure default config is loaded
    [{cleanup_funs, []} %% cleanup funs run after each test
     | Config].

end_per_suite(Config) ->
    meck:unload(),
    Config.

init_per_group(batch_redis, Config) ->
    application:set_env(logplex, batch_redis, true),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(channel_not_found, Config0) ->
    Config = make_storage(channel_not_found, Config0),

    meck:new(logplex_shard_info),
    meck:expect(logplex_shard_info, read, fun(_) -> fake_info end),

    meck:new(logplex_channel),
    meck:expect(logplex_channel, lookup_flag, fun(_, _) -> not_found end),

    init_per_testcase(any, Config);

init_per_testcase(process_messages, ConfigBase) ->
    ChannelFlags = [],
    Config0 = prepare_test(process_messages, ChannelFlags, ConfigBase),
    Config = lists:foldr(fun(F, Acc) -> F(Acc) end,
                         Config0,
                         [fun mock_firehose/1,
                          fun mock_drains/1,
                          fun mock_tails/1,
                          fun mock_redis/1
                         ]),
    init_per_testcase(any, Config);

init_per_testcase(process_messages_no_redis, ConfigBase) ->
    ChannelFlags = [no_redis],
    Config0 = prepare_test(process_messages, ChannelFlags, ConfigBase),
    Config = lists:foldr(fun(F, Acc) -> F(Acc) end,
                         Config0,
                         [fun mock_firehose/1,
                          fun mock_drains/1,
                          fun mock_tails/1,
                          fun mock_redis/1
                         ]),
    init_per_testcase(any, Config);

init_per_testcase(_, Config) ->
    Table = ?config(table, Config),
    %% mock logging
    %% We don't want actually test logging, only that we log the expected lines.
    %% We write logs into an ets table. We use a duplicate bag such that all log
    %% lines can be stored under the same key.
    meck:new(syslog_lib),
    meck:expect(syslog_lib, notice, fun(_, Line) ->
                                            ets:insert(Table, {logs, Line})
                                    end),

    %% mock metrics
    meck:new([logplex_stats, logplex_realtime]),
    meck:expect(logplex_stats, incr, fun(Key, Value) ->
                                             ets:update_counter(Table, Key, Value, {Key, 0})
                                     end),
    meck:expect(logplex_stats, incr, fun(Key) ->
                                             ets:update_counter(Table, Key, 1, {Key, 0})
                                     end),
    meck:expect(logplex_realtime, incr, fun(Key, Value) ->
                                                ets:update_counter(Table, Key, Value, {Key, 0})
                                        end),
    meck:expect(logplex_realtime, incr, fun(Key) ->
                                                ets:update_counter(Table, Key, 1, {Key, 0})
                                        end),


    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    [Cleanup() || Cleanup <- ?config(cleanup_funs, Config)],
    Config.


%% ----------------------------------------------------------------------------
%% tests
%% ----------------------------------------------------------------------------

channel_not_found(Config) ->
    Table = ?config(table, Config),
    NumMsgs = 5,
    Msgs = make_msgs(NumMsgs),
    ChannelId = new_channel_id(), %% create a new unknown channel id
    Token = new_token(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, Token, TokenName)),
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(Table, 'message.received')),
    ?assertMatch([{unknown_channel, NumMsgs}], ets:lookup(Table, unknown_channel)),
    ExpectedLogLine = io_lib:format("at=process_msgs? channel_id=~s msg=unknown_channel", [ChannelId]),
    ?assert(is_contained_in_logs(ExpectedLogLine, fetch_logs(Table))).

process_messages(Config) ->
    Table = ?config(table, Config),
    Bag = ?config(bag, Config),
    NumMsgs = 5,
    Msgs = make_msgs(NumMsgs),
    ChannelId = ?config(channel_id, Config),
    Token = new_token(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, Token, TokenName)),
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(Table, 'message.received')),
    ?assertMatch(asdf, ets:lookup(Bag, queued_redis_command)),
    ok.

process_messages_no_redis(Config) ->
    Table = ?config(table, Config),
    Bag = ?config(bag, Config),
    NumMsgs = 5,
    Msgs = make_msgs(NumMsgs),
    ChannelId = ?config(channel_id, Config),
    Token = new_token(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, Token, TokenName)),
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(Table, 'message.received')),
    ?assertMatch([], ets:lookup(Bag, queued_redis_command)),
    ok.

%% ----------------------------------------------------------------------------
%% helper functions
%% ----------------------------------------------------------------------------

prepare_test(Testcase, ChannelFlags, Config0) ->
    %% Ensure we can store and read channels locally.
    ChannelsTable = logplex_channel:create_ets_table(),
    CleanupChannels = fun() -> ets:delete(ChannelsTable) end,
    CleanupFuns = [CleanupChannels | ?config(cleanup_funs, Config0)],

    ChannelId = new_channel_id(),
    Channel = logplex_channel:new(ChannelId, ChannelId, ChannelFlags),
    %% here we circumvent config redis, just keep channel in ets
    logplex_channel:cache(Channel),

    Config = make_storage(Testcase, Config0),

    [{channel_id, ChannelId},
     {cleanup_funs, CleanupFuns}
     | Config].

make_storage(Testcase, Config) ->
    %% the table is used for counters and single objects
    Table = ets:new(Testcase, [public]),
    %% the bag is used for similar objects under the same key, like logs or msgs
    Bag = ets:new(Testcase, [public, duplicate_bag]),
    CleanupTable = fun() -> ets:delete(Table) end,
    CleanupBag = fun() -> ets:delete(Bag) end,
    CleanupFuns = [CleanupTable, CleanupBag | ?config(cleanup_funs, Config)],
    [{table, Table},
     {bag, Bag},
     {cleanup_funs, CleanupFuns}
     | Config].

make_msgs(NumMsgs) ->
    Msgs = [{N, uuid:to_string(uuid:v4())} || N <- lists:seq(1, NumMsgs)],
    [{msg, new_msg(N, Msg)} || {N, Msg} <- Msgs].

new_channel_id() ->
    list_to_binary(["app-", uuid:to_string(uuid:v4())]).

new_msg(N, Msg) when is_integer(N) ->
    list_to_binary(["<", integer_to_list(N), ">1 ", "aaaa bbbb cccc dddd eeee - ", Msg]).

new_token() ->
    list_to_binary(["t.",  uuid:to_string(uuid:v4())]).

new_token_name() ->
    list_to_binary(["token-",  uuid:to_string(uuid:v4())]).

fetch_logs(Table) ->
    [Msg || {logs, Msg} <- ets:lookup(Table, logs)].

is_contained_in_logs(Msg, Logs) ->
    lists:any(fun(M) -> M == match end,
              [re:run(Log, Msg, [{capture, none}]) || Log <- Logs]).

mock_firehose(Config) ->
    Bag = ?config(bag, Config),
    meck:new(logplex_firehose),
    meck:expect(logplex_firehose, post_msg, fun(ChannelId, TokenName, RawMsg) ->
                                                    ets:insert(Bag, {firehose, ChannelId, TokenName, RawMsg})
                                            end),
    Config.

mock_drains(Config) ->
    Bag = ?config(bag, Config),
    meck:new(logplex_channel, [passthrough]),
    meck:expect(logplex_channel, post_msg, fun({channel, ChannelId}, Msg) ->
                                                   ets:insert(Bag, {drain, ChannelId, Msg})
                                           end),
    Config.

mock_tails(Config) ->
    Bag = ?config(bag, Config),
    meck:new(logplex_tail),
    meck:expect(logplex_tail, route, fun(ChannelId, Msg) ->
                                             ets:insert(Bag, {tail, ChannelId, Msg})
                                     end),
    Config.

mock_redis(Config) ->
    Bag = ?config(bag, Config),
    meck:new([logplex_shard_info, logplex_shard, logplex_queue]),
    meck:expect(logplex_shard_info, read, fun(_) -> fake_info end),
    meck:expect(logplex_shard_info, map_interval, fun(_) -> {fake_map, fake_interval} end),
    meck:expect(logplex_shard, lookup, fun(_, _, _) -> fake_buffer_pid end),
    meck:expect(logplex_queue, in, fun(_BufferPid, Cmd) ->
                                           % TODO: maybe fake a redis write, without shards?
                                           ets:insert(Bag, {queued_redis_command, Cmd})
                                   end),
    Config.
