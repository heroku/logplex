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
                       process_messages_no_redis,
                       process_messages_deny_tails,
                       process_messages_deny_drains
                     ].

groups() -> [ {classic, [], tests_per_group()},
              {batch_redis, [], tests_per_group()}
            ].

init_per_suite(Config) ->
    application:load(logplex), %% ensure default config is loaded

    %% We want to test writing to an actual redis instance. We should have at
    %% least one redis shard locally from docker.
    ShardUrls = string:tokens(os:getenv("LOGPLEX_SHARD_URLS"), ","),
    application:set_env(logplex, logplex_shard_urls, logplex_shard:redis_sort(ShardUrls)),

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
    Config1 = make_storage(channel_not_found, Config0),
    Config = mock_logging(Config1),

    meck:new(logplex_shard_info),
    meck:expect(logplex_shard_info, read, fun(_) -> fake_info end),

    meck:new(logplex_channel),
    meck:expect(logplex_channel, lookup_flag, fun(_, _) -> not_found end),

    init_per_testcase(any, Config);

init_per_testcase(process_messages = Testcase, ConfigBase) ->
    ChannelFlags = [],
    Config = mock_message_processing(Testcase, ChannelFlags, ConfigBase),
    init_per_testcase(any, Config);

init_per_testcase(process_messages_no_redis = Testcase, ConfigBase) ->
    ChannelFlags = [no_redis],
    Config = mock_message_processing(Testcase, ChannelFlags, ConfigBase),
    init_per_testcase(any, Config);

init_per_testcase(process_messages_deny_tails = Testcase, ConfigBase) ->
    application:set_env(logplex, deny_tail_sessions, true),
    ChannelFlags = [],
    Config = mock_message_processing(Testcase, ChannelFlags, ConfigBase),
    init_per_testcase(any, Config);

init_per_testcase(process_messages_deny_drains = Testcase, ConfigBase) ->
    application:set_env(logplex, deny_drain_forwarding, true),
    ChannelFlags = [],
    Config = mock_message_processing(Testcase, ChannelFlags, ConfigBase),
    init_per_testcase(any, Config);

init_per_testcase(_, Config) ->
    Table = ?config(table, Config),

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
    ?assert(is_contained_in_logs(ExpectedLogLine, fetch_logs(Config))).

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
    ?assertMatch(asdf, logplex_channel:logs(ChannelId, 1500)),
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
    ?assertMatch([], logplex_channel:logs(ChannelId, 1500)),
    ok.

process_messages_deny_tails(Config) ->
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
    ?assertMatch([], ets:lookup(Bag, tail)),
    ok.

process_messages_deny_drains(Config) ->
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
    ?assertMatch([], ets:lookup(Bag, drain)).

%% ----------------------------------------------------------------------------
%% helper functions
%% ----------------------------------------------------------------------------

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

fetch_logs(Config) ->
    Bag = ?config(bag, Config),
    [Msg || {logs, Msg} <- ets:lookup(Bag, logs)].

is_contained_in_logs(Msg, Logs) ->
    lists:any(fun(M) -> M == match end,
              [re:run(Log, Msg, [{capture, none}]) || Log <- Logs]).


mock_message_processing(Testcase, ChannelFlags, ConfigBase) ->
    Config = prepare_test(Testcase, ChannelFlags, ConfigBase),
    lists:foldr(fun(F, Acc) -> F(Acc) end,
                Config,
                [fun mock_firehose/1,
                 fun mock_drains/1,
                 fun mock_tails/1,
                 fun setup_redis/1
                ]).

prepare_test(Testcase, ChannelFlags, Config0) ->
    %% Ensure we can store and read channels locally.
    ChannelsTable = logplex_channel:create_ets_table(),
    CleanupChannels = fun() -> ets:delete(ChannelsTable) end,
    CleanupFuns = [CleanupChannels | ?config(cleanup_funs, Config0)],

    ChannelId = new_channel_id(),
    Channel = logplex_channel:new(ChannelId, ChannelId, ChannelFlags),
    %% here we circumvent config redis, just keep channel in ets
    logplex_channel:cache(Channel),

    Config1 = make_storage(Testcase, Config0),
    Config = mock_logging(Config1),

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

mock_logging(Config) ->
    %% We don't want actually test logging, only that we log the expected lines.
    %% We write logs into an ets table. We use a duplicate bag such that all log
    %% lines can be stored under the same key.
    Bag = ?config(bag, Config),
    meck:new(syslog_lib),
    meck:expect(syslog_lib, notice, fun(_, Line) ->
                                            ets:insert(Bag, {logs, Line})
                                    end),
    Config.

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

setup_redis(Config) ->
    %% start the logplex_redis_writer_sup supervisor
    {ok, RedisWriterPid} = logplex_worker_sup:start_link(logplex_redis_writer_sup, logplex_redis_writer),
    CleanupRedisWriter = fun() ->
                                 unlink(RedisWriterPid),
                                 exit(RedisWriterPid, kill)
                         end,

    {ok, ShardPid} = logplex_shard:start_link(),
    timer:sleep(200), %% give this thing some time to do whatever
    CleanupShard = fun() ->
                           unlink(ShardPid),
                           exit(ShardPid, kill)
                   end,

    CleanupFuns = ?config(cleanup_funs, Config),

    [{cleanup_funs, [CleanupRedisWriter, CleanupShard | CleanupFuns]}
     | Config].
