-module(logplex_message_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/logplex_logging.hrl").

-compile(export_all).

all() -> [ {group, classic},
           {group, batch_redis}
         ].

tests_per_group() -> [
                       %% tests for process_msgs/4
                       channel_not_found,
                       process_messages,
                       process_messages_no_redis,
                       process_messages_deny_tails,
                       process_messages_deny_drains,
                       process_messages_log_history_limit,
                       %% tests for process_msgs/1 (aka. any creds)
                       process_messages_any_creds_unknown_token,
                       process_messages_any_creds_malformed,
                       process_messages_any_creds_no_token,
                       process_messages_any_creds
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
    Cleanup = fun() ->
                      application:set_env(logplex, batch_redis, false)
              end,
    [{cleanup_funs, [Cleanup | ?config(cleanup_funs, Config)]}
     | Config];
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
    Config = fake_message_processing(Testcase, ChannelFlags, ConfigBase),
    init_per_testcase(any, Config);

init_per_testcase(process_messages_no_redis = Testcase, ConfigBase) ->
    ChannelFlags = [no_redis],
    Config = fake_message_processing(Testcase, ChannelFlags, ConfigBase),
    init_per_testcase(any, Config);

init_per_testcase(process_messages_deny_tails = Testcase, ConfigBase) ->
    application:set_env(logplex, deny_tail_sessions, true),
    ChannelFlags = [],
    Config0 = fake_message_processing(Testcase, ChannelFlags, ConfigBase),
    Cleanup = fun() ->
                      application:set_env(logplex, deny_tail_sessions, false)
              end,
    CleanupFuns = ?config(cleanup_funs, ConfigBase),
    Config = [{cleanup_funs, [Cleanup | CleanupFuns]} | Config0],
    init_per_testcase(any, Config);

init_per_testcase(process_messages_deny_drains = Testcase, ConfigBase) ->
    application:set_env(logplex, deny_drain_forwarding, true),
    ChannelFlags = [],
    Config0 = fake_message_processing(Testcase, ChannelFlags, ConfigBase),
    Cleanup = fun() ->
                      application:set_env(logplex, deny_drain_forwarding, false)
              end,
    CleanupFuns = ?config(cleanup_funs, ConfigBase),
    Config = [{cleanup_funs, [Cleanup | CleanupFuns]} | Config0],
    init_per_testcase(any, Config);

init_per_testcase(process_messages_log_history_limit = Testcase, ConfigBase) ->
    LogHistoryOriginal = logplex_app:config(log_history),
    application:set_env(logplex, log_history, 4), %% 0-indexed, expecting 5 elemets
    ChannelFlags = [],
    Config0 = fake_message_processing(Testcase, ChannelFlags, ConfigBase),
    Cleanup = fun() ->
                      application:set_env(logplex, log_history, LogHistoryOriginal)
              end,
    CleanupFuns = ?config(cleanup_funs, ConfigBase),
    Config = [{cleanup_funs, [Cleanup | CleanupFuns]} | Config0],
    init_per_testcase(any, Config);

init_per_testcase(process_messages_any_creds_unknown_token = Testcase, ConfigBase) ->
    Config1 = make_storage(Testcase, ConfigBase),
    Config2 = mock_logging(Config1),

    meck:new(logplex_shard_info),
    meck:expect(logplex_shard_info, read, fun(_) -> fake_info end),

    TokenId = new_token_id(),
    meck:new(logplex_token, [passthrough]),
    meck:expect(logplex_token, lookup, fun(_) -> undefined end),

    Config = [{token_id, TokenId} | Config2],
    init_per_testcase(any, Config);

init_per_testcase(Testcase, ConfigBase)
  when Testcase == process_messages_any_creds_malformed;
       Testcase == process_messages_any_creds_no_token ->
    Config = make_storage(Testcase, ConfigBase),

    meck:new(logplex_shard_info),
    meck:expect(logplex_shard_info, read, fun(_) -> fake_info end),

    init_per_testcase(any, Config);

init_per_testcase(process_messages_any_creds = Testcase, ConfigBase) ->
    ChannelFlags = [],
    Config0 = fake_message_processing(Testcase, ChannelFlags, ConfigBase),

    %% add a token for channel
    ChannelId = ?config(channel_id, Config0),
    TokenId = new_token_id(),
    TokenName = new_token_name(),
    Token = logplex_token:new(TokenId, ChannelId, TokenName),

    %% add another channel plus token
    ChannelId2 = new_channel_id(),
    Channel2 = logplex_channel:new(ChannelId2, ChannelId2, ChannelFlags),
    logplex_channel:cache(Channel2),
    TokenId2 = new_token_id(),
    TokenName2 = new_token_name(),
    Token2 = logplex_token:new(TokenId2, ChannelId2, TokenName2),

    meck:new(logplex_token, [passthrough]),
    meck:expect(logplex_token, lookup, fun(T) when T == TokenId -> Token;
                                          (T) when T == TokenId2 -> Token2;
                                          (_) -> undefined
                                       end),

    Config = [{token_id, TokenId},
              {token_name, TokenName},
              {channel_id_2, ChannelId2},
              {token_id_2, TokenId2},
              {token_name_2, TokenName2}
              | Config0],
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
    TokenId = new_token_id(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, TokenId, TokenName)),
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(Table, 'message.received')),
    ?assertMatch([{unknown_channel, NumMsgs}], ets:lookup(Table, unknown_channel)),
    ExpectedLogLine = io_lib:format("at=process_msgs? channel_id=~s msg=unknown_channel", [ChannelId]),
    ?assert(is_contained_in_logs(ExpectedLogLine, fetch_logs(Config))).

process_messages(Config) ->
    Table = ?config(table, Config),
    Bag = ?config(bag, Config),
    NumMsgs = 5,
    ChannelId = ?config(channel_id, Config),
    TokenId = new_token_id(),
    TokenName = new_token_name(),
    {Msgs, WantMsgs} = make_msgs(NumMsgs, TokenId, TokenName),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, TokenId, TokenName)),
    timer:sleep(100), %% give some time figure things out
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(Table, 'message.received')),
    %% We sort the channel logs here to simplify validation, this isn't quite accurate as the redis writer
    %% could write logs in random order. We only want to know the messages are there and their count is correct.
    validate_msgs(redis, WantMsgs, lists:sort(logplex_channel:logs(ChannelId, logplex_app:config(log_history)))),
    WantFirehoseMsgs = [{firehose, ChannelId, TokenName, RawMsg} || {msg, RawMsg} <- Msgs],
    validate_msgs(firehose, WantFirehoseMsgs, ets:lookup(Bag, firehose)),
    WantDrainMsgs = [{drain, ChannelId, Msg} || Msg <- WantMsgs],
    validate_msgs(drain, WantDrainMsgs, ets:lookup(Bag, drain)),
    WantTailMsgs = [{tail, ChannelId, Msg} || Msg <- WantMsgs],
    validate_msgs(tail, WantTailMsgs, ets:lookup(Bag, tail)).

process_messages_no_redis(Config) ->
    Table = ?config(table, Config),
    NumMsgs = 5,
    Msgs = make_msgs(NumMsgs),
    ChannelId = ?config(channel_id, Config),
    TokenId = new_token_id(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, TokenId, TokenName)),
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(Table, 'message.received')),
    ?assertMatch([], logplex_channel:logs(ChannelId, logplex_app:config(log_history))),
    ok.

process_messages_deny_tails(Config) ->
    Table = ?config(table, Config),
    Bag = ?config(bag, Config),
    NumMsgs = 5,
    Msgs = make_msgs(NumMsgs),
    ChannelId = ?config(channel_id, Config),
    TokenId = new_token_id(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, TokenId, TokenName)),
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
    TokenId = new_token_id(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, TokenId, TokenName)),
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(Table, 'message.received')),
    ?assertMatch([], ets:lookup(Bag, drain)).

process_messages_log_history_limit(Config) ->
    NumMsgs = 5,
    ChannelId = ?config(channel_id, Config),
    TokenId = new_token_id(),
    TokenName = new_token_name(),
    {Msgs, WantMsgs} = make_msgs(NumMsgs, TokenId, TokenName),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, TokenId, TokenName)),
    timer:sleep(100),
    validate_msgs(redis, WantMsgs, lists:sort(logplex_channel:logs(ChannelId, logplex_app:config(log_history)))),
    Msg = new_msg(6, TokenId, uuid:to_string(uuid:v4())),
    WantMsgs1 = tl(WantMsgs) ++ [cook_msg(Msg, TokenId, TokenName)], %% drop oldest message and append newest
    ?assertMatch(ok, logplex_message:process_msgs([{msg, Msg}], ChannelId, TokenId, TokenName)),
    timer:sleep(100),
    validate_msgs(redis, WantMsgs1, lists:sort(logplex_channel:logs(ChannelId, logplex_app:config(log_history)))).

process_messages_any_creds_unknown_token(Config) ->
    Table = ?config(table, Config),
    NumMsgs = 5,
    TokenId = ?config(token_id, Config),
    Msgs = make_msgs(TokenId, NumMsgs),
    logplex_message:process_msgs(Msgs),
    ?assertMatch([{unknown_token, NumMsgs}], ets:lookup(Table, unknown_token)),
    ExpectedLogLine = io_lib:format("at=process_msgs? token_id=<<\"~s\">> msg=unknown_token", [TokenId]),
    ct:pal("want: ~p~nin: ~p", [ExpectedLogLine, fetch_logs(Config)]),
    ?assert(is_contained_in_logs(ExpectedLogLine, fetch_logs(Config))).

process_messages_any_creds_malformed(Config) ->
    Table = ?config(table, Config),
    NumMsgs = 5,
    MalformedMsgs = [{malformed, uuid:to_string(uuid:v4())} || _ <- lists:seq(1, NumMsgs)],
    logplex_message:process_msgs(MalformedMsgs),
    ?assertMatch([{message_received_malformed, NumMsgs}], ets:lookup(Table, message_received_malformed)),
    ?assertMatch([{'message.received-malformed', NumMsgs}], ets:lookup(Table, 'message.received-malformed')).

process_messages_any_creds_no_token(Config) ->
    Table = ?config(table, Config),
    NumMsgs = 5,
    Msgs = [{msg, uuid:to_string(uuid:v4())} || _ <- lists:seq(1, NumMsgs)],
    logplex_message:process_msgs(Msgs),
    ?assertMatch([{message_received_malformed, NumMsgs}], ets:lookup(Table, message_received_malformed)),
    ?assertMatch([{'message.received-malformed', NumMsgs}], ets:lookup(Table, 'message.received-malformed')).

process_messages_any_creds(Config) ->
    Table = ?config(table, Config),
    Bag = ?config(bag, Config),

    NumMsgs = 5,
    ChannelId = ?config(channel_id, Config),
    TokenId = ?config(token_id, Config),
    TokenName = ?config(token_name, Config),
    ChannelId2 = ?config(channel_id_2, Config),
    TokenId2 = ?config(token_id_2, Config),
    TokenName2 = ?config(token_name_2, Config),
    {Msgs, WantMsgs} = make_msgs(NumMsgs, TokenId, TokenName),
    {Msgs2, WantMsgs2} = make_msgs(NumMsgs, TokenId2, TokenName2),

    logplex_message:process_msgs(Msgs ++ Msgs2),
    timer:sleep(100), %% give some time figure things out

    WantNumMsgs = 2 * NumMsgs,
    ?assertMatch([{message_received, WantNumMsgs}], ets:lookup(Table, message_received)),
    ?assertMatch([{'message.received', WantNumMsgs}], ets:lookup(Table, 'message.received')),

    validate_msgs(redis, WantMsgs, lists:sort(logplex_channel:logs(ChannelId, logplex_app:config(log_history)))),
    validate_msgs(redis, WantMsgs2, lists:sort(logplex_channel:logs(ChannelId2, logplex_app:config(log_history)))),

    WantFirehoseMsgs = [{firehose, ChannelId, TokenName, RawMsg} || {msg, RawMsg} <- Msgs] ++
                       [{firehose, ChannelId2, TokenName2, RawMsg} || {msg, RawMsg} <- Msgs2],
    validate_msgs(firehose, lists:sort(WantFirehoseMsgs), lists:sort(ets:lookup(Bag, firehose))),

    WantDrainMsgs = [{drain, ChannelId, Msg} || Msg <- WantMsgs ] ++
                    [{drain, ChannelId2, Msg} || Msg <- WantMsgs2 ],
    validate_msgs(drain, lists:sort(WantDrainMsgs), lists:sort(ets:lookup(Bag, drain))),

    WantTailMsgs = [{tail, ChannelId, Msg} || Msg <- WantMsgs] ++
                   [{tail, ChannelId2, Msg} || Msg <- WantMsgs2],
    validate_msgs(tail, lists:sort(WantTailMsgs), lists:sort(ets:lookup(Bag, tail))).

%% ----------------------------------------------------------------------------
%% helper functions
%% ----------------------------------------------------------------------------

new_channel_id() ->
    list_to_binary(["app-", uuid:to_string(uuid:v4())]).

new_token_id() ->
    logplex_token:new_token_id().

new_token_name() ->
    list_to_binary(["token-",  uuid:to_string(uuid:v4())]).

make_msgs(NumMsgs, TokenId, TokenName) ->
    Msgs = make_msgs(TokenId, NumMsgs),
    WantMsgs = [cook_msg(RawMsg, TokenId, TokenName) || {msg, RawMsg} <- Msgs],
    {Msgs, WantMsgs}.

make_msgs(TokenId, NumMsgs) ->
    Msgs = [{N, uuid:to_string(uuid:v4())} || N <- lists:seq(1, NumMsgs)],
    [{msg, new_msg(N, TokenId, Msg)} || {N, Msg} <- Msgs].

make_msgs(NumMsgs) ->
    make_msgs(<<"TOKEN_TOKEN">>, NumMsgs).

new_msg(N, TokenId, Msg) when is_integer(N) ->
    list_to_binary(["<", integer_to_list(N), ">1 timestamp host ", TokenId, " aaaa bbbb cccc dddd - ", Msg]).

cook_msg(RawMsg, TokenId, TokenName) ->
    iolist_to_binary(re:replace(RawMsg, TokenId, TokenName)).

fake_message_processing(Testcase, ChannelFlags, ConfigBase) ->
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

fetch_logs(Config) ->
    Bag = ?config(bag, Config),
    [Msg || {logs, Msg} <- ets:lookup(Bag, logs)].

is_contained_in_logs(Msg, Logs) ->
    lists:any(fun(M) -> M == match end,
              [re:run(Log, Msg, [{capture, none}]) || Log <- Logs]).

validate_msgs(_Context, [], []) ->
    ok;
validate_msgs(Context, WantMsgs, GotMsgs) when length(WantMsgs) =/= length(GotMsgs) ->
    ct:pal("context: ~p~nwant: ~p~ngot: ~p~n",[ Context, WantMsgs, GotMsgs]),
    ct:fail({Context, msg_count_mismatch});
validate_msgs(Context, [WantMsg | WantMsgs], [GotMsg | GotMsgs]) ->
    ct:pal("context: ~p~nwant: ~p~ngot: ~p~nequals: ~p", [ Context, WantMsg, GotMsg, WantMsg == GotMsg]),
    ?assertEqual(WantMsg, GotMsg),
    validate_msgs(Context, WantMsgs, GotMsgs).

