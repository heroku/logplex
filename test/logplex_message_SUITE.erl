-module(logplex_message_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/logplex_logging.hrl").
-compile(export_all).

all() -> [ channel_not_found
         ].

init_per_suite(Config) ->
    application:load(logplex), %% ensure default config is loaded
    %% mock shard stuff
    % meck:new([logplex_shard_info, logplex_shard]),
    % meck:expect(logplex_shard_info, read, fun(_) -> fake_info end),
    % meck:expect(logplex_shard_info, map_interval, fun(_) -> fake_interval end),
    % meck:expect(logplex_shard, lookup, fun(_, _) -> fake_pid end),

    Config.

end_per_suite(Config) ->
    meck:unload(),
    Config.

init_per_testcase(channel_not_found, Config) ->
    meck:new(logplex_channel),
    meck:expect(logplex_channel, lookup_flag, fun(_, _) -> not_found end),
    init_per_testcase(any, Config);

init_per_testcase(_, Config) ->
    %% mock logging
    %% We don't want actually test logging, only that we log the expected lines.
    %% We write logs into an ets table. We use a duplicate bag such that all log
    %% lines can be stored under the same key.
    ets:new(logging, [named_table, public, duplicate_bag]),
    meck:new(syslog_lib),
    meck:expect(syslog_lib, notice, fun(_, Line) ->
                                            ets:insert(logging, {logs, Line})
                                    end),

    %% mock metrics
    ets:new(metrics, [named_table, public]),
    meck:new([logplex_stats, logplex_realtime]),
    meck:expect(logplex_stats, incr, fun(Key, Value) ->
                                             ets:update_counter(metrics, Key, Value, {Key, 0})
                                     end),
    meck:expect(logplex_stats, incr, fun(Key) ->
                                             ets:update_counter(metrics, Key, 1, {Key, 0})
                                     end),
    meck:expect(logplex_realtime, incr, fun(Key, Value) ->
                                                ets:update_counter(metrics, Key, Value, {Key, 0})
                                        end),
    meck:expect(logplex_realtime, incr, fun(Key) ->
                                                ets:update_counter(metrics, Key, 1, {Key, 0})
                                        end),

    meck:new(logplex_shard_info),
    meck:expect(logplex_shard_info, read, fun(_) -> fake_info end),

    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    ets:delete(logging),
    ets:delete(metrics),
    Config.


%% ----------------------------------------------------------------------------
%% tests
%% ----------------------------------------------------------------------------

channel_not_found(_Config) ->
    NumMsgs = 5,
    ExpectedMsgs = [{N, uuid:to_string(uuid:v4())} || N <- lists:seq(1, NumMsgs)],
    Msgs = [{msg, new_msg(N, Msg)} || {N, Msg} <- ExpectedMsgs],
    ChannelId = new_channel_id(),
    Token = new_token(),
    TokenName = new_token_name(),
    ?assertMatch(ok, logplex_message:process_msgs(Msgs, ChannelId, Token, TokenName)),
    ?assertMatch([{message_received, NumMsgs}], ets:lookup(metrics, message_received)),
    ?assertMatch([{'message.received', NumMsgs}], ets:lookup(metrics, 'message.received')),
    ?assertMatch([{unknown_channel, NumMsgs}], ets:lookup(metrics, unknown_channel)),
    ExpectedLogLine = io_lib:format("at=process_msgs? channel_id=~s msg=unknown_channel", [ChannelId]),
    ?assert(is_contained_in_logs(ExpectedLogLine, fetch_logs())).


%% ----------------------------------------------------------------------------
%% helper functions
%% ----------------------------------------------------------------------------

new_channel_id() ->
    list_to_binary(["app-", uuid:to_string(uuid:v4())]).

new_msg(N, Msg) when is_integer(N) ->
    list_to_binary(["<", integer_to_list(N), ">1 ", "aaaa bbbb cccc dddd eeee - ", Msg]).

new_token() ->
    list_to_binary(["t.",  uuid:to_string(uuid:v4())]).

new_token_name() ->
    list_to_binary(["token-",  uuid:to_string(uuid:v4())]).

fetch_logs() ->
    [Msg || {logs, Msg} <- ets:lookup(logging, logs)].

is_contained_in_logs(Msg, Logs) ->
    lists:any(fun(M) -> M == match end,
              [re:run(Log, Msg, [{capture, none}]) || Log <- Logs]).
