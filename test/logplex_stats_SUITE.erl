-module(logplex_stats_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("logplex/include/logplex.hrl").
-compile(export_all).

all() -> [channel_flood].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    Config.

end_per_suite(_Config) ->
    application:stop(logplex),
    meck:unload().

init_per_testcase(_, Config) ->
    application:set_env(logplex, channel_flood_msg_threshold, 2),
    application:set_env(logplex, channel_flood_flap_threshold, 2),
    ChannelId = 1337,
    [{channel, ChannelId} | Config].

end_per_testcase(_, _Config) ->
    ok.

%%% Setup & teardown helpers %%%
set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"}
        ]],
    logplex_app:cache_os_envvars().

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
channel_flood(Config) ->
    meck:new(logplex_channel, [passthrough, no_link]),
    meck:expect(logplex_channel, set_local_flag, fun(_, _) -> ok end),
    meck:expect(logplex_channel, unset_local_flag, fun(_, _) -> ok end),
    ChannelId = ?config(channel, Config),
    flush_logs(),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    flush_logs(),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    flush_logs(),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1), % triggers flood
    flush_logs(),
    1 = meck:num_calls(logplex_channel, set_local_flag, [ChannelId, no_redis_local]),
    meck:reset(logplex_channel),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    flush_logs(),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    flush_logs(),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId}, 1),
    flush_logs(),
    1 = meck:num_calls(logplex_channel, unset_local_flag, [ChannelId, no_redis_local]).

fake_msg(M) ->
    {user, debug, logplex_syslog_utils:datetime(now), "fakehost", "erlang", M}.

flush_logs() ->
    logplex_stats ! {timeout, make_ref(), flush},
    timer:sleep(100).

