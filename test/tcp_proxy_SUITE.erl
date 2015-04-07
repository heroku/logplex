-module(tcp_proxy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("logplex.hrl").
-compile(export_all).

all() -> [accepts_tcp_syslog_data].

init_per_suite(Config) ->
    set_os_vars(),
    logplex_app:a_start(logplex, temporary),
    Config.

end_per_suite(_Config) ->
    application:stop(logplex).

init_per_testcase(accepts_tcp_syslog_data, Config) ->
    Port = logplex_app:config(syslog_port),
    {ok, Sock} = gen_tcp:connect("localhost", Port, 
                                 [binary, {packet, 0}]),
    [{socket, Sock} | Config];
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    ok = gen_tcp:close(?config(socket, Config)),
    Config.

accepts_tcp_syslog_data(Config) ->
    Sock = ?config(socket, Config),
    Token = #token{id = <<"t.d6799f88-4a77-402f-b197-2b722a02cdbc">>,
                   channel_id = 12345,
                   name = <<"test">>},
    meck:expect(logplex_token, lookup, fun(Id) when Id =:= Token#token.id -> Token end),
    meck:expect(logplex_channel, lookup_flag, [no_redis, Token#token.channel_id], meck:val(no_such_flag)),
    meck:expect(logplex_channel, post_msg, fun(_ChannelId, _Msg) -> ok end),

    Logs = [[<<"<134>1 2012-12-10T04:05:25Z+00:00 erlang ">>, Token#token.id, <<" console.1 - Logsplat test message 1 from <0.72.0>.">>],
            [<<"<134>1 2012-12-10T04:05:25Z+00:00 erlang ">>, Token#token.id, <<" console.1 - Logsplat test message 2 from <0.72.0>.">>],
            [<<"<134>1 2012-12-10T04:05:25Z+00:00 erlang ">>, Token#token.id, <<" console.1 - Logsplat test message 3 from <0.72.0>.">>],
            [<<"<134>1 2012-12-10T04:05:25Z+00:00 erlang ">>, Token#token.id, <<" console.1 - Logsplat test message 4 from <0.72.0>.">>],
            [<<"<134>1 2012-12-10T04:05:25Z+00:00 erlang ">>, Token#token.id, <<" console.1 - Logsplat test message 5 from <0.72.0>.">>]],

    Packet = frame_packet(Logs),
    ok = gen_tcp:send(Sock, Packet),

    meck:wait(5, logplex_channel, post_msg, [{channel, Token#token.channel_id}, '_'], 5000),
    meck:unload().

frame_packet(Logs) when is_list(Logs) ->
    iolist_to_binary([[integer_to_list(byte_size(iolist_to_binary(Log))), <<" ">>, Log] || Log <- Logs]).

set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"}
        ]].
