-module(tcp_proxy_SUITE).
-include_lib("common_test/include/ct.hrl").
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
    Logs = [<<"<134>1 2012-12-10T04:05:25Z+00:00 erlang t.d6799f88-4a77-402f-b197-2b722a02cdbc console.1 - Logsplat test message 1 from <0.72.0>.">>,
            <<"<134>1 2012-12-10T04:05:25Z+00:00 erlang t.d6799f88-4a77-402f-b197-2b722a02cdbc console.1 - Logsplat test message 2 from <0.72.0>.">>,
            <<"<134>1 2012-12-10T04:05:25Z+00:00 erlang t.d6799f88-4a77-402f-b197-2b722a02cdbc console.1 - Logsplat test message 3 from <0.72.0>.">>,
            <<"<134>1 2012-12-10T04:05:25Z+00:00 erlang t.d6799f88-4a77-402f-b197-2b722a02cdbc console.1 - Logsplat test message 4 from <0.72.0>.">>,
            <<"<134>1 2012-12-10T04:05:25Z+00:00 erlang t.d6799f88-4a77-402f-b197-2b722a02cdbc console.1 - Logsplat test message 5 from <0.72.0>.">>],
    meck:expect(logplex_message, process_msgs, fun(_Logs) -> ok end),

    ok = gen_tcp:send(Sock, frame_packet(Logs)),
    meck:wait(logplex_message, process_msgs, '_', 5000),

    Expected = [ {msg, Log} || Log <- Logs ],
    true = meck:called(logplex_message, process_msgs, [Expected]),
    meck:unload(logplex_message).

frame_packet(Logs) when is_list(Logs) ->
    iolist_to_binary([[integer_to_list(byte_size(Log)), <<" ">>, Log] || Log <- Logs]).

set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"}
        ]].
