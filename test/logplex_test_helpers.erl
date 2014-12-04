-module(logplex_test_helpers).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", net_adm:localhost()},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_NODE_NAME", atom_to_list(node())}
        ]].

make_msg(Token, N) ->
    logplex_syslog_utils:rfc5424(
        user,
        debug,
        logplex_syslog_utils:datetime(now),
        "fakehost",
        logplex_token:id(Token),
        "erlang",
        "web.1 -", % the - is a trick because we suck at rfc5424 generation
        integer_to_list(N)
    ).
