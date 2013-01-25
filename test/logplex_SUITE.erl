%%% Tests the general behaviour of getting logplex to register channels,
%%% create tokens, log to them, and consume the logs within one VM.
-module(logplex_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [boot_and_stop, roundtrip].

init_per_suite(Config) ->
    ok = application:load(logplex),
    set_os_vars(),
    [ok = application:start(App, permanent) || App <-
        [sasl, crypto, public_key, ssl, inets,
         gproc, ehmon, ex_uri, redis, cowboy,
         logplex]],
    Config.

end_per_suite(_Config) ->
    application:stop(logplex).

init_per_testcase(Case, Config) ->
    Name = atom_to_binary(Case, latin1),
    Chan = logplex_channel:create(Name),
    ChanId = logplex_channel:id(Chan),
    TokenId = logplex_token:create(ChanId, <<"ct">>),
    wait_for_chan(Chan),
    logplex_channel:register({channel, ChanId}),
    wait_for_registration({channel, ChanId}),
    Token = get_token(TokenId),
    [{channel, Chan}, {token,Token} | Config].

end_per_testcase(_, Config) ->
    Chan = ?config(channel, Config),
    logplex_channel:destroy(Chan),
    ok.

set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_CONFIG_REDIS_URL", "redis://localhost:6379/"},
         {"LOGPLEX_SHARD_URLS", "redis://localhost:6379/"},
         {"LOGPLEX_STATS_REDIS_URL", "redis://localhost:6379/"},
         {"LOGPLEX_LOG_HISTORY", "10"}]].


boot_and_stop(_Config) ->
    ok.

roundtrip(Config) ->
    Chan = ?config(channel, Config),
    Token = ?config(token, Config),
    ok = send_msg(make_msg(Token,1)),
    timer:sleep(1000),
    [_] = read_logs(Chan).

make_msg(Token, N) ->
    logplex_syslog_utils:rfc5424(
        user,
        debug,
        logplex_syslog_utils:datetime(now),
        "fakehost",
        logplex_token:id(Token),
        "erlang",
        "web.1",
        integer_to_list(N)
     ).

send_msg(Msg) ->
    logplex_message:process_msgs([iolist_to_binary(Msg)]).

read_logs(Chan) ->
    logplex_channel:logs(logplex_channel:id(Chan), 10).

wait_for_chan(Chan) ->
    case ets:match_object(channels, Chan) of
        [] ->
            timer:sleep(50),
            wait_for_chan(Chan);
        _ ->
            ok
    end.

get_token(TokenId) ->
    case logplex_token:lookup(TokenId) of
        undefined ->
            timer:sleep(50),
            get_token(TokenId);
        Token ->
            Token
    end.

wait_for_registration(Name) ->
    case lists:member(self(), logplex_channel:whereis(Name)) of
        true -> ok;
        false -> wait_for_registration(Name)
    end.
