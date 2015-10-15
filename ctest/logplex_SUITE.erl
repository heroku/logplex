%%% Tests the general behaviour of getting logplex to register channels,
%%% create tokens, log to them, and consume the logs within one VM.
-module(logplex_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [boot_and_stop, roundtrip, log_history].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
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
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"}
        ]].


boot_and_stop(_Config) ->
    ok.

roundtrip(Config) ->
    Chan = ?config(channel, Config),
    Token = ?config(token, Config),
    ok = send_msg(make_msg(Token,1)),
    timer:sleep(1000),
    [_] = read_logs(Chan).

%% after hitting the configured limit of messages in log history,
%% they start to rewrite themselves as in a ring buffer. The default size
%% is set to 1500, but redis is 0-indexed so the max value is actually 1501
log_history(Config) ->
    Chan = ?config(channel, Config),
    Token = ?config(token, Config),
    Max = 1501,
    [ok = send_msg(make_msg(Token,N)) ||
        N <- lists:seq(1, Max)],
    timer:sleep(1000),
    FirstBatch = read_logs(Chan),
    ok = send_msg(make_msg(Token, Max+1)),
    ok = send_msg(make_msg(Token, Max+2)),
    ok = send_msg(make_msg(Token, Max+3)),
    timer:sleep(1000),
    SecondBatch = read_logs(Chan),
    Max = length(FirstBatch),
    Max = length(SecondBatch),
    Max = 3+length([X || X <- SecondBatch, lists:member(X,FirstBatch)]).

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
    [Res] = logplex_message:process_msgs([{msg,iolist_to_binary(Msg)}]),
    Res.

read_logs(Chan) ->
    logplex_channel:logs(logplex_channel:id(Chan), 500000).

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
