-module(logplex_shard_replace_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [replace_shard].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    Config.

init_per_testcase(Case, Config) ->
    Name = atom_to_binary(Case, latin1),
    Chan = logplex_channel:create(Name),
    ChanId = logplex_channel:id(Chan),
    TokenId = logplex_token:create(ChanId, <<"ct">>),
    logplex_SUITE:wait_for_chan(Chan),
    logplex_channel:register({channel, ChanId}),
    logplex_SUITE:wait_for_registration({channel, ChanId}),
    Token = logplex_SUITE:get_token(TokenId),
    [{channel, Chan}, {token,Token} | Config].

set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", net_adm:localhost()},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_NODE_NAME", atom_to_list(node())}
        ]].

replace_shard(Config) ->
    Chan = ?config(channel, Config),
    Token = ?config(token, Config),
    [ok = logplex_SUITE:send_msg(logplex_SUITE:make_msg(Token,N)) ||
        N <- lists:seq(1, 50)],
    50 = length(wait_for_messages(Chan, 50, timer:seconds(1), os:timestamp())),
    %% Set the new shard as our main shard - this is the same shard in this
    %% test but can be another server.
    true = os:putenv("LOGPLEX_SHARD_URLS", "redis://localhost:6379"),
    %% Replace the shard with itself
    os:cmd("../../bin/replace_shards -e"),
    "0\n" = os:cmd("echo $?"),
    % This will fetch them from the shard
    50 = length(wait_for_messages(Chan, 50, timer:seconds(1), os:timestamp())),
    Config.

wait_for_messages(Channel, MessageAmount, MaxWait, StartTime) ->
    case  timer:now_diff(os:timestamp(), StartTime) / 1000 > MaxWait of
        true ->
            {error, timeout};
        _ ->
            Msg = logplex_SUITE:read_logs(Channel),
            if
                length(Msg) >= MessageAmount ->
                    Msg;
                true ->
                    timer:sleep(10),
                    wait_for_messages(Channel, MessageAmount, MaxWait, StartTime)
            end
    end.
