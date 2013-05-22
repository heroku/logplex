-module(logplex_tcp_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-compile(export_all).

-define(PORT, 9601).
-define(DRAIN_BUFFER_SIZE,5).

all() -> [full_stack, retry].

groups() -> [].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP / TEADOWN %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% Directly copy/pastedfrom logplex_tcp_syslog_drain
-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                host :: string() | inet:ip_address() | binary(),
                port :: inet:port_number(),
                sock = undefined :: 'undefined' | inet:socket(),
                %% Buffer for messages while disconnected
                buf :: pid(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% TCP failures since last_good_time
                failures = 0 :: non_neg_integer(),
                %% Reconnect timer reference
                reconnect_tref = undefined :: 'undefined' | reference(),
                %% Send timer reference
                send_tref = undefined :: 'undefined' | reference(),
                %% Time of last successful connection
                connect_time :: 'undefined' | erlang:timestamp(),
                %% Buffered items to send
                out_q = queue:new() :: queue(),
                drop_info :: {erlang:timestamp(), pos_integer()}
               }).

-record(frame, {frame :: iolist(),
                msg_count = 0 :: non_neg_integer(),
                loss_count = 0 :: non_neg_integer(),
                tries = 0 :: non_neg_integer()
               }).

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    Config.

end_per_suite(_Config) ->
    application:stop(logplex),
    meck:unload().

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(full_stack, Config) ->
    application:set_env(logplex, tcp_drain_buffer_size, 5),
    Port = start_server(Config),
    %% Drain data
    ChannelId = 1337,
    DrainId = 2198712,
    DrainTok = "d.12930-321-312213-12321",
    {ok,URI,_} = ex_uri:decode("syslog://127.0.0.1:"++integer_to_list(?PORT)),
    ok = meck:new(logplex_drain_buffer, [passthrough, no_link]),
    {ok, Pid} = logplex_tcpsyslog_drain2:start_link(ChannelId, DrainId, DrainTok, URI),
    unlink(Pid),
    [{channel,ChannelId},{listen,Port},{drain,Pid} | Config];
init_per_testcase(_, Config) ->
    application:set_env(logplex, tcp_drain_buffer_size, 5),
    Listen = start_server(Config),
    %% Drain data
    {ok,URI,_} = ex_uri:decode("syslog://127.0.0.1:"++integer_to_list(?PORT)),
    #ex_uri{scheme="syslog",
            authority=#ex_uri_authority{host=Host, port=Port}} = URI,
    [{channel,1337},
     {drain_id,2198712},
     {drain_tok,"d.12930-321-312213-12321"},
     {uri,URI},
     {listen, Listen},
     {host,Host},
     {port,Port} | Config].


end_per_testcase(_, Config) ->
    Drain = ?config(drain,Config),
    catch meck:unload(logplex_drain_buffer),
    erlang:monitor(process, Drain),
    catch Drain ! shutdown,
    receive
        {'DOWN', _, _, Drain, {shutdown,call}} -> ok;
        {'DOWN', _, _, Drain, Other} -> ct:pal("DRAIN DIED OF REASON: ~p",[Other])
    after 2000 ->
        error({not_dead, sys:get_status(Drain)})
    end,
    stop_server(Config).

%%% Setup & teardown helpers %%%
set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"}
        ]].

start_server(_Config) ->
    {ok, Listen} = gen_tcp:listen(?PORT, [binary, {active,false}, {reuseaddr,true}]),
    Listen.

stop_server(Config) ->
    Port = ?config(port, Config),
    catch gen_tcp:close(Port).




%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

%% We drop a frame, but the rest is successfully delivered. The message is
%% implicitly dropped due to the drain buffer limit being set to 5
full_stack(Config) ->
    Listen = ?config(listen, Config),
    ChannelId = ?config(channel, Config),
    logplex_channel:post_msg({channel, ChannelId}, msg("mymsg1")),
    logplex_channel:post_msg({channel, ChannelId}, msg("mymsg2")),
    logplex_channel:post_msg({channel, ChannelId}, msg("mymsg3")),
    logplex_channel:post_msg({channel, ChannelId}, msg("mymsg4")),
    logplex_channel:post_msg({channel, ChannelId}, msg("mymsg5")),
    logplex_channel:post_msg({channel, ChannelId}, msg("mymsg6")),
    logplex_channel:post_msg({channel, ChannelId}, msg("mymsg7")),
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 1, 5000),
    {ok, Sock} = gen_tcp:accept(Listen, 5000),
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 2, 5000),
    Logs = receive_logs(Sock, 6),
    nomatch = re:run(Logs, "mymsg1"),
    nomatch = re:run(Logs, "mymsg2"),
    {match, _} = re:run(Logs, "L10.*2 messages dropped"),
    {match, _} = re:run(Logs, "mymsg3"),
    {match, _} = re:run(Logs, "mymsg4"),
    {match, _} = re:run(Logs, "mymsg5"),
    {match, _} = re:run(Logs, "mymsg6"),
    {match, _} = re:run(Logs, "mymsg7").

%% We use individual callbacks to test for specific error values for TCP
%% transmissions if any
retry(Config) ->
    Listen = ?config(listen, Config),
    ChannelId = ?config(channel, Config),
    DrainId =  ?config(drain_id, Config),
    DrainTok = ?config(drain_tok, Config),
    Host = ?config(host, Config),
    Port = ?config(port, Config),
    %% Initialize the state. We have no good connection to do it over so we set
    %% a timer ref with a very long delay so that we don't try to reconnect in
    %% vain and lock the process
    Ref = erlang:start_timer(60000, self(), fake_timer),
    {ok, _, State0 = #state{buf=Buf},_} = logplex_tcpsyslog_drain2:init([
        #state{drain_id=DrainId, drain_tok=DrainTok, channel_id=ChannelId,
               host=Host, port=Port, reconnect_tref=Ref}
    ]),
    %% First we try to make stuff fail by pretending to be in
    %% the connected state
    Frame1 = [syslog("msg1",DrainTok), syslog("msg2",DrainTok)],
    Lost1 = 1, % we pretend!
    %% this calls send after putting the frame in the out_q
    {next_state, disconnected, State1} = logplex_tcpsyslog_drain2:connected(
        {logplex_drain_buffer, Buf, {frame, Frame1, length(Frame1), Lost1}},
        State0#state{sock=fake_port()}
    ),
    %% send then fails, but first queues up the frame and puts it in the retry
    %% bin. There should be no data in drop_info at this point
    #state{drop_info=undefined, out_q=Q1} = State1,
    [#frame{msg_count=2,loss_count=Lost1,tries=1}] = queue:to_list(Q1),
    %% more messages
    Frame2 = [syslog("msg3",DrainTok), syslog("msg4",DrainTok)],
    Lost2 = 0,
    %% Drop once more, get a failure that should result in a L10 later on
    {next_state, disconnected, State2} = logplex_tcpsyslog_drain2:connected(
        {logplex_drain_buffer, Buf, {frame, Frame2, length(Frame2), Lost2}},
        State1#state{sock=fake_port(), reconnect_tref=Ref}
    ),
    %% 3 dropes: one loss, 2 regulars. Second frame has never been tried so far
    #state{drop_info={_,3}, out_q=Q2} = State2,
    [#frame{msg_count=2,loss_count=Lost2,tries=2}] = queue:to_list(Q2),
    %% Fail once for the new one,
    %% We use a 'new_data' message that still goes through send as if
    %% stuff was incoming to prompt actual sending.
    {next_state, disconnected, State3} = logplex_tcpsyslog_drain2:connected(
        {logplex_drain_buffer, Buf, new_data},
        State2#state{sock=fake_port(), reconnect_tref=Ref}
    ),
    %% Tricky bit. We need to have a valid socket for things to work, but we
    %% both need to accept and connect at once.
    S = self(),
    spawn_link(fun() ->
        {ok, Sock} = gen_tcp:accept(Listen, 5000),
        ok = gen_tcp:controlling_process(Sock,S),
        S ! {sock, Sock}
        end),
    {ok,Client} = gen_tcp:connect(Host,Port,tcp_options()),
    {next_state, connected, State4} = logplex_tcpsyslog_drain2:connected(
        {logplex_drain_buffer, Buf, new_data},
        State3#state{sock=Client}
    ),
    #state{drop_info=undefined} = State4,
    receive
        {sock, Sock} ->
            % 3 messages expected: 3, 4, L10
            Logs = receive_logs(Sock, 3),
            nomatch = re:run(Logs, "msg1"),
            nomatch = re:run(Logs, "msg2"),
            {match, _} = re:run(Logs, "msg3"),
            {match, _} = re:run(Logs, "msg4"),
            {match, _} = re:run(Logs, "L10.*3 messages dropped")
        after 2000 ->
            error(too_long)
    end.

%%%%%%%%%%%%%
%%% UTILS %%%
%%%%%%%%%%%%%
msg(M) ->
    {user, debug, logplex_syslog_utils:datetime(now), "fakehost", "erlang", M}.

syslog(M,Tok) ->
    logplex_syslog_utils:frame(logplex_syslog_utils:to_msg(msg(M), Tok)).

%% receives N line-delimited messages from Sock.
%% We can do it with line mode because tcpsyslog
%% automatically adds a newline
receive_logs(Sock,N) ->
    inet:setopts(Sock, [{packet, line},{active,false}]),
    [begin
        {ok,Res} = gen_tcp:recv(Sock,0,2000),
        Res
     end || _ <- lists:seq(1,N)].

wait_for_mocked_call(Mod, Fun, Args, NumCalls, Time) ->
    wait_for_mocked_call(Mod,Fun,Args, NumCalls, Time*1000,os:timestamp()).

wait_for_mocked_call(Mod,Fun,Args, NumCalls, Max,T0) ->
    Called = meck:num_calls(Mod, Fun, Args),
    case {Called, timer:now_diff(os:timestamp(),T0)} of
        {N, Diff} when Diff > Max -> error({timeout,N});
        {N, _} when N >= NumCalls -> true;
        {_, _} ->
            timer:sleep(10),
            wait_for_mocked_call(Mod,Fun,Args,NumCalls,Max,T0)
    end.

%% borrowed from the module
tcp_options() ->
    [binary
     %% We don't expect data, but why not.
     ,{active, true}
     ,{exit_on_close, true}
     ,{keepalive, true}
     ,{packet, raw}
     ,{reuseaddr, true}
     ,{linger, {true,1}}
    ].

fake_port() ->
    {ok, Listen} = gen_tcp:listen(?PORT+1, [binary, {active,false}, {reuseaddr,true}]),
    Listen.

