-module(logplex_tcp_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

-define(PORT, 9601).
-define(DRAIN_BUFFER_SIZE,5).

all() -> [full_stack, {group, with_tcp_server}].

groups() -> [{with_tcp_server,[],[shrink]}].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP / TEADOWN %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: move to .hrl file
%% Directly copy/pastedfrom logplex_tcp_syslog_drain
-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                host :: string() | inet:ip_address() | binary(),
                port :: inet:port_number(),
                sock = undefined :: 'undefined' | inet:socket(),
                %% Buffer for messages while disconnected
                buf = logplex_msg_buffer:new() :: logplex_msg_buffer:buf(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% TCP failures since last_good_time
                failures = 0 :: non_neg_integer(),
                %% Reconnect timer reference
                reconnect_tref = undefined :: 'undefined' | reference(),
                %% Send timer reference
                send_tref = undefined :: 'undefined' | reference(),
                %% Idle timer reference
                idle_tref = undefined :: 'undefined' | reference(),
                %% Time of last successful connection
                connect_time :: 'undefined' | erlang:timestamp()
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

init_per_testcase(shrink, Config) ->
    with_tcp_server([{channel, 1337} | Config]);
init_per_testcase(_, Config) ->
    application:set_env(logplex, tcp_drain_buffer_size, 5),
    Port = start_server(Config),
    %% Drain data
    ChannelId = 1337,
    DrainId = 2198712,
    DrainTok = "d.12930-321-312213-12321",
    {ok,URI,_} = ex_uri:decode("syslog://127.0.0.1:"++integer_to_list(?PORT)),
    {ok, Pid} = logplex_tcpsyslog_drain:start_link(ChannelId, DrainId, DrainTok, URI),
    unlink(Pid),
    [{channel,ChannelId},{port,Port},{drain,Pid} | Config].

end_per_testcase(shrink, Config) ->
    end_tcp_server(Config);
end_per_testcase(_, Config) ->
    Drain = ?config(drain,Config),
    erlang:monitor(process, Drain),
    Drain ! shutdown,
    receive
        {'DOWN', _, _, Drain, {shutdown,call}} -> ok;
        {'DOWN', _, _, Drain, Other} -> ct:pal("DRAIN DIED OF REASON: ~p",[Other])
    after 2000 ->
        case Drain of
            undefined -> ok;
            _ -> error({not_dead, sys:get_status(Drain)})
        end
    end,
    stop_server(Config).

%%% Setup & teardown helpers %%%
set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_TCP_DRAIN_IDLE", "50"}
        ]],
    logplex_app:cache_os_envvars().


mock_drain_buffer() ->
    Id = self(),
    meck:new(logplex_drain_buffer, [passthrough, no_link]),
    meck:expect(logplex_drain_buffer, start_link,
                fun(_ChannelId, _Pid, _State, _Size) ->
                    {ok, Id}
                end),
    meck:expect(logplex_drain_buffer, notify, fun(_) -> ok end),
    meck:expect(logplex_drain_buffer, set_active,
                fun(_Buf, _Bytes, _Fun) ->
                    ok
                end),
    Id.

%% start_server starts a listener and hands the port over, whereas
%% with_tcp_server starts a real TCP server good for one request being accepted
start_server(_Config) ->
    {ok, Listen} = gen_tcp:listen(?PORT, [binary, {active,false}, {reuseaddr,true}]),
    Listen.

stop_server(Config) ->
    Port = ?config(port, Config),
    catch gen_tcp:close(Port).

with_tcp_server(Config) ->
    Self = self(),
    Server = fun() ->
        {ok, Listen} = gen_tcp:listen(0,[]),
        {ok, Port} = inet:port(Listen),
        Self ! {endpoint, self(), {{127,0,0,1}, Port}},
        {ok, _Accept} = gen_tcp:accept(Listen),
        timer:sleep(infinity)
    end,
    Pid = spawn_link(Server),
    receive
        {endpoint, Pid, {Ip, Port}} ->
            [{endpoint, {Pid, Ip, Port}},
             {channel, 1337} | Config]
    after 5000 ->
        error(bad_server)
    end.

end_tcp_server(Config) ->
    {Pid, _, _} = ?config(endpoint, Config),
    unlink(Pid),
    exit(Pid, kill),
    Config.


%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
%% We drop a frame, but the rest is successfully delivered. The message is
%% implicitly dropped due to the drain buffer limit being set to 5
full_stack(Config) ->
    Listen = ?config(port, Config),
    Drain = ?config(drain, Config),
    ChannelId = ?config(channel, Config),
    Msg = fun(M) -> {user, debug, logplex_syslog_utils:datetime(now),
                     "fakehost", "erlang", M}
    end,
    %% due to an implementation detail, a disconnected tcp syslog drain
    %% has a buffer of N+1 after receiving the first message and then trying
    %% to connect while the rest accumulates in the mailbox.
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg1")), % not in buffer
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg2")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg3")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg4")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg5")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg6")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg7")),
    wait_until(fun() -> % all made it through, messages 2-6 or 3-7 are in the buffer
                {_Name,#state{buf=Buf}} = get_state(Drain),
                length(buf_entries(Buf)) =:= 5
               end),
    wait_until(fun() -> % no message left, 3-7 in the buffer
                0 =:= element(2,process_info(Drain, message_queue_len))
               end),
    {ok, Sock} = gen_tcp:accept(Listen, 5000),
    Logs = receive_logs(Sock, 7),
    timer:sleep(100),
    undefined = erlang:port_info(Sock), % drain should idle out
    {match, _} = re:run(Logs, "mymsg1"),
    nomatch    = re:run(Logs, "mymsg2"),
    {match, _} = re:run(Logs, "L10.*1 messages? dropped"),
    {match, _} = re:run(Logs, "mymsg3"),
    {match, _} = re:run(Logs, "mymsg4"),
    {match, _} = re:run(Logs, "mymsg5"),
    {match, _} = re:run(Logs, "mymsg6"),
    {match, _} = re:run(Logs, "mymsg7").

shrink(Config) ->
    %% Use an explicitly larger default than whatever the suite sets
    application:set_env(logplex, tcp_drain_buffer_size, 1024),
    Msg = fun(M) -> {user, debug, logplex_syslog_utils:datetime(now),
                     "fakehost", "erlang", M}
    end,
    {_, Ip, Port} = ?config(endpoint, Config),
    ChannelId = ?config(channel, Config),
    FullBuff = lists:foldl(
        fun(M,Buf) -> logplex_msg_buffer:push(M,Buf) end,
        logplex_msg_buffer:new(10000),
        [Msg(integer_to_binary(N)) || N <- lists:seq(1,10000)]
    ),
    HalfBuff = logplex_msg_buffer:resize(20000, FullBuff),
    Ref = make_ref(),
    State0 = #state{
        drain_id=1337,
        drain_tok= <<"some token">>,
        channel_id=ChannelId,
        host=Ip,
        port=Port,
        buf = FullBuff,
        reconnect_tref = Ref,
        failures=1,
        last_good_time = {0,0,0}
    },
    %% Posting will prompt a reconnection. Because our reference is invalid
    %% *and* because we've had a failure before, the drain will assume we just
    %% failed and will set up a backoff + timer, and attempt to shrink the
    %% drains. Note that this is different than if we had received the timer
    %% timeout event (which would have called do_reconnect)
    %%
    %% First try, don't resize because we don't have enough failures
    {next_state, disconnected, State1, _} = logplex_tcpsyslog_drain:disconnected({post,Msg("a")}, State0),
    #state{buf=Buf1} = State1,
    10000 = logplex_msg_buffer:len(Buf1),
    10000 = logplex_msg_buffer:max_size(Buf1),
    full = logplex_msg_buffer:full(Buf1),
    %% then we don't resize because the buffer isn't full/hasn't lost data
    {next_state, disconnected, State2, _} = logplex_tcpsyslog_drain:disconnected({post,Msg("a")}, State0#state{buf=HalfBuff, failures=100}),
    #state{buf=Buf2} = State2,
    20000 = logplex_msg_buffer:max_size(Buf2),
    have_space = logplex_msg_buffer:full(Buf2),
    %% Then we lose because we have all the good criteria
    {next_state, disconnected, State3, _} = logplex_tcpsyslog_drain:disconnected({post,Msg("a")}, State0#state{failures=100}),
    #state{buf=Buf3} = State3,
    10 = logplex_msg_buffer:len(Buf3),
    %% 9990 drops + the one triggered by {post, Msg}
    {{loss_indication, 9991, _}, _} = logplex_msg_buffer:pop(Buf3),
    10 = logplex_msg_buffer:len(logplex_msg_buffer:push(msg, Buf3)),
    %% Buffer seems sane. Now let's reconnect and see if it gets resized.
    %% The server we used accepts only one connection and this one should
    %% be established.
    State4 = State3#state{sock=undefined, failures = 0, reconnect_tref = Ref},
    %% force a reconnect. This calls 'do_reconnect', which
    %% should succeed, then try to send data (which should succeed),
    %% then go to 'sending'
    {next_state, sending, State5} = logplex_tcpsyslog_drain:disconnected({timeout, Ref, reconnect}, State4),
    #state{buf=Buf4} = State5,
    0 = logplex_msg_buffer:len(Buf4), % all stuff was sent
    %% resized to configured default
    Default = logplex_app:config(tcp_drain_buffer_size, 1024),
    Default = logplex_msg_buffer:len(lists:foldl(
        fun(M,Buf) -> logplex_msg_buffer:push(M,Buf) end,
        Buf4,
        [Msg(integer_to_binary(N)) || N <- lists:seq(1,10000)]
    )).





%%%%%%%%%%%%%
%%% UTILS %%%
%%%%%%%%%%%%%
get_state(Pid) ->
    {status,Pid,
     _Mod,
     [_Pdict,
      _Sched,_Parent,_,
      [_Header,
       {data,[{"Status",running},
              {"Parent",_Parent},
              {"Logged events",_},
              {"StateName",StateName}]},
       {data,[{"StateData",
               State}]}]]} = sys:get_status(Pid),
    {StateName,State}.

buf_entries(BufState) ->
    queue:to_list(element(2,BufState)).

wait_until(F) ->
    case F() of
        true -> ok;
        false ->
            timer:sleep(100),
            wait_until(F)
    end.

%% receives N line-delimited messages from Sock.
%% We can do it with line mode because tcpsyslog
%% automatically adds a newline
receive_logs(Sock, N) ->
    inet:setopts(Sock, [{packet, line},{active,false}]),
    [begin
        {ok,Res} = gen_tcp:recv(Sock,0,2000),
        Res
     end || _ <- lists:seq(1,N)].
