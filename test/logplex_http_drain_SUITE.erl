-module(logplex_http_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-compile(export_all).

all() -> [{group, overflow},
          {group, drain_buf}].

groups() -> [{overflow, [], [full_buffer_success, full_buffer_fail,
                             full_buffer_temp_fail, full_stack]},
             {drain_buf, [], [restart_drain_buf, shrink]}].

-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                uri :: #ex_uri{},
                buf :: pid(),
                client :: pid(),
                out_q = queue:new() :: queue(),
                reconnect_tref :: reference() | 'undefined',
                drop_info,
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                service = normal :: 'normal' | 'degraded'
               }).

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    Config.

end_per_suite(_Config) ->
    application:stop(logplex),
    meck:unload().

init_per_group(drain_buf, Config) ->
    Config;
init_per_group(overflow, Config) ->
    Config.

end_per_group(drain_buf, _Config) ->
    ok;
end_per_group(overflow, _Config) ->
    ok.

init_per_testcase(restart_drain_buf, Config) ->
    %% Drain data
    ChannelId = 1337,
    DrainId = 2198712,
    DrainTok = "d.12930-321-312213-12321",
    {ok,URI,_} = ex_uri:decode("http://example.org"),
    %% HTTP Client
    meck:new(logplex_http_client, [passthrough]),
    meck:expect(logplex_http_client, start_link,
        fun(_Drain, _Channel, _Uri, _Scheme, _Host, _Port, _Timeout) ->
            {ok, self()}
        end),
    meck:expect(logplex_http_client, close, fun(_Pid) -> ok end),
    %% We make failure controleable by helper functions.
    %% Rube goldberg-esque, but makes writing the actual test
    %% a bit simpler. Succeeds by default
    Tab = client_call_init(),
    meck:expect(logplex_http_client, raw_request,
        fun(_Pid, _Req, _Timeout) ->
                Status = client_call_status(Tab),
                {ok, Status, []}
        end),
    %% Starting the drain
    {ok, Pid} = logplex_http_drain:start_link(ChannelId, DrainId, DrainTok, URI),
    unlink(Pid),
    [{channel, ChannelId}, {drain_id, DrainId}, {drain_tok, DrainTok},
     {uri, URI}, {drain,Pid}, {client, Tab} | Config];
init_per_testcase(full_stack, Config) ->
    %% Same as any other overflow test case but with drain buffers
    %% unmocked
    %% Drain data
    ChannelId = 1337,
    DrainId = 2198712,
    DrainTok = "d.12930-321-312213-12321",
    {ok,URI,_} = ex_uri:decode("http://example.org"),
    %% --- Mocks ---
    %% HTTP Client
    meck:new(logplex_http_client, [passthrough]),
    meck:expect(logplex_http_client, start_link,
        fun(_Drain, _Channel, _Uri, _Scheme, _Host, _Port, _Timeout) ->
            {ok, self()}
        end),
    meck:expect(logplex_http_client, close, fun(_Pid) -> ok end),
    %% We make failure controleable by helper functions.
    %% Rube goldberg-esque, but makes writing the actual test
    %% a bit simpler. Succeeds by default
    Tab = client_call_init(),
    meck:expect(logplex_http_client, raw_request,
        fun(_Pid, _Req, _Timeout) ->
                Status = client_call_status(Tab),
                {ok, Status, []}
        end),
    %% Starting the drain
    {ok, Pid} = logplex_http_drain:start_link(ChannelId, DrainId, DrainTok, URI),
    unlink(Pid),
    [{channel, ChannelId}, {drain_id, DrainId}, {drain_tok, DrainTok},
     {uri, URI}, {drain,Pid}, {client, Tab} | Config];
init_per_testcase(shrink, Config) ->
    %% Drain data
    ChannelId = 1337,
    DrainId = 2198712,
    DrainTok = "d.12931-e21-312213-12321",
    {ok,URI,_} = ex_uri:decode("http://example.org:80"),
    %% --- Mocks ---
    %% Drain buffer
    Ref = mock_drain_buffer(),
    %% HTTP Client
    meck:new(logplex_http_client, [passthrough]),
    meck:expect(logplex_http_client, start_link,
        fun(_Drain, _Channel, _Uri, _Scheme, _Host, _Port, _Timeout) ->
            {ok, self()}
        end),
    meck:expect(logplex_http_client, close, fun(_Pid) -> ok end),
    %% We make failure controleable by helper functions.
    %% Rube goldberg-esque, but makes writing the actual test
    %% a bit simpler. Succeeds by default
    Tab = client_call_init(),
    meck:expect(logplex_http_client, raw_request,
        fun(_Pid, _Req, _Timeout) ->
                Status = client_call_status(Tab),
                {ok, Status, []}
        end),
    %% Starting the drain
    {ok, Pid} = logplex_http_drain:start_link(ChannelId, DrainId, DrainTok, URI),
    unlink(Pid),
    [{channel, ChannelId}, {drain_id, DrainId}, {drain_tok, DrainTok},
     {uri, URI}, {buffer, Ref}, {drain,Pid}, {client, Tab} | Config];
init_per_testcase(_, Config) ->
    %% Drain data
    ChannelId = 1337,
    DrainId = 2198712,
    DrainTok = "d.12930-321-312213-12321",
    {ok,URI,_} = ex_uri:decode("http://example.org"),
    %% --- Mocks ---
    %% Drain buffer
    Ref = mock_drain_buffer(),
    %% HTTP Client
    meck:new(logplex_http_client, [passthrough]),
    meck:expect(logplex_http_client, start_link,
        fun(_Drain, _Channel, _Uri, _Scheme, _Host, _Port, _Timeout) ->
            {ok, self()}
        end),
    meck:expect(logplex_http_client, close, fun(_Pid) -> ok end),
    %% We make failure controleable by helper functions.
    %% Rube goldberg-esque, but makes writing the actual test
    %% a bit simpler. Succeeds by default
    Tab = client_call_init(),
    meck:expect(logplex_http_client, raw_request,
        fun(_Pid, _Req, _Timeout) ->
                Status = client_call_status(Tab),
                {ok, Status, []}
        end),
    %% Starting the drain
    {ok, Pid} = logplex_http_drain:start_link(ChannelId, DrainId, DrainTok, URI),
    unlink(Pid),
    [{channel, ChannelId}, {drain_id, DrainId}, {drain_tok, DrainTok},
     {uri, URI}, {buffer, Ref}, {drain,Pid}, {client, Tab} | Config].

end_per_testcase(_, Config) ->
    Drain = ?config(drain,Config),
    erlang:monitor(process, Drain),
    Drain ! shutdown,
    catch meck:unload(logplex_http_client),
    catch meck:unload(logplex_drain_buffer),
    client_call_end(?config(client,Config)),
    receive
        {'DOWN', _, _, Drain, {shutdown,call}} -> ok;
        {'DOWN', _, _, Drain, Other} -> ct:pal("DRAIN DIED OF REASON: ~p",[Other])
    end.


%%% Setup & teardown helpers %%%
set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"},
         {"LOGPLEX_HTTP_DRAIN_IDLE", "50"},
         {"LOGPLEX_HTTP_IDLE_FUZZ", "1"}
        ]].


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
    meck:expect(logplex_drain_buffer, resize_msg_buffer,
        fun(_Buf, Size) when Size > 0 ->
      ct:pal("CALLED: (~p, ~p)", [_Buf, Size]),
                ok
        end),
    Id.

client_call_init() ->
    Tid = ets:new(client_call, [public, set]),
    ets:insert(Tid, {status, 200}),
    Tid.

client_call_end(Tab) ->
    ets:delete(Tab).

client_call_status(Tab) ->
    try
        [{_,Val}] = ets:lookup(Tab, status),
        Val
    catch
        E:R ->
            ct:pal("OH THAT WENT BAD"),
            {E,R}
    end.

client_call_status(Tab, N) ->
    true = ets:insert(Tab, {status,N}).

%%% TESTS %%%
%% We drop a frame, but the rest is successfully delivered
full_buffer_success(Config) ->
    Buf = ?config(buffer, Config),
    Drain = ?config(drain, Config),
    Drain ! {logplex_drain_buffer, Buf, new_data},
    %% Here the drain should try connecting (and succeeding through mocks)
    %% and then set the buffer to active
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 1, 1000),
    %% Now is time to send messages. We should have a count of 15 successful
    %% and 3 skipped, within the same frame.
    Msg = "some io data that represents 15 messages",
    Frame = {frame, Msg, 15, 3},
    Drain ! {logplex_drain_buffer, Buf, Frame},
    %% When the drain is done sending stuff, it sets active back again
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 2, 1000),
    %% And we can check we received all the calls we needed.
    1 = meck:num_calls(logplex_http_client, raw_request, '_'),
    Hist = meck:history(logplex_http_client),
    [Success] =
      [iolist_to_binary(IoData) ||
       {_Pid, {_Mod, raw_request, [_Ref, IoData, _TimeOut]}, _Res} <- Hist],
    {match, _} = re:run(Success, Msg),
    L10 = "Error L10 \\(output buffer overflow\\): 3 messages dropped",
    {match, _} = re:run(Success, L10).

%% We drop frames twice, but the rest is successfully delivered. Overflow
%% messages should be accumulated. This one experiments with full failure of
%% delivery (400 range of HTTP responses)
full_buffer_fail(Config) ->
    Buf = ?config(buffer, Config),
    Drain = ?config(drain, Config),
    Client = ?config(client, Config),
    Drain ! {logplex_drain_buffer, Buf, new_data},
    %% Here the drain should try connecting (and succeeding through mocks)
    %% and then set the buffer to active
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 1, 1000),
    %% We send the message but expect it to fail. Because the error is in the
    %% 400s, we never retry (our request must be bad), so we accumulate it
    %% globally for later.
    client_call_status(Client, 404),
    Msg = "some io data that represents 15 messages",
    Frame = {frame, Msg, 15, 3},
    Drain ! {logplex_drain_buffer, Buf, Frame},
    %% When the drain is done sending (which fails), it sets active back
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 2, 1000),
    %% We can now allow sending again, send a second frame and expect our
    %% errors as part of the L10 message part of that frame
    client_call_status(Client, 200),
    Drain ! {logplex_drain_buffer, Buf, Frame},
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 3, 1000),
    %% We expect 2 requests:
    %% 1. failure for the correct log of the 1st frame
    %% 3. success for the correct log of the 2nd frame
    2 = meck:num_calls(logplex_http_client, raw_request, '_'),
    Hist = meck:history(logplex_http_client),
    [Fail, Success] =
      [iolist_to_binary(IoData) ||
       {_Pid, {_Mod, raw_request, [_Ref, IoData, _TimeOut]}, _Res} <- Hist],
    {match, _} = re:run(Fail, Msg),
    %% 15 + 3 failures
    {match, _} = re:run(Fail, "3 messages dropped"),
    {match, _} = re:run(Success, Msg),
    %% (15 + 3) + 3 failures
    {match, _} = re:run(Success, "21 messages dropped").

%% We drop frames twice, but the rest is successfully delivered. Overflow
%% messages should be accumulated. This one experiments with temporary
%% failure of delivery (500 range of HTTP responses)
full_buffer_temp_fail(Config) ->
    Buf = ?config(buffer, Config),
    Drain = ?config(drain, Config),
    Client = ?config(client, Config),
    Drain ! {logplex_drain_buffer, Buf, new_data},
    %% Here the drain should try connecting (and succeeding through mocks)
    %% and then set the buffer to active
    wait_for_mocked_call(logplex_drain_buffer, set_active, '_', 1, 1000),
    %% We send the message but expect it to fail.
    client_call_status(Client, 500),
    Msg = "some io data that represents 15 messages",
    Frame = {frame, Msg, 15, 3},
    Drain ! {logplex_drain_buffer, Buf, Frame},
    %% When the drain fails temp, it closes the connection before continuing and
    %% retrying again on the next frame sent
    1 = logplex_app:config(http_frame_retries, 1),
    wait_for_mocked_call(logplex_http_client, close, '_', 1, 1000),
    %% 1st frame: 15 queued, 3 lost (1 fail)
    %% :: 0 global lost, 1 req
    %%
    %% We can now send a second frame and expect our errors
    %% to be accumulated when the reconnection with the client is done.
    Drain ! {logplex_drain_buffer, Buf, Frame},
    wait_for_mocked_call(logplex_http_client, start_link, '_', 2, 1000), % reconnects
    wait_for_mocked_call(logplex_http_client, close, '_', 2, 1000),
    %% 1st frame:  0 queued, 0 lost (dropped after 2 fails)
    %% 2nd frame: 15 queued, 3 lost (0 attempt)
    %% :: 18 global lost, 2 req
    Drain ! {logplex_drain_buffer, Buf, Frame},
    wait_for_mocked_call(logplex_http_client, start_link, '_', 3, 1000),
    wait_for_mocked_call(logplex_http_client, close, '_', 3, 1000),
    %% 2nd frame: 15 queued, 3 lost (1 fail)
    %% 3rd frame: 15 queued, 3 lost (0 attempt)
    %% :: 18 global lost, 3 req
    %%
    %% We start making it suceed now.
    client_call_status(Client, 200),
    Drain ! {logplex_drain_buffer, Buf, Frame},
    %% 2nd frame: delivered, 3+(15+3) lost (1 req)
    %% 3rd frame: delivered, 3 lost (1 req)
    %% 4th frame: delivered, 3 lost (1 req)
    %% :: 18+3+3+3 = 27 global lost, (reqs for 3fail+3succeed = 6 total), 4 reconnections
    wait_for_mocked_call(logplex_http_client, start_link, '_', 4, 1000),
    %% Everything is sent
    6 = meck:num_calls(logplex_http_client, raw_request, '_'), % sends
    Hist = meck:history(logplex_http_client),
    [Fail1, Fail2, Fail3, Success1, Success2, Success3] =
      [iolist_to_binary(IoData) || {_Pid, {_Mod, raw_request, [_Ref, IoData, _TimeOut]}, _Res} <- Hist],
    {match, _} = re:run(Fail1, Msg), % temp 1st frame
    {match, _} = re:run(Fail2, Msg), % permanent 1st frame
    {match, _} = re:run(Fail3, Msg), % temp 2nd frame
    {match, _} = re:run(Success1, Msg), % success 2nd frame
    {match, _} = re:run(Success2, Msg), % success 3rd frame
    {match, _} = re:run(Success3, Msg), % success 4th frame
    %% 15 + 3 + 3 failures
    {match, _} = re:run(Success1, "21 messages dropped"),
    {match, _} = re:run(Success2, "3 messages dropped"),
    {match, _} = re:run(Success3, "3 messages dropped").

%% Checking that framing funs and overflow functions are alright
full_stack(Config) ->
    ChannelId = ?config(channel, Config),
    Client = ?config(client, Config),
    client_call_status(Client, 404),
    Msg = fun(M) -> {user, debug, logplex_syslog_utils:datetime(now),
                     "fakehost", "erlang", M}
    end,
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg1")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg2")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg3")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg4")),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg5")),
    wait_for_mocked_call(logplex_http_client, raw_request, '_', 1, 5000),
    client_call_status(Client, 200),
    logplex_channel:post_msg({channel, ChannelId}, Msg("mymsg6")),
    wait_for_mocked_call(logplex_http_client, raw_request, '_', 2, 5000),
    Hist = meck:history(logplex_http_client),
    [Failure, Success] =
      [iolist_to_binary(IoData) ||
       {_Pid, {_Mod, raw_request, [_Ref, IoData, _TimeOut]}, _Res} <- Hist],
    %% ensure idle drain is closed
    wait_for_mocked_call(logplex_http_client, close, '_', 1, 100),
    %% missed call
    {match, _} = re:run(Failure, "mymsg1"),
    {match, _} = re:run(Failure, "mymsg2"),
    {match, _} = re:run(Failure, "mymsg3"),
    {match, _} = re:run(Failure, "mymsg4"),
    {match, _} = re:run(Failure, "mymsg5"),
    nomatch = re:run(Failure, "mymsg6"),
    nomatch = re:run(Failure, "Error L10"),
    %% successful call
    nomatch = re:run(Success, "mymsg1"),
    nomatch = re:run(Success, "mymsg2"),
    nomatch = re:run(Success, "mymsg3"),
    nomatch = re:run(Success, "mymsg4"),
    nomatch = re:run(Success, "mymsg5"),
    {match, _} = re:run(Success, "mymsg6"),
    {match, _} = re:run(Success, "Error L10"),
    {match, _} = re:run(Success, "5 messages dropped").

%% Check that the drain restarts the drain buffer if we kill it.
restart_drain_buf(Config) ->
    {Buf0, true} = gen_fsm:sync_send_all_state_event(?config(drain, Config),
                                                     buf_alive),
    exit(Buf0, zing),
    ct:pal("Old buf ~p", [Buf0]),
    wait_for_dead_proc(Buf0),
    {Buf1, true} = gen_fsm:sync_send_all_state_event(?config(drain, Config),
                                                     buf_alive),
    ct:pal("New buf ~p", [Buf1]),
    false = Buf0 =:= Buf1.

shrink(Config) ->
    Buf = ?config(buffer, Config),
    State1 = #state{last_good_time={0,0,0},
                    buf = Buf,
                    uri = ?config(uri, Config),
                    service=normal},
    meck:expect(logplex_http_client, start_link,
        fun(_Drain, _Channel, _Uri, _Scheme, _Host, _Port, _Timeout) ->
            {error, mocked}
        end),
    %% Simulate an event according to which we've been disconnected
    %% for a long time. This goes -> cancel_reconnect -> try_connect.
    %% This in turns tries to connect, which fails because of the mock above.
    %% This in turns calls http_fail/1, which will call resize on the buffer
    Res1 = logplex_http_drain:disconnected({logplex_drain_buffer, Buf, new_data}, State1),
    %% This remains disconnected and degraded the service
    {next_state, disconnected, State2=#state{service=degraded}, hibernate} = Res1,
    %% also called the drain buffer for a resize down to 10
    wait_for_mocked_call(logplex_drain_buffer, resize_msg_buffer, ['_',10], 1, 1000),
    %% This worked, so let's see the opposite -- that the size is brought back up
    %% to whatever configured value we have:
    Val = logplex_app:config(http_drain_buffer_size, 1024),
    meck:expect(logplex_http_client, start_link,
        fun(_Drain, _Channel, _Uri, _Scheme, _Host, _Port, _Timeout) ->
            {ok, self()}
        end),
    Res2 = logplex_http_drain:disconnected({logplex_drain_buffer, Buf, new_data}, State2),
    {next_state, connected, #state{service=normal}, _} = Res2,
    %% The buffer was resized
    wait_for_mocked_call(logplex_drain_buffer, resize_msg_buffer, ['_',Val], 1, 1000).

%%% HELPERS
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

wait_for_dead_proc(Pid) ->
    Ref = erlang:monitor(process,Pid),
    receive
         {'DOWN', Ref, process, Pid, _} -> ok
    after
        1000 ->
            erlang:exit("process took more than 1s to exit")
    end.
