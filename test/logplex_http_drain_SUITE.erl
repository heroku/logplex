-module(logplex_http_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [{group, overflow}].

groups() -> [{overflow, [], [full_buffer_success, full_buffer_fail,
                             full_buffer_temp_fail]}].

init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    Config.

end_per_suite(_Config) ->
    application:stop(logplex),
    meck:unload().

init_per_group(overflow, Config) ->
    Config.

end_per_group(overflow, _Config) ->
    ok.

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
    meck:unload(logplex_http_client),
    meck:unload(logplex_drain_buffer),
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
         {"LOGPLEX_COOKIE", "ct test"}
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
    L10 = "Error L10 \\(Drain buffer overflow\\) -> This drain dropped 3 messages",
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
    {match, _} = re:run(Fail, "This drain dropped 3 messages"),
    {match, _} = re:run(Success, Msg),
    %% (15 + 3) + 3 failures
    {match, _} = re:run(Success, "This drain dropped 21 messages").

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
    {match, _} = re:run(Success1, "This drain dropped 21 messages"),
    {match, _} = re:run(Success2, "This drain dropped 3 messages"),
    {match, _} = re:run(Success3, "This drain dropped 3 messages").



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


