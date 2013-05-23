-module(logplex_tcp_drain_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

-define(PORT, 9601).
-define(DRAIN_BUFFER_SIZE,5).

all() -> [full_stack].

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
                buf = logplex_msg_buffer:new() :: logplex_msg_buffer:buf(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% TCP failures since last_good_time
                failures = 0 :: non_neg_integer(),
                %% Reconnect timer reference
                reconnect_tref = undefined :: 'undefined' | reference(),
                %% Send timer reference
                send_tref = undefined :: 'undefined' | reference(),
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
         {"LOGPLEX_COOKIE", "ct test"}
        ]].

start_server(_Config) ->
    {ok, Listen} = gen_tcp:listen(?PORT, [binary, {active,false}, {reuseaddr,true}]),
    Listen.

stop_server(Config) ->
    Port = ?config(port, Config),
    catch gen_tcp:close(Port).


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
    ct:pal("Res: ~p",[Logs]),
    {match, _} = re:run(Logs, "mymsg1"),
    nomatch    = re:run(Logs, "mymsg2"),
    {match, _} = re:run(Logs, "L10.*1 messages? dropped"),
    {match, _} = re:run(Logs, "mymsg3"),
    {match, _} = re:run(Logs, "mymsg4"),
    {match, _} = re:run(Logs, "mymsg5"),
    {match, _} = re:run(Logs, "mymsg6"),
    {match, _} = re:run(Logs, "mymsg7").

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
receive_logs(N, Sock) ->
    inet:setopts(Sock, [{packet, line},{active,false}]),
    [begin
        {ok,Res} = gen_tcp:recv(Sock,0,2000),
        Res
     end || _ <- lists:seq(1,N)].
