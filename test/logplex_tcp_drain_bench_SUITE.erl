-module(logplex_tcp_drain_bench_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-compile(export_all).
-define(PORT_OLD, 9601).
-define(PORT_NEW, 9602).

all() -> [find_end].

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP / TEADOWN %%%
%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
    set_os_vars(),
    ok = logplex_app:a_start(logplex, temporary),
    Config.

end_per_suite(_Config) ->
    application:stop(logplex),
    meck:unload().

init_per_testcase(_, Config) ->
    application:set_env(logplex, tcp_drain_buffer_size, 50),
    ChannelId = 1337,
    DrainIdOld = 2198712,
    DrainIdNew = 2198713,
    DrainTokOld = "d.12930-321-312213-12321-old",
    DrainTokNew = "d.12930-321-312213-12321-new",
    {ok,URIOld,_} = ex_uri:decode("syslog://127.0.0.1:"++integer_to_list(?PORT_OLD)),
    {ok,URINew,_} = ex_uri:decode("syslog://127.0.0.1:"++integer_to_list(?PORT_NEW)),
    {ok, PidOld} = logplex_tcpsyslog_drain:start_link(ChannelId, DrainIdOld, DrainTokOld, URIOld),
    {ok, PidNew} = logplex_tcpsyslog_drain2:start_link(ChannelId, DrainIdNew, DrainTokNew, URINew),
    unlink(PidOld),
    unlink(PidNew),
    [{channel,ChannelId},{old,PidOld},{new,PidNew}
     | Config].

end_per_testcase(_, Config) ->
    Old = ?config(old,Config),
    New = ?config(new,Config),
    shutdown(Old),
    shutdown(New).

%%% Setup & teardown helpers %%%
set_os_vars() ->
    [os:putenv(Key,Val) || {Key,Val} <-
        [{"INSTANCE_NAME", "localhost"},
         {"LOCAL_IP", "localhost"},
         {"CLOUD_DOMAIN", "localhost"},
         {"LOGPLEX_AUTH_KEY", uuid:to_string(uuid:v4())},
         {"LOGPLEX_COOKIE", "ct test"}
        ]].

shutdown(Pid) ->
    erlang:monitor(process, Pid),
    Pid ! shutdown,
    receive
        {'DOWN', _, _, Pid, {shutdown,call}} -> ok;
        {'DOWN', _, _, Pid, Other} -> ct:pal("DRAIN DIED OF REASON: ~p",[Other])
    after 2000 ->
        error({not_dead, sys:get_status(Pid)})
    end.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
find_end(Config) ->
    RNew = receiver(self(), new),
    ROld = receiver(self(), old),
    Total = 5000,
    Slice = 200,
    Sleep = 5,
    send_msgs(?config(channel,Config), Total, "some message", Slice, Sleep),
    MsgsOld = receive
        {ROld, OldRes} -> OldRes
    end,
    MsgsNew = receive
        {RNew, NewRes} -> NewRes
    end,
    OldTime = total_time(MsgsOld),
    NewTime = total_time(MsgsNew),
    L10ctOld = l10_count(MsgsOld),
    L10ctNew = l10_count(MsgsNew),
    L10msgOld = l10_msgs(MsgsOld),
    L10msgNew = l10_msgs(MsgsNew),
    ct:pal("Total: ~p, Slice: ~p, Pauses: ~pms~n"
           "------~n"
           "Shortest: ~p (~p vs. ~p ~p%)~n"
           "L10 old: ~p% (~p over ~p L10 messages)~n"
           "L10 new: ~p% (~p over ~p L10 messages)~n",
           [Total, Slice, Sleep,
            if OldTime < NewTime -> old; OldTime >= NewTime -> new end,
            OldTime, NewTime, (OldTime/NewTime)*100,
            (L10ctOld/Total)*100, L10ctOld, L10msgOld,
            (L10ctNew/Total)*100, L10ctNew, L10msgNew]).


%% Measure:
%% - memory (all involved procs)
%% - reductions?
%% - msg queue len
%% - L10 msg count
%% - overall speed
%% - msg delivered per sec
%%
%% Expectations:
%% - memory is nicer for syslog2
%% - reductions is ???
%% - msg queue len is shorter for syslog2
%% - L10 count is lower for syslog
%% - overall speed is better for syslog2
%% - msg delivered per sec better for syslog
%%
%% Test: 3 levels
%%  - low use
%%  - medium use
%%  - overload use

%%%%%%%%%%%%%
%%% UTILS %%%
%%%%%%%%%%%%%

send_msgs(ChannelId, N, Msg, _Slice, _Sleep) when N =< 0 ->
    %% The last message is pretty much guaranteed to make it.
    logplex_channel:post_msg({channel, ChannelId}, msg("last "++Msg));
send_msgs(ChannelId, N, Msg, Slice, Sleep) ->
    Msgs = [msg("regular "++Msg) || _ <- lists:seq(1,Slice)],
    [logplex_channel:post_msg({channel, ChannelId}, Entry)
     || Entry <- Msgs],
    timer:sleep(Sleep),
    send_msgs(ChannelId, N-Slice, Msg, Slice, Sleep).

msg(M) ->
    {user, debug, logplex_syslog_utils:datetime(now), "fakehost", "erlang", M}.

receiver(Pid, Type) ->
    spawn_link(fun() ->
        State = serv_init(Type),
        Pid ! {self(), serv(State)}
    end).

serv_init(Type) ->
    Port = case Type of
        new -> ?PORT_NEW;
        old -> ?PORT_OLD
    end,
    {ok, Listen} = gen_tcp:listen(Port, [binary,{active,false},
                                         {packet,line},{reuseaddr,true}]),
    {ok, Sock} = gen_tcp:accept(Listen, 5000),
    Sock.

serv(Sock) -> serv(Sock,[]).

serv(Sock, Acc) ->
    {ok, Bin} =  gen_tcp:recv(Sock,0,2000),
    case binary:match(Bin, <<"last">>) of
        nomatch -> serv(Sock, [{os:timestamp(), Bin} | Acc]);
        _ -> lists:reverse([{os:timestamp(), Bin} | Acc])
    end.

total_time(Msgs) ->
    {First,_} = hd(Msgs),
    {Last,_} = hd(lists:reverse(Msgs)),
    timer:now_diff(Last, First).

l10_count(MsgList) ->
    Msgs = [Msg || {_, Msg} <- MsgList],
    case re:run(Msgs, "([0-9]+) messages dropped", [{capture, all_but_first, list}, global]) of
        nomatch -> 0;
        {match, Lists} -> lists:sum([list_to_integer(X) || [X] <- Lists])
    end.

l10_msgs(MsgList) ->
    Msgs = [Msg || {_, Msg} <- MsgList],
    case re:run(Msgs, "([0-9]+) messages dropped", [{capture, all_but_first, list}, global]) of
        nomatch -> 0;
        {match, Lists} -> length(Lists)
    end.

