%%%-------------------------------------------------------------------
%% @copyright Heroku, 2013
%% @author Fred Hebert <mononcqc@ferd.ca>
%% @doc CommonTest test suite for logplex_drain
%% @end
%%%-------------------------------------------------------------------

-module(logplex_channel_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom.

all() ->
    [properties].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    Config.

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(_, Config) ->
    application:start(gproc),
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    application:stop(gproc),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

properties(_Config) ->
    Channel = {channel, <<"2189312">>},
    Msg1 = msg(term_to_binary(make_ref())),
    Msg2 = msg(term_to_binary(make_ref())),
    S = self(),
    spawn_link(fun() ->
        logplex_channel:register(Channel),
        S ! ok,
        receive M -> S ! {rec, M} end,
        receive N -> S ! {rec, N} end
    end),
    logplex_channel:register(Channel),
    receive ok -> ok end,
    [_,_] = logplex_channel:whereis(Channel),
    ok = logplex_channel:post_msg(Channel, Msg1),
    ok = recv_msg({post, Msg1}),
    ok = recv_msg({rec, {post, Msg1}}),
    logplex_channel:unregister(Channel),
    [_] = logplex_channel:whereis(Channel),
    %% 2nd message
    ok = logplex_channel:post_msg(Channel, Msg2),
    timeout = recv_msg({post, Msg2}),
    ok = recv_msg({rec, {post, Msg2}}),
    %% The process should be down
    [] = logplex_channel:whereis(Channel).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
recv_msg(Msg) ->
    receive
        Msg -> ok
    after 2000 ->
        timeout
    end.

msg(M) ->
    {user, debug, logplex_syslog_utils:datetime(now), "fakehost", "erlang", M}.
