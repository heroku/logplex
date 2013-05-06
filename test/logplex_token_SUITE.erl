%%%-------------------------------------------------------------------
%% @copyright Heroku, 2013
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @doc CommonTest test suite for logplex_token
%% @end
%%%-------------------------------------------------------------------

-module(logplex_token_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom.

all() ->
    [by_channel].

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
init_per_testcase(by_channel, Config) ->
    logplex_db:start_link(),
    [{chan, 1} | Config];
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

by_channel(Config) ->
    Chan = ?config(chan, Config),
    Tokens = [ logplex_token:new(Chan, list_to_binary(Name))
               || Name <- ["heroku", "app", "postgres", "bob"]],
    [] = logplex_token:lookup_by_channel(Chan),
    [ logplex_token:cache(Token)
      || Token <- Tokens ],
    true = lists:sort(Tokens) =:=
        lists:sort(logplex_token:lookup_by_channel(Chan)),
    logplex_token:delete_by_channel(Chan),
    [] = logplex_token:lookup_by_channel(Chan).
