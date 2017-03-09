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
    [by_channel, by_id].

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
init_per_testcase(by_id, Config) ->
    logplex_db:start_link(),
    Config;
init_per_testcase(by_channel, Config) ->
    logplex_db:start_link(),
    [{chan, <<"1">>} | Config];
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    meck:unload(),
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

    meck:expect(redis_helper, delete_token, fun(_Tok) -> ok end),
    logplex_token:delete_by_channel(Chan),
    [] = logplex_token:lookup_by_channel(Chan),
    4 = meck:num_calls(redis_helper, delete_token, '_'),
    ok.

by_id(_Config) ->
    Chan = <<"2">>,
    Id = <<"t.1">>,
    BobT = logplex_token:new(Id, Chan, <<"bob">>),
    undefined = logplex_token:lookup(Id),
    logplex_token:load(BobT),
    BobT = logplex_token:lookup(Id),
    [] = logplex_token:lookup_by_channel(Chan),
    logplex_token:cache(BobT),
    [BobT] = logplex_token:lookup_by_channel(Chan),
    [Id] = logplex_token:lookup_ids_by_channel(Chan),
    meck:expect(redis_helper, delete_token, fun(_Tok) -> ok end),
    logplex_token:delete_by_id(Id),
    [] = logplex_token:lookup_by_channel(Chan),
    [] = logplex_token:lookup_ids_by_channel(Chan),
    undefined = logplex_token:lookup(Id),
    1 = meck:num_calls(redis_helper, delete_token, '_'),
    ok.
