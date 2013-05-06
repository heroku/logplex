%%%-------------------------------------------------------------------
%% @copyright You, 2036
%% @author You <erlanghacker@example.com>
%% @doc CommonTest test suite for logplex_channel
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
    [token_caching].

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
init_per_testcase(token_caching, Config) ->
    logplex_channel:create_ets_table(),
    Config;
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    ets:delete(channels),
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%

token_caching(_Config) ->
    ChannelId = 1,
    Chan0 = logplex_channel:new(ChannelId, <<"Test channel">>),
    [] = logplex_channel:tokens(Chan0),
    logplex_channel:cache(Chan0),
    Token = logplex_token:new(logplex_token:new_unique_token_id(), ChannelId),
    Chan1 = logplex_channel:cache_token(ChannelId, Token),
    Chan1 = logplex_channel:lookup(ChannelId),
    Token = logplex_channel:has_token(logplex_token:id(Token), Chan1),
    [Token] = logplex_channel:lookup_tokens(ChannelId),
    Chan1 = logplex_channel:cache_token(ChannelId, Token),
    Token = logplex_channel:has_token(logplex_token:id(Token), Chan1),
    Token2 = logplex_token:new(logplex_token:new_unique_token_id(), ChannelId),
    logplex_channel:cache_token(ChannelId, Token2),
    Token2 = logplex_channel:has_token(logplex_token:id(Token2), Chan1),
    ok.
