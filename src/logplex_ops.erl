%%%-------------------------------------------------------------------
%%% @doc 
%%% Provides operational functions for inspecting running logplex.
%%% Only for use in the shell. Do NOT call from other modules.
%%% @end
%%%-------------------------------------------------------------------
-module(logplex_ops).

-export([version/0,
         git_commit/0]).

version() ->
    os:cmd("git branch 2> /dev/null | sed -e '/^[^*]/d'").

git_commit() ->
    os:cmd("git log -1 --format=\"%H\"").


