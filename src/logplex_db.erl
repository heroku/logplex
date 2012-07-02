%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(logplex_db).
-export([start_link/0]).
-export([poll/2]).

-include_lib("logplex.hrl").

start_link() ->
    create_ets_tables(),
    redo:start_link(config, redo_opts()).

create_ets_tables() ->
    ets:new(channels, [named_table, public, set, {keypos, 2}]),
    ets:new(tokens,   [named_table, public, set, {keypos, 2}]),
    ets:new(drains,   [named_table, public, set, {keypos, 2}]),
    logplex_session:create_ets_table(),
    ets:new(drain_sockets, [named_table, public, set]),
    ets:new(drain_socket_quarentine, [named_table, public, set]),
    ok.

redo_opts() ->
    case os:getenv("LOGPLEX_CONFIG_REDIS_URL") of
        false -> [];
        Url -> redo_uri:parse(Url)
    end.

-spec poll(fun ( () -> 'not_found' | {'found', T} | {'error', E} ),
           pos_integer) ->
                  {error, timeout} | {error, E} | T.

poll(Fun, Timeout) when is_function(Fun, 0),
                         is_integer(Timeout) ->
    Ref = erlang:start_timer(Timeout, self(), poll_limit),
    poll_loop(Fun, Ref).

poll_loop(Fun, Ref) ->
    case Fun() of
        not_found ->
            receive
                {timeout, Ref, poll_limit} ->
                    {error, timeout}
            after 100 ->
                    poll_loop(Fun, Ref)
            end;
        {found, Value} ->
            %% Cancel timer and flush message queue of timeout messages
            poll_cancel(Ref)
            Value;
        Else ->
            poll_cancel(Ref),
            {error, Else}
    end.

poll_cancel(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            %% Only flush if timer had expired
            receive
                {timeout, Ref, poll_limit} -> ok
            after 0 -> ok
            end;
        _ -> ok
    end.
