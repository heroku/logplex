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

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([poll/2]).

-include("logplex_logging.hrl").
-include_lib("logplex.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    create_ets_tables(),
    {ok, []}.

create_ets_tables() ->
    logplex_channel:create_ets_table(),
    logplex_token:create_ets_table(),
    logplex_drain:create_ets_table(),
    logplex_cred:create_ets_table(),
    logplex_session:create_ets_table(),
    logplex_firehose:create_ets_tables(),
    ok.

-spec poll(fun ( () -> 'not_found' | {'found', T} | {'error', E} ),
           pos_integer()) ->
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
            poll_cancel(Ref),
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

handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
