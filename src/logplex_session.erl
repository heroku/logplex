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
-module(logplex_session).
-export([create_ets_table/0]).
-export([new/1
         ,lookup/1
         ,delete/1
         ,expiry/0
         ,publish/1
         ,store/2
         ,poll/2
        ]).

-include("logplex_session.hrl").

create_ets_table() ->
    ets:new(sessions, [named_table, public, set, {keypos, 2}]).

new(Body) when is_binary(Body) ->
    SessionId = uuid:to_binary(uuid:v4()),
    #session{id=SessionId, body=Body}.

publish(Body) when is_binary(Body) ->
    Session = #session{ id = UUID } = new(Body),
    redis_helper:create_session(UUID, Body),
    store(Session).

store(UUID, Body)
  when is_binary(UUID), is_binary(Body),
       byte_size(UUID) =:= 36 ->
    store(#session{id=UUID, body=Body}).

store(Session = #session{id=SessionId, body=Body})
  when is_binary(SessionId), is_binary(Body) ->
    ets:insert(sessions, Session), SessionId.

poll(UUID, Timeout) when is_binary(UUID),
                         byte_size(UUID) =:= 36,
                         is_integer(Timeout) ->
    Ref = erlang:start_timer(Timeout, self(), poll_limit),
    poll_loop(UUID, Ref).

poll_loop(UUID, Ref) ->
    case lookup(UUID) of
        undefined ->
            receive
                {timeout, Ref, poll_limit} ->
                    timeout
            after 100 ->
                    poll_loop(UUID, Ref)
            end;
        Body ->
            %% Cancel timer and flush message queue of timeout messages
            case erlang:cancel_timer(Ref) of
                false ->
                    %% Only flush if timer had expired
                    receive
                        {timeout, Ref, poll_limit} -> ok
                    after 0 -> ok
                    end;
                _ -> ok
            end,
            Body
    end.

lookup(UUID) when is_binary(UUID),
                  byte_size(UUID) =:= 36 ->
    case ets:lookup(sessions, UUID) of
        [#session{body=Body}] -> Body;
        _ -> undefined
    end.

delete(UUID) when is_binary(UUID),
                  byte_size(UUID) =:= 36 ->
    ets:delete(sessions, UUID).

-spec expiry() -> binary().
%% @doc return the default redis expiry time in seconds as an ascii
%% integer encoded as a binary string.
expiry() ->
    Seconds = logplex_app:config(session_expiry_s, 360),
    list_to_binary(integer_to_list(Seconds)).
