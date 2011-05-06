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
-export([create/1, lookup/1, expire_session_cache/0, init/1, loop/0]).

-include_lib("logplex.hrl").

create(Body) when is_binary(Body) ->
    SessionId = iolist_to_binary(["/sessions/", string:strip(os:cmd("uuidgen"), right, $\n)]),
    Expiration = datetime_to_epoch(erlang:universaltime()) + 360,
    {atomic, _} = mnesia:transaction(
        fun() ->
            Session = #session{id=SessionId, body=Body, expiration=Expiration},
            mnesia:write(session, Session, write)
        end),
    SessionId.

lookup(SessionId) when is_binary(SessionId) ->
    case ets:lookup(session, SessionId) of
        [#session{body=Body}] -> Body;
        _ -> undefined
    end.

expire_session_cache() ->
    proc_lib:start_link(?MODULE, init, [self()]).

init(Parent) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    loop().

loop() ->
    timer:sleep(60 * 1000),
    Now = datetime_to_epoch(erlang:universaltime()),
    Sessions = ets:tab2list(session),
    {atomic, _} = mnesia:transaction(
        fun() ->
            [mnesia:delete(session, Session, write) || #session{id=Session, expiration=Expiration} <- Sessions, Expiration > Now]
        end),
    loop().

datetime_to_epoch(Datetime) when is_tuple(Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).

