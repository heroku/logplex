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
-export([create/1, lookup/1]).

-include_lib("logplex.hrl").

create(Body) when is_binary(Body) ->
    SessionId = iolist_to_binary(["/sessions/", string:strip(os:cmd("uuidgen"), right, $\n)]),
    Session = #session{id=SessionId, body=Body},
    ets:insert(sessions, Session),
    spawn_link(fun() ->
        timer:sleep(6 * 60 * 1000), % 6 mins
        ets:delete(sessions, SessionId)
    end),
    SessionId.

lookup(SessionId) when is_binary(SessionId) ->
    case ets:lookup(sessions, SessionId) of
        [#session{body=Body}] -> Body;
        _ -> undefined
    end.
