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
%% Nsync calls
-export([insert_session/3, expire_session/1]).

-include_lib("logplex.hrl").
-include_lib("nsync_helper.hrl").

create(Body) when is_binary(Body) ->
    Session = iolist_to_binary(["/sessions/", string:strip(os:cmd("uuidgen"), right, $\n)]),
    redis_helper:create_session(Session, Body),
    Session.

lookup(Session) when is_binary(Session) ->
    case ets:lookup(?MODULE, Session) of
        [] ->
	    {error, not_found};
        [{Session, Body}] ->
            Body
    end.



%%--------------------------------------------------------------------
%% @doc Inserts a session into logplex_session ETS (previously created
%% in logplex_nsync_callback) and starts a timer expiring it
%% @spec insert_session(Key::binary(), TTL::integer, Value::binary) ->
%% ok
%% @end
%%--------------------------------------------------------------------
insert_session(Key, TTL, Value) ->
    ets:insert(logplex_session, {Key, Value}),
    timer:apply_after(TTL,
		      ?MODULE,
		      expire_session,
		      [Key]).


%%--------------------------------------------------------------------
%% @doc Expires a previously created session
%% @spec expire_session(Key::binary()) -> ok
%% @end
%%--------------------------------------------------------------------
expire_session(Key) ->
    ets:delete(logplex_session, Key).
