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
-module(logplex_stats).

%% API
-export([start_link/0,
         report/2]).

-export([healthcheck/0, incr/1, incr/2]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
    Interval = timer:seconds(logplex_app:config(stats_reporting_interval, 60)),
    ectr_srv:start_link(?MODULE,
                        {?MODULE, report},
                        Interval).

healthcheck() ->
    redis_helper:healthcheck().

incr(Key) ->
    incr(Key, 1).

-spec incr(#drain_stat{} | #channel_stat{} | #logplex_stat{} |
           #queue_stat{} | list() | atom(), integer()) ->
                  any().
incr(Key, Incr) when is_integer(Incr) ->
    ectr_srv:incr(?MODULE, Key, Incr).

unix_ts({Mega, S, _}) ->
    Mega * 1000000 + S.

report(Now, {K, V}) ->
    log_stat(unix_ts(Now), K, V).

log_stat(UnixTS, #drain_stat{drain_id=DrainId, channel_id=ChannelId, key=Key}, Val) ->
    io:format("m=logplex_stats ts=~p channel_id=~p drain_id=~p ~p=~p~n",
        [UnixTS, ChannelId, DrainId, Key, Val]);

log_stat(UnixTS, #channel_stat{channel_id=ChannelId, key=Key}, Val) ->
    io:format("m=logplex_stats ts=~p channel_id=~p ~p=~p~n",
        [UnixTS, ChannelId, Key, Val]);

log_stat(UnixTS, #logplex_stat{module=Mod, key=K}, Val) ->
    io:format("m=logplex_stats ts=~p system module=~p ~200p=~p~n",
        [UnixTS, Mod, K, Val]);

log_stat(UnixTS, #queue_stat{redis_url=RedisUrl, key=Key}, Val) ->
    io:format(user, "m=logplex_stats ts=~p redis_url=~p ~p=~p~n",
              [UnixTS, RedisUrl, Key, Val]);

log_stat(UnixTS, {Class, Key}, Val) ->
    io:format("m=logplex_stats ts=~p freeform class=~p key=~p count=~p~n",
        [UnixTS, Class, Key, Val]);

log_stat(UnixTS, Key, Val) when is_atom(Key); is_list(Key) ->
    io:format("m=logplex_stats ts=~p ~p=~p~n", [UnixTS, Key, Val]).
