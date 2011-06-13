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
-module(logplex_worker).
-export([start_link/1, init/1, loop/1]).

-include_lib("logplex.hrl").

-record(state, {regexp, map, interval}).

%% API functions
start_link(_QueuePid) ->
    proc_lib:start_link(?MODULE, init, [self()], 5000).

init(Parent) ->
    io:format("init ~p~n", [?MODULE]),
    {ok, RE} = re:compile("^<\\d+>\\S+ \\S+ \\S+ (t[.]\\S+) "),
    RedisBuffers = [{logplex_queue:get(Pid, redis_url), Pid} || {_Id, Pid, worker, _Modules} <- supervisor:which_children(logplex_redis_buffer_sup)],
    {ok, Map, Interval} = redis_shard:generate_map_and_interval(lists:sort(RedisBuffers)),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{regexp=RE, map=Map, interval=Interval}).

loop(#state{regexp=RE, map=Map, interval=Interval}=State) ->
    case catch logplex_queue:out(logplex_work_queue) of
        timeout ->
            ok;
        {'EXIT', _} ->
            exit(normal);
        {1, [Msg]} ->
            case re:run(Msg, RE, [{capture, all_but_first, binary}]) of
                {match, [Token]} ->
                    route(Token, Map, Interval, Msg);
                _ ->
                    ok
            end
    end,
    ?MODULE:loop(State).

route(Token, Map, Interval, Msg) when is_binary(Token), is_binary(Msg) ->
    case logplex_token:lookup(Token) of
        #token{channel_id=ChannelId, name=TokenName, app_id=AppId, addon=Addon} ->
            BufferPid = logplex_shard:lookup(integer_to_list(ChannelId), Map, Interval),
            logplex_stats:incr(logplex_stats_channels, {message_processed, AppId, ChannelId}),
            Msg1 = iolist_to_binary(re:replace(Msg, Token, TokenName)),
            process(ChannelId, BufferPid, Addon, Msg1);
        _ ->
            ok
    end.

process(ChannelId, BufferPid, Addon, Msg) ->
    logplex_tail:route(ChannelId, Msg),
    [logplex_queue:in(logplex_drain_buffer, {Host, Port, Msg}) || #drain{resolved_host=Host, port=Port} <- logplex_channel:lookup_drains(ChannelId)],
    logplex_queue:in(BufferPid, redis_helper:build_push_msg(ChannelId, spool_length(Addon), Msg)).

spool_length(<<"advanced">>) -> ?ADVANCED_LOG_HISTORY;
spool_length(_) -> ?DEFAULT_LOG_HISTORY.
