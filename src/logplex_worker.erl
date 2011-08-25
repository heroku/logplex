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

-record(state, {regexp, map, interval, drain_accepting=true}).

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

loop(#state{regexp=RE, map=Map, interval=Interval, drain_accepting=DrainAccepting}=State) ->
    DrainAccepting1 =
        receive
            {logplex_drain_buffer, stop_accepting} -> false;
            {logplex_drain_buffer, start_accepting} -> true
        after 0 -> DrainAccepting
        end,
    case catch logplex_queue:out(logplex_work_queue) of
        timeout ->
            ok;
        {'EXIT', _} ->
            exit(normal);
        {1, [Msg]} ->
            case re:run(Msg, RE, [{capture, all_but_first, binary}]) of
                {match, [Token]} ->
                    route(Token, Map, Interval, Msg, DrainAccepting1);
                _ ->
                    ok
            end
    end,
    ?MODULE:loop(State#state{drain_accepting=DrainAccepting1}).

route(Token, Map, Interval, Msg, DrainAccepting) when is_binary(Token), is_binary(Msg) ->
    case logplex_token:lookup(Token) of
        #token{channel_id=ChannelId, name=TokenName, app_id=AppId, drains=Drains} ->
            BufferPid = logplex_shard:lookup(integer_to_list(ChannelId), Map, Interval),
            logplex_stats:incr(logplex_stats_channels, {message_processed, AppId, ChannelId}),
            Msg1 = iolist_to_binary(re:replace(Msg, Token, TokenName)),
            process_drains(AppId, ChannelId, Drains, Msg1, DrainAccepting),
            process_tails(ChannelId, Msg1),
            process_msg(ChannelId, BufferPid, Msg1);
        _ ->
            ok
    end.

process_drains(_AppId, _ChannelId, _Drains, _Msg, false = _DrainAccepting) ->
    logplex_stats:incr("dropped_stat_key"),
    logplex_realtime:incr("dropped_stat_key"),
    ok;

process_drains(AppId, ChannelId, Drains, Msg, true = _DrainAccepting) ->
    [logplex_queue:in(logplex_drain_buffer, {TcpDrain, AppId, ChannelId, Token, Host, Port, Msg}) ||
        #drain{token=Token, resolved_host=Host, port=Port, tcp=TcpDrain} <- Drains],
    ok.

process_tails(ChannelId, Msg) ->
    logplex_tail:route(ChannelId, Msg),
    ok.

process_msg(ChannelId, BufferPid, Msg) ->
    logplex_queue:in(BufferPid, redis_helper:build_push_msg(ChannelId, ?LOG_HISTORY, Msg)),
    ok.
