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

-export([init_state/0, handle_message/2, route/3]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-record(state, {regexp,
                shard_info :: logplex_shard_info:shard_info()}).

-define(SI_KEY, logplex_redis_buffer_map).

%% API functions
start_link(_QueuePid) ->
    proc_lib:start_link(?MODULE, init, [self()], 5000).

init(Parent) ->
    ?INFO("at=init parent=~p", [Parent]),
    State = init_state(),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(State).

init_state() ->
    SInfo = logplex_shard_info:read(?SI_KEY),
    {ok, RE} = re:compile("^<\\d+>\\S+ \\S+ \\S+ (t[.]\\S+) "),
    #state{regexp=RE, shard_info = SInfo}.

%% Temporary code upgrade path.
loop({state, RE, _Map, _Interval}) ->
    SInfo = logplex_shard_info:read(logplex_redis_buffer_map),
    loop(#state{regexp=RE, shard_info=SInfo});

loop(#state{} = State) ->
    case catch logplex_queue:out(logplex_work_queue) of
        timeout ->
            ?MODULE:loop(State);
        {'EXIT', _} ->
            normal;
        {1, [Msg]} ->
            {ok, NewState} = handle_message(Msg, State),
            ?MODULE:loop(NewState)
    end.

handle_message(Msg, State = #state{regexp = RE}) ->
    case re:run(Msg, RE, [{capture, all_but_first, binary}]) of
        {match, [Token]} ->
            NewState = maybe_update_cache(State),
            route(Token, NewState, Msg),
            {ok, NewState};
        _ ->
            K = #logplex_stat{module=?MODULE,
                              key=msg_drop_missing_token},
            logplex_stats:incr(K),
            {ok, State}
    end.

maybe_update_cache(S = #state{shard_info = SI}) ->
    NewSI = logplex_shard_info:cached_read(?SI_KEY, SI),
    S#state{shard_info=NewSI}.

map_interval(#state{shard_info=SI}) ->
    logplex_shard_info:map_interval(SI).

route(Token, State = #state{}, RawMsg)
  when is_binary(Token), is_binary(RawMsg) ->
    case logplex_token:lookup(Token) of
        #token{channel_id=ChannelId, name=TokenName} ->
            CookedMsg = iolist_to_binary(re:replace(RawMsg, Token, TokenName)),
            logplex_firehose:post_msg(ChannelId, TokenName, RawMsg),
            process_drains(ChannelId, CookedMsg),
            process_tails(ChannelId, CookedMsg),
            process_msg(ChannelId, State, CookedMsg);
        _ ->
            Key = case logplex_app:config(log_unknown_tokens, false) of
                      false -> msg_drop_unknown_token;
                      _ -> {msg_drop_unknown_token, Token}
                  end,
            K = #logplex_stat{module=?MODULE, key=Key},
            logplex_stats:incr(K),
            ok
    end.

process_drains(ChannelID, Msg) ->
    logplex_channel:post_msg({channel, ChannelID}, Msg).

process_tails(ChannelId, Msg) ->
    logplex_tail:route(ChannelId, Msg),
    ok.

process_msg(ChannelId, State, Msg) ->
    case logplex_channel:lookup_flag(no_redis, ChannelId) of
        no_redis -> ok;
        _ ->
            Expiry = logplex_app:config(redis_buffer_expiry),
            HistorySize = logplex_app:config(log_history),
            {Map, Interval} = map_interval(State),
            BufferPid = logplex_shard:lookup(integer_to_list(ChannelId),
                                             Map, Interval),
            Cmd = redis_helper:build_push_msg(ChannelId, HistorySize,
                                              Msg, Expiry),
            logplex_queue:in(BufferPid, Cmd)
    end,
    ok.
