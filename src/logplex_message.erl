%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Logplex Log Message Handling Functions
%% @end
-module(logplex_message).

-export([process_msg/4
         ,process_msg/5
         ,process_msgs/4
        ]).

-include("logplex.hrl").
-define(SI_KEY, logplex_redis_buffer_map).

shard_info() ->
    logplex_shard_info:read(?SI_KEY).

process_msgs(Msgs, ChannelId, Token, TokenName) ->
    ShardInfo = shard_info(),
    [ process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo)
      || RawMsg <- Msgs ],
    ok.

process_msg(RawMsg, ChannelId, Token, TokenName) ->
    process_msg(RawMsg, ChannelId, Token, TokenName,
                shard_info()).

process_msg({msg, RawMsg}, ChannelId, Token, TokenName, ShardInfo)
  when is_binary(RawMsg) ->
    process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo);
process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo)
  when is_binary(RawMsg) ->
    CookedMsg = iolist_to_binary(re:replace(RawMsg, Token, TokenName)),
    process_drains(ChannelId, CookedMsg),
    process_tails(ChannelId, CookedMsg),
    process_redis(ChannelId, ShardInfo, CookedMsg).

process_drains(ChannelID, Msg) ->
    logplex_channel:post_msg({channel, ChannelID}, Msg).

process_tails(ChannelId, Msg) ->
    logplex_tail:route(ChannelId, Msg).

process_redis(ChannelId, ShardInfo, Msg) ->
    case logplex_channel:lookup_flag(no_redis, ChannelId) of
        no_redis -> ok;
        _ ->
            {Map, Interval} = logplex_shard_info:map_interval(ShardInfo),
            BufferPid = logplex_shard:lookup(integer_to_list(ChannelId),
                                             Map, Interval),
            Cmd = redis_helper:build_push_msg(ChannelId, ?LOG_HISTORY, Msg),
            logplex_queue:in(BufferPid, Cmd)
    end.
