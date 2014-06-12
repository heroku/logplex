%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Logplex Log Message Handling Functions
%% @end
-module(logplex_message).

-export([process_msg/4
         ,process_msg/5
         ,process_msgs/4
         ,process_msgs/1
         ,parse_msg/1
        ]).

-include("logplex.hrl").
-define(SI_KEY, logplex_redis_buffer_map).

shard_info() ->
    logplex_shard_info:read(?SI_KEY).

process_msgs(Msgs, ChannelId, Token, TokenName) when is_list(Msgs) ->
    ShardInfo = shard_info(),
    [ process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo)
      || RawMsg <- Msgs ],
    ok.

process_msgs(Msgs) ->
    ShardInfo = shard_info(),
    [ case parse_msg(RawMsg) of
          {ok, TokenId} ->
              case logplex_token:lookup(TokenId) of
                  undefined -> {error, invalid_token};
                  Token ->
                      ChannelId = logplex_token:channel_id(Token),
                      TokenName = logplex_token:name(Token),
                      process_msg(RawMsg, ChannelId, TokenId,
                                  TokenName, ShardInfo)
              end;
          _ ->
              {error, malformed_msg}
      end
      || {msg, RawMsg} <- Msgs ].

process_msg(RawMsg, ChannelId, Token, TokenName) when not is_list(RawMsg) ->
    process_msg(RawMsg, ChannelId, Token, TokenName,
                shard_info()).

process_msg({msg, RawMsg}, ChannelId, Token, TokenName, ShardInfo)
  when is_binary(RawMsg) ->
    process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo);
process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo)
  when is_binary(RawMsg),
       is_integer(ChannelId),
       is_binary(Token),
       is_binary(TokenName) ->
    logplex_stats:incr(message_received),
    logplex_realtime:incr(message_received),
    CookedMsg = iolist_to_binary(re:replace(RawMsg, Token, TokenName)),
    logplex_firehose:post_msg(ChannelId, TokenName, RawMsg),
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
            Expiry = logplex_app:config(redis_buffer_expiry),
            HistorySize = logplex_app:config(log_history),
            {Map, Interval} = logplex_shard_info:map_interval(ShardInfo),
            BufferPid = logplex_shard:lookup(integer_to_list(ChannelId),
                                             Map, Interval),
            Cmd = redis_helper:build_push_msg(ChannelId, HistorySize,
                                              Msg, Expiry),
            logplex_queue:in(BufferPid, Cmd)
    end.

parse_msg(Msg) ->
    case re:split(Msg, <<" +">>,
                  [{parts, 5}]) of
        [_PriVal, _Timestamp, _Host, Token = <<"t.", _/binary>>, _Rest] ->
            {ok, Token};
        _ ->
            {error, token_not_found}
    end.
