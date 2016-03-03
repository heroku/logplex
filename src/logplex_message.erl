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
         ,process_error/5
         ,process_error/4
         ,parse_msg/1
        ]).

-include("logplex.hrl").
-include("logplex_error.hrl").
-include("logplex_logging.hrl").
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
    [ process_msg(RawMsg, ShardInfo) || RawMsg <- Msgs ].

process_msg({malformed, _Msg}, _ShardInfo) ->
    logplex_stats:incr(message_received_malformed),
    logplex_realtime:incr('message.received-malformed'),
    {error, malformed_msg};

process_msg({msg, RawMsg}, ShardInfo) ->
    case parse_msg(RawMsg) of
        {ok, TokenId} ->
            case logplex_token:lookup(TokenId) of
                undefined ->
                    logplex_realtime:incr(unknown_token),
                    ?INFO("at=process_msg token_id=~p msg=unknown_token", [TokenId]),
                    {error, invalid_token};
                Token ->
                    ChannelId = logplex_token:channel_id(Token),
                    TokenName = logplex_token:name(Token),
                    process_msg(RawMsg, ChannelId, TokenId,
                                TokenName, ShardInfo)
            end;
        _ ->
            logplex_stats:incr(message_received_malformed),
            logplex_realtime:incr('message.received-malformed'),
            {error, malformed_msg}
    end.

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
    logplex_realtime:incr('message.received'),
    case logplex_channel:lookup_flag(no_redis, ChannelId) of
        not_found ->
            logplex_realtime:incr(unknown_channel),
            ?INFO("at=process_msg channel_id=~p msg=unknown_channel", [ChannelId]);
        Flag ->
            CookedMsg = iolist_to_binary(re:replace(RawMsg, Token, TokenName)),
            logplex_firehose:post_msg(ChannelId, TokenName, RawMsg),
            process_drains(ChannelId, CookedMsg),
            process_tails(ChannelId, CookedMsg),
            process_redis(ChannelId, ShardInfo, CookedMsg, Flag)
    end.

process_error(ChannelID, Origin, ?L14, Fmt, Args) ->
    process_error(ChannelID, Origin, ["Error L14 (certificate validation): ", Fmt], Args).

process_error(ChannelID, Origin, Fmt, Args) when is_list(Fmt), is_list(Args) ->
    HerokuToken = logplex_token:lookup_heroku_token(ChannelID),
    HerokuOrigin = <<"heroku">>,
    do_process_error({HerokuToken#token.id, HerokuOrigin}, ChannelID, Origin, Fmt, Args).

do_process_error({HerokuToken, HerokuOrigin}, ChannelID, Origin, Fmt, Args) when is_binary(HerokuToken) ->
    Msg = logplex_syslog_utils:fmt(local7,
                                   warning,
                                   now,
                                   HerokuToken,
                                   "logplex",
                                   iolist_to_binary(Fmt),
                                   Args),
    RawMsg =  iolist_to_binary(logplex_syslog_utils:to_msg(Msg, Origin)),
    logplex_firehose:post_msg(ChannelID, HerokuOrigin, RawMsg),

    case logplex_channel:lookup_flag(no_redis, ChannelID) of
        not_found ->
            ignore;
        Flag ->
            CookedMsg = iolist_to_binary(re:replace(RawMsg, HerokuToken, HerokuOrigin)),
            ShardInfo = shard_info(),
            process_tails(ChannelID, CookedMsg),
            process_redis(ChannelID, ShardInfo, CookedMsg, Flag)
    end;

do_process_error({false, _HerokuOrigin}, _ChannelID, _Origin, Fmt, Args) ->
    ErrMsg = io_lib:format(iolist_to_binary(Fmt), Args),
    ?INFO("at=process_error channel_id=~s err=\"~s\"",
          [channel_id, ErrMsg]),
    ignored.

process_drains(ChannelID, Msg) ->
    logplex_channel:post_msg({channel, ChannelID}, Msg).

process_tails(ChannelId, Msg) ->
    logplex_tail:route(ChannelId, Msg).

process_redis(_ChannelId, _ShardInfo, _Msg, no_redis) ->
    ok;
process_redis(ChannelId, ShardInfo, Msg, _Flag) ->
    Expiry = logplex_app:config(redis_buffer_expiry),
    HistorySize = logplex_app:config(log_history),
    {Map, Interval} = logplex_shard_info:map_interval(ShardInfo),
    BufferPid = logplex_shard:lookup(integer_to_list(ChannelId),
                                     Map, Interval),
    Cmd = redis_helper:build_push_msg(ChannelId, HistorySize,
                                      Msg, Expiry),
    logplex_queue:in(BufferPid, Cmd).

parse_msg(Msg) ->
    case re:split(Msg, <<" +">>,
                  [{parts, 5}]) of
        [_PriVal, _Timestamp, _Host, Token = <<"t.", _/binary>>, _Rest] ->
            {ok, Token};
        _ ->
            {error, token_not_found}
    end.
