%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Logplex Log Message Handling Functions
%% @end
-module(logplex_message).

-export([process_msgs/4
         ,process_msgs/1
         ,process_error/5
         ,process_error/4
        ]).

-include("logplex.hrl").
-include("logplex_error.hrl").
-include("logplex_logging.hrl").
-define(SI_KEY, logplex_redis_buffer_map).


%% ----------------------------------------------------------------------------
%% API Functions
%% ----------------------------------------------------------------------------

process_msgs(Msgs, ChannelId, Token, TokenName) when is_list(Msgs) ->
    case logplex_app:config(batch_redis, false) of
        true ->
            process_msgs_batch_redis(Msgs, ChannelId, Token, TokenName);
        _ ->
            process_msgs_classic(Msgs, ChannelId, Token, TokenName)
    end.

process_msgs_classic(Msgs, ChannelId, Token, TokenName) ->
    ShardInfo = shard_info(),
    [ process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo)
      || RawMsg <- Msgs ],
    ok.

process_msgs_batch_redis(Msgs, ChannelId, Token, TokenName) when is_list(Msgs) ->
    RawMsgs = get_raw_msgs(Msgs),
    logplex_stats:incr(message_received, length(RawMsgs)),
    logplex_realtime:incr('message.received', length(RawMsgs)),
    case logplex_channel:lookup_flag(no_redis, ChannelId) of
        not_found ->
            logplex_realtime:incr(unknown_channel, length(RawMsgs)),
            ?INFO("at=process_msgs channel_id=~s msg=unknown_channel", [ChannelId]),
            ok;
        Flag ->
            [logplex_firehose:post_msg(ChannelId, TokenName, RawMsg) || RawMsg <- RawMsgs],
            CookedMsgs = [ iolist_to_binary(re:replace(RawMsg, Token, TokenName)) || RawMsg <- RawMsgs],

            [process_drains(ChannelId, CookedMsg, logplex_app:config(deny_drain_forwarding, false)) || CookedMsg <- CookedMsgs],
            [process_tails(ChannelId, CookedMsg, logplex_app:config(deny_tail_sessions, false)) || CookedMsg <- CookedMsgs],

            process_redis_batch(ChannelId, CookedMsgs, logplex_app:config(deny_redis_buffers, Flag)),
            ok
    end.


process_msgs(Msgs) ->
    ShardInfo = shard_info(),
    [ process_msg(RawMsg, ShardInfo) || RawMsg <- Msgs ].

process_msg({malformed, _Msg}, _ShardInfo) ->
    logplex_stats:incr(message_received_malformed),
    logplex_realtime:incr('message.received-malformed'),
    {error, malformed_msg};

process_msg({msg, RawMsg}, ShardInfo) ->
    case extract_token(RawMsg) of
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

process_msg({msg, RawMsg}, ChannelId, Token, TokenName, ShardInfo)
  when is_binary(RawMsg) ->
    process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo);
process_msg(RawMsg, ChannelId, Token, TokenName, ShardInfo)
  when is_binary(RawMsg),
       is_binary(ChannelId),
       is_binary(Token),
       is_binary(TokenName) ->
    logplex_stats:incr(message_received),
    logplex_realtime:incr('message.received'),
    case logplex_channel:lookup_flag(no_redis, ChannelId) of
        not_found ->
            logplex_realtime:incr(unknown_channel),
            ?INFO("at=process_msg channel_id=~s msg=unknown_channel", [ChannelId]);
        Flag ->
            CookedMsg = iolist_to_binary(re:replace(RawMsg, Token, TokenName)),
            logplex_firehose:post_msg(ChannelId, TokenName, RawMsg),
            process_drains(ChannelId, CookedMsg, logplex_app:config(deny_drain_forwarding, false)),
            process_tails(ChannelId, CookedMsg, logplex_app:config(deny_tail_sessions, false)),
            process_redis(ChannelId, ShardInfo, CookedMsg, logplex_app:config(deny_redis_buffers, Flag))
    end.

process_error(ChannelID, Origin, ?L14, Fmt, Args) ->
    process_error(ChannelID, Origin, ["Error L14 (certificate validation): ", Fmt], Args).

process_error(ChannelID, Origin, Fmt, Args) when is_list(Fmt), is_list(Args) ->
    #token{ id=HerokuToken } = logplex_token:lookup_heroku_token(ChannelID),
    HerokuOrigin = <<"heroku">>,
    do_process_error({HerokuToken, HerokuOrigin}, ChannelID, Origin, Fmt, Args).

%% ----------------------------------------------------------------------------
%% internal functions
%% ----------------------------------------------------------------------------

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
            process_tails(ChannelID, CookedMsg, logplex_app:config(deny_tail_sessions, false)),
            process_redis(ChannelID, ShardInfo, CookedMsg, logplex_app:config(deny_redis_buffers, Flag))
    end;

do_process_error({false, _HerokuOrigin}, _ChannelID, _Origin, Fmt, Args) ->
    ErrMsg = io_lib:format(iolist_to_binary(Fmt), Args),
    ?INFO("at=process_error channel_id=~s err=\"~s\"",
          [channel_id, ErrMsg]),
    ignored.

process_drains(ChannelID, Msg, true) ->
    ok;
process_drains(ChannelID, Msg, false) ->
    logplex_channel:post_msg({channel, ChannelID}, Msg).

process_tails(ChannelId, Msg, true) ->
    ok;
process_tails(ChannelId, Msg, false) ->
    logplex_tail:route(ChannelId, Msg).

process_redis(_ChannelId, _ShardInfo, _Msg, Flag) when Flag =:= no_redis;
                                                       Flag =:= true ->
    ok;
process_redis(ChannelId, ShardInfo, Msg, _Flag) ->
    Expiry = logplex_app:config(redis_buffer_expiry),
    HistorySize = logplex_app:config(log_history),
    {Map, Interval} = logplex_shard_info:map_interval(ShardInfo),
    BufferPid = logplex_shard:lookup(binary_to_list(ChannelId),
                                     Map, Interval),
    Cmd = redis_helper:build_push_msg(ChannelId, HistorySize,
                                      Msg, Expiry),
    logplex_queue:in(BufferPid, Cmd).

process_redis_batch(_ChannelId, _Msgs, Flag) when Flag =:= no_redis;
                                                  Flag =:= true ->
    ok;
process_redis_batch(ChannelId, Msgs, _Flag) ->
    ShardInfo = shard_info(),
    Expiry = logplex_app:config(redis_buffer_expiry),
    HistorySize = list_to_binary(integer_to_list(logplex_app:config(log_history))),
    {Map, Interval} = logplex_shard_info:map_interval(ShardInfo),
    BufferPid = logplex_shard:lookup(binary_to_list(ChannelId),
                                     Map, Interval),
    Cmd = redis_helper:build_push_batch_msgs(ChannelId, HistorySize,
                                             Msgs, Expiry),
    logplex_queue:in(BufferPid, Cmd).

%% ----------------------------------------------------------------------------
%% helper functions
%% ----------------------------------------------------------------------------

shard_info() ->
    logplex_shard_info:read(?SI_KEY).

get_raw_msgs(Msgs) ->
    [ case Msg of
          {msg, RawMsg} -> RawMsg;
          RawMsg -> RawMsg
      end || Msg <- Msgs ].

extract_token(Msg) ->
    case re:split(Msg, <<" +">>,
                  [{parts, 5}]) of
        [_PriVal, _Timestamp, _Host, Token = <<"t.", _/binary>>, _Rest] ->
            {ok, Token};
        _ ->
            {error, token_not_found}
    end.
