-module(logplex).
-export([route/2]).

-include_lib("logplex.hrl").

route(Token, Msg) ->
    case logplex_token:lookup(Token) of
        #token{channel_id=ChannelId, name=TokenName, addon=Addon} ->
            Msg1 = re:replace(Msg, Token, TokenName),
            Msg2 = iolist_to_binary(Msg1),
            logplex_channel:push(ChannelId, Addon, Msg2),
            logplex_tail:route(ChannelId, Msg2),
            [logplex_drain_pool:route(Host, Port, Msg2) || [_Channel, {host, Host}, {port, Port}] <- logplex_channel:drains(ChannelId)],
            logplex_stats:incr(message_processed);
        _ ->
            ok
    end.