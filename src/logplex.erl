-module(logplex).

-export([route/2]).

route(Token, Msg) ->
    Props = logplex_token:lookup(Token),
    ChannelId = proplists:get_value(channel_id, Props),
    Msg1 = re:replace(Msg, Token, proplists:get_value(token_name, Props, "")),
    Msg2 = iolist_to_binary(Msg1),
    logplex_channel:push(ChannelId, Msg2),
    logplex_tail:route(ChannelId, Msg2),
    [logplex_drain:route(Host, Port, Msg2) || [_Channel, {host, Host}, {port, Port}] <- logplex_channel:drains(ChannelId)],
    logplex_stats:incr(message_processed),
    ok.