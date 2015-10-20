%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Logplex Shell functions
%% @end
-module(logplex).

-export([serialize_from_token/1]).
-export([serialize_channel/1,
         deserialize_channel/1]).

-export([drain_dests/0]).

-export([debug_to_channel/3,
         post_to_channel/4]).

serialize_from_token(TokenId) when is_binary(TokenId) ->
    case logplex_token:lookup(TokenId) of
        undefined ->
            {error, no_such_token};
        Token ->
            serialize_channel(logplex_token:channel_id(Token))
    end.

serialize_channel(ChannelId) when is_integer(ChannelId) ->
    {logplex_channel:lookup(ChannelId),
     logplex_token:lookup_by_channel(ChannelId),
     logplex_drain:lookup_by_channel(ChannelId)}.

%% seems unused
deserialize_channel({Chan,
                     Tokens,
                     Drains}) ->
    {logplex_channel:store(Chan),
     [ logplex_token:store(Token)
       || Token <- Tokens ],
     [ logplex_drain:store(Drain)
       || Drain <- Drains ]}.

drain_dests() ->
    logplex_drain:by_dest().

debug_to_channel(ChannelId, Fmt, Args) when is_integer(ChannelId) ->
    logplex_channel:post_msg({channel, ChannelId},
                             logplex_syslog_utils:fmt('user', 'debug', now,
                                                      "erlang", "shell",
                                                      Fmt, Args)).

post_to_channel(ChannelId, SyslogMsg, Token, TokenName) when is_integer(ChannelId), is_tuple(SyslogMsg) ->
  post_to_channel(ChannelId, iolist_to_binary(logplex_syslog_utils:to_msg(SyslogMsg)), Token, TokenName);
post_to_channel(ChannelId, SyslogMsg, Token, TokenName)
  when is_integer(ChannelId),
       is_binary(SyslogMsg),
       is_binary(Token),
       is_binary(TokenName) ->
  logplex_message:process_msgs([SyslogMsg], ChannelId, Token, TokenName).
