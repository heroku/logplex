%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Logplex Shell functions
%% @end
-module(logplex).

-export([serialize_from_token/1]).
-export([serialize_channel/1
         ,deserialize_channel/1
        ]).

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

deserialize_channel({Chan,
                     Tokens,
                     Drains}) ->
    {logplex_channel:store(Chan),
     [ logplex_token:store(Token)
       || Token <- Tokens ],
     [ logplex_drain:store(Drain)
       || Drain <- Drains ]}.
