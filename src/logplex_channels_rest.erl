%% @copyright Heroku
%% @author Andrew Gwozdziewycz <apg@heroku.com>
%% @version {@vsn}, {@date} {@time}
%% @doc Handler for the /channels (but not drains) endpoints
%% @end

-module(logplex_channels_rest).

-include("logplex_logging.hrl").
-include("logplex_channel.hrl").

-export([init/3
        , allowed_methods/2
        , is_authorized/2
        , content_types_accepted/2
        , content_types_provided/2
        , to_json/2
        , resource_exists/2
        , delete_resource/2
        , from_json/2]).

init(_Transport, _Req, _) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {ChannelId, Req2} = cowboy_req:binding(chan_id, Req),
    case ChannelId of
        undefined ->
            {[<<"POST">>], Req2, ChannelId};
        _ ->
            {[<<"GET">>, <<"DELETE">>], Req2, ChannelId}
    end.

is_authorized(Req, State) ->
    logplex_rest:is_authorized(Req, State).

content_types_accepted(Req, State) ->
    {[{'*', from_json}],
     Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}],
     Req, State}.

resource_exists(Req, ChanId) ->
    case ChanId of
        undefined ->
            {true, Req, ChanId};
        ChanId ->
            case logplex_channel:lookup(ChanId) of
                undefined ->
                    {false, Req, not_found};
                Channel ->
                    {true, Req, Channel}
            end
    end.

delete_resource(Req, #channel{id=ChannelId}) ->
    case logplex_channel:delete(ChannelId) of
        ok ->
            {true, Req, deleted};
        {error, not_found} ->
            {false, Req, not_found}
    end.

from_json(Req, _State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    {struct, Params} = mochijson2:decode(Body), %% TODO(apg) Don't blow up and 500 on error of this.
    ChannelId = logplex_channel:create_id(),
    case is_integer(ChannelId) of
        true ->
            Tokens =
                case proplists:get_value(<<"tokens">>, Params) of
                    List when length(List) > 0 ->
                        [{TokenName, logplex_token:create(ChannelId, TokenName)}
                         || TokenName <- List];
                    _ ->
                        []
                end,
            Info = mochijson2:encode({struct, [{channel_id, ChannelId}, {tokens, Tokens}]}),
            Req3 = cowboy_req:set_resp_body(Info, Req2),
            {true, Req3, ok};
        false ->
            {false, Req2, expected_integer}
    end.

to_json(Req, not_found) ->
    Req2 = cowboy_req:set_resp_body(mochijson2:encode({struct, [{error, "Not Found"}]}), Req),
    {ok, Req3} = cowboy_req:reply(<<"404 Not Found">>, Req2),
    {halt, Req3, no_state};

to_json(Req, #channel{id=ChannelId}) ->
    case logplex_channel:info(ChannelId) of
        not_found ->
            to_json(Req, not_found);
        {ChanId, Tokens, Drains} ->
            Info = [{channel_id, ChanId},
                    {tokens, [logplex_token:json_encode(Token) || Token <- Tokens]},
                    {drains, [logplex_drain:json_encode(Drain) || Drain <- Drains]}],
            {mochijson2:encode({struct, Info}), Req, no_state}
    end.
