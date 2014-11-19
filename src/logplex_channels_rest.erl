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
            ?INFO("In allowed_methods: Req=~w State=~w", [Req, State]),
            {[<<"GET">>, <<"DELETE">>], Req2, ChannelId}
    end.

is_authorized(Req, State) ->
    logplex_rest:is_authorized(Req, State).

content_types_accepted(Req, State) ->
    {[{'*', from_json}],
     Req, State}.

content_types_provided(Req, State) ->
    ?INFO("In content_types_provided: Req=~w State=~w", [Req, State]),
    {[{<<"application/json">>, to_json}],
     Req, State}.

resource_exists(Req, ChanId) ->
    ?INFO("ChanId is ~w", [ChanId]),
    case ChanId of
        undefined ->
            ?INFO("ChanId is UNDEFINED", []),
            {true, Req, ChanId};
        ChanId ->
            case logplex_channel:lookup(ChanId) of
                undefined ->
                    ?INFO("Resource DOES NOT exist", []),
                    {false, Req, not_found};
                Channel ->
                    ?INFO("Resource DOES exist", []),
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
    {struct, Params} = mochijson2:decode(Body),
    ChannelId = logplex_channel:create_id(),
    case is_integer(ChannelId) of
        true ->
            _Tokens =
                case proplists:get_value(<<"tokens">>, Params) of
                    List when length(List) > 0 ->
                        [{TokenName, logplex_token:create(ChannelId, TokenName)}
                         || TokenName <- List];
                    _ ->
                        []
                end,
            ChanIdStr = integer_to_list(ChannelId),
            ?INFO("ChannelID=~w, ChanIdStr=~w", [ChannelId, ChanIdStr]),
            {{true, "/v3/channels/" ++ ChanIdStr}, Req2, ok};
        false ->
            ?INFO("ChannelID=~w", [ChannelId]),
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
        {ChanId, Tokens, _Drains} ->
            Info = [{channel_id, ChanId},
                    {tokens, [logplex_token:json_encode(Token) || Token <- Tokens]}],
            Req2 = cowboy_req:set_resp_body(
                     mochijson2:encode({struct, Info}), Req),
            {ok, Req3} = cowboy_req:reply(<<"200 OK">>, Req2),
            {halt, Req3, no_state}
    end.
