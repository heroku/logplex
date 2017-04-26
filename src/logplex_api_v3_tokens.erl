-module(logplex_api_v3_tokens).

-include("logplex.hrl").
-include("logplex_channel.hrl").
-include("logplex_logging.hrl").


-export([init/3,
         rest_init/2,
         service_available/2,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         resource_exists/2,
         content_types_provided/2,
         to_json/2,
         content_types_accepted/2,
         from_json/2
        ]).

-record(state, {
          channel_id :: logplex_channel:id(),
          token_name :: undefined | logplex_token:name()
         }).

%% @private
init(_Transport, Req, Opts) ->
    Route = proplists:get_value(route, Opts),
    Req1 = logplex_api_v3:prepare(Route, Req),
    {upgrade, protocol, cowboy_rest, Req1, Opts}.

%% @private
rest_init(Req, _Opts) ->
    {ChannelId, Req1} = cowboy_req:binding(channel_id, Req),
    State = #state{ channel_id = ChannelId },
    {ok, Req1, State}.

%% @private
service_available(Req, State) ->
    logplex_api_v3:service_available(Req, State).

%% @private
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% @private
malformed_request(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    try jsx:decode(Body, [return_maps]) of
        Map ->
            case validate_payload(Map) of
                {ok, TokenName} ->
                    NewState = State#state{ token_name = TokenName },
                    {false, Req1, NewState};
                {error, _Reason} ->
                    {true, Req, State}
            end
    catch
        error:badarg ->
            %% invalid json
            {true, Req1, State}
    end.

%% @private
is_authorized(Req, State) ->
    logplex_api_v3:is_authorized(Req, State).

%% @private
resource_exists(Req, #state{ channel_id = ChannelId } = State) ->
    %% we need to poll here to wait for nsync replication to catch up
    Timeout = logplex_app:config(default_redis_poll_ms, 2000),
    case logplex_channel:poll(ChannelId, Timeout) of
        Channel when is_record(Channel, channel) ->
            {true, Req, State};
        {error, timeout} ->
            %% channel was not found
            {ok, Req1} = cowboy_req:reply(404, Req),
            {halt, Req1, State}
    end.


%% @private
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

%% @private
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

%% @private
to_json(_, _) ->
    %% function is ignored
    {error, not_implemented}.

%% @private
from_json(Req, #state{ channel_id = ChannelId,
                       token_name = TokenName } = State) ->
    case logplex_token:create(ChannelId, TokenName) of
        Token when is_binary(Token) ->
            Resp = jsx:encode([{<<"token">>, Token}, {<<"name">>, TokenName}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {true, Req1, State};
        {error, Reason} ->
            ?INFO("at=from_json channel_id=~s token_name=~s "
                  "error=failed_to_create_token context=~p",
                  [ChannelId, TokenName, Reason]),
            Resp = jsx:encode([{<<"error">>, <<"failed to create token">>}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {ok, Req2} = cowboy_req:reply(500, Req1),
            {halt, Req2, State}
    end.

validate_payload(#{ <<"name">> := TokenName }) when is_binary(TokenName) ->
    {ok, TokenName};
validate_payload(_) ->
    {error, invalid_payload}.
