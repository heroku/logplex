-module(logplex_api_v3_sessions).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-export([init/3,
         rest_init/2,
         service_available/2,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         resource_exists/2,
         content_types_accepted/2,
         from_json/2,
         content_types_provided/2,
         to_json/2
        ]).

-record(state, {
          channel_id :: undefined | binary()
         }).

%% @private
init(_Transport, Req, Opts) ->
    Route = proplists:get_value(route, Opts),
    Req1 = logplex_api_v3:prepare(Route, Req),
    {upgrade, protocol, cowboy_rest, Req1, Opts}.

%% @private
rest_init(Req, _Opts) ->
    State = #state{},
    {ok, Req, State}.

%% @private
service_available(Req, State) ->
    logplex_api_v3:service_available(Req, State).

%% @private
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% @private
is_authorized(Req, State) ->
    logplex_api_v3:is_authorized(Req, State).

%% @private
resource_exists(Req, State) ->
    %% We return false here as we are creating a new resource
    {false, Req, State}.

%% @private
malformed_request(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    try jsx:decode(Body, [return_maps]) of
        Map ->
            case validate_payload(Map) of
                {ok, ChannelId} ->
                    NewState = State#state{ channel_id = ChannelId },
                    {false, Req1, NewState};
                {error, _Reason} ->
                    {true, Req1, State}
            end
    catch
        error:badarg ->
            %% invalid json
            {true, Req1, State}
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
from_json(Req, #state{ channel_id = ChannelId } = State) ->
    case logplex_channel:find(ChannelId) of
        {ok, _Channel} ->
            {ok, Body, Req1} = cowboy_req:body(Req),
            UUID = logplex_session:publish(Body),
            Location = iolist_to_binary([logplex_app:config(api_endpoint_url, ""), <<"/sessions/">>, UUID]),
            Resp = jsx:encode([{<<"url">>, Location}]),
            Req2 = cowboy_req:set_resp_body(Resp, Req1),
            {{true, Location}, Req2, State};
        {error, not_found} ->
            Resp = jsx:encode([{<<"error">>, <<"channel not found">>}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {ok, Req2} = cowboy_req:reply(422, Req1),
            {halt, Req2, State}
    end.

validate_payload(#{ <<"channel_id">> := ChannelId }) when is_binary(ChannelId) ->
    {ok, ChannelId};
validate_payload(_) ->
    {error, invalid_payload}.
