-module(logplex_api_v3_channel_flags).

-include("logplex.hrl").
-include("logplex_channel.hrl").
-include("logplex_logging.hrl").


-export([init/3,
         rest_init/2,
         service_available/2,
         allowed_methods/2,
         is_authorized/2,
         resource_exists/2,
         delete_resource/2,
         content_types_provided/2,
         to_json/2,
         content_types_accepted/2,
         from_json/2
        ]).

-record(state, {
          channel_id :: logplex_channel:id(),
          flags      :: [logplex_channel:flag()]
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
    {[<<"PUT">>, <<"GET">>, <<"DELETE">>], Req, State}.

%% @private
is_authorized(Req, State) ->
    logplex_api_v3:is_authorized(Req, State).

%% @private
resource_exists(Req, #state{ channel_id = ChannelId } = State) ->
    case logplex_channel:find(ChannelId) of
        {ok, Channel} ->
            NewState = State#state{ channel_id = ChannelId,
                                    flags      = Channel#channel.flags },
            {true, Req, NewState};
        {error, not_found} ->
            %% channel was not found
            {false, Req, State}
    end.

%% @private
delete_resource(Req, #state{ channel_id = ChannelId } = State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    try jsx:decode(Body, [return_maps]) of
        Map ->
            case validate_payload(Map) of
                {ok, NewFlags} ->
                    DeserializedFlags = deserialize_flags(NewFlags),
                    {ok, UpdatedChannel} = logplex_channel:remove_flags(DeserializedFlags, ChannelId),
                    Resp = serialize_channel(UpdatedChannel),
                    Req2 = cowboy_req:set_resp_body(Resp, Req1),
                    {true, Req2, State};
                {error, invalid_payload} ->
                    Resp = jsx:encode([{<<"error">>, <<"invalid payload">>}]),
                    Req2 = cowboy_req:set_resp_body(Resp, Req1),
                    {false, Req2, State}
            end
    catch
        error:badarg ->
            %% invalid json
            {false, Req, State}
    end.

%% @private
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

%% @private
to_json(Req, #state{ channel_id = ChannelId } = State) ->
    case logplex_channel:find(ChannelId) of
        {ok, Channel} ->
            Resp = serialize_channel(Channel),
            {Resp, Req, State};
        {error, not_found} ->
            %% channel was not found
            {false, Req, State}
    end.

%% @private
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

%% @private
from_json(Req, #state{ channel_id = ChannelId } = State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    try jsx:decode(Body, [return_maps]) of
        Map ->
            case validate_payload(Map) of
                {ok, NewFlags} ->
                    DeserializedFlags = deserialize_flags(NewFlags),
                    {ok, UpdatedChannel} = logplex_channel:set_flags(DeserializedFlags, ChannelId),
                    Resp = serialize_channel(UpdatedChannel),
                    Req2 = cowboy_req:set_resp_body(Resp, Req1),
                    {true, Req2, State};
                {error, invalid_payload} ->
                    Resp = jsx:encode([{<<"error">>, <<"invalid payload">>}]),
                    Req2 = cowboy_req:set_resp_body(Resp, Req1),
                    {false, Req2, State}
            end
    catch
        error:badarg ->
            %% invalid json
            {false, Req, State}
    end.

-spec validate_payload(map()) -> {ok, [binary()]} | {error, term()}.
validate_payload(#{<<"flags">> := Flags}) ->
    true = lists:all(fun is_binary/1, Flags),
    {ok, lists:usort(Flags)};
validate_payload(#{}) ->
    {ok, []};
validate_payload(_) ->
    {error, invalid_payload}.

serialize_channel(Channel) ->
    jsx:encode([{<<"channel">>, Channel#channel.id}] ++
                serialize_flags(Channel#channel.flags)).

serialize_flags([]) -> [];
serialize_flags(Flags) ->
    [{<<"flags">>, lists:usort(Flags)}].

deserialize_flags(Flags) ->
    lists:flatten([ logplex_channel:binary_to_flags(F) || F <- Flags ]).