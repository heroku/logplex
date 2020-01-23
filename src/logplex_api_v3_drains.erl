-module(logplex_api_v3_drains).

-include("logplex.hrl").
-include("logplex_channel.hrl").
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").

-export([init/3,
         rest_init/2,
         service_available/2,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         resource_exists/2,
         is_conflict/2,
         content_types_accepted/2,
         from_json/2,
         content_types_provided/2,
         to_json/2,
         delete_resource/2
        ]).

-record(state, {
          channel_id :: binary(),
          drain_id   :: undefined | logplex_drain:id(),
          drain      :: undefined | logplex_drain:drain(),
          uri        :: undefined | #ex_uri{} %% optional json url parameter
         }).



%% @private
init(_Transport, Req, Opts) ->
    Route = proplists:get_value(route, Opts),
    Req1 = logplex_api_v3:prepare(Route, Req),
    {upgrade, protocol, cowboy_rest, Req1, Opts}.

%% @private
rest_init(Req, _Opts) ->
    {ChannelId, Req1} = cowboy_req:binding(channel_id, Req),
    case cowboy_req:binding(drain_id, Req1) of
        {undefined, Req2} ->
            State = #state{ channel_id = ChannelId },
            {ok, Req2, State};
        {DrainId, Req2} ->
            State = #state{ channel_id = ChannelId,
                            drain_id   = DrainId },
            {ok, Req2, State}
    end.

%% @private
service_available(Req, State) ->
    logplex_api_v3:service_available(Req, State).

%% @private
allowed_methods(Req, #state{ drain_id = undefined } = State) ->
    %% No drain id on path, hence this is a POST request to create a new drain.
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    %% We have a drain id, hence this is an update request to a drain resource.
    {[<<"PUT">>, <<"DELETE">>], Req, State}.

%% @private
malformed_request(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    case cowboy_req:has_body(Req1) of
        false when Method == <<"DELETE">> ->
            {false, Req1, State};
        true ->
            {ok, Body, Req2} = cowboy_req:body(Req1),
            try jsx:decode(Body, [return_maps]) of
                Map ->
                    case validate_payload(Map) of
                        {ok, URI} ->
                            NewState = State#state{ uri = URI },
                            {false, Req2, NewState};
                        {error, _Reason} ->
                            {true, Req2, State}
                    end
            catch
                error:badarg ->
                    %% invalid json
                    {true, Req2, State}
            end
    end.

%% @private
is_authorized(Req, State) ->
    logplex_api_v3:is_authorized(Req, State).

%% @private
resource_exists(Req, #state{ channel_id = ChannelId,
                             drain_id   = undefined } = State) ->
    %% this is a POST
    case logplex_channel:find(ChannelId) of
        {ok, _Channel} ->
            %% we return false here because we're attempting to create a new resource
            {false, Req, State};
        {error, not_found} ->
            %% channel was not found
            {ok, Req1} = cowboy_req:reply(404, Req),
            {halt, Req1, State}
    end;
resource_exists(Req, #state{ channel_id = ChannelId,
                             drain_id   = DrainId } = State) ->

    case logplex_channel:find(ChannelId) of
        {ok, _Channel} ->
            case logplex_drain:find(DrainId) of
                {ok, Drain} ->
                    NewState = State#state{ drain = Drain },
                    {true, Req, NewState};
                {error, timeout} ->
                    ?WARN("at=resource_exists channel_id=~s drain_id=~s error=drain_find_timeout",
                    [ChannelId, DrainId]),
                    {ok, Req2} = cowboy_req:reply(500, Req),
                    {halt, Req2, State};
                {error, not_found} ->
                    %% drain was not found for channel
                    {false, Req, State}
            end;
        {error, timeout} ->
            ?WARN("at=resource_exists channel_id=~s error=channel_find_timeout", [ChannelId]),
            {ok, Req2} = cowboy_req:reply(500, Req),
            {halt, Req2, State};
        {error, not_found} ->
            %% channel was not found
            {ok, Req1} = cowboy_req:reply(404, Req),
            {halt, Req1, State}
    end.

%% @private
is_conflict(Req, #state{ uri = undefined} = State) ->
    %% no url parameter given, no conflict possible
    {false, Req, State};
is_conflict(Req, #state{ channel_id = ChannelId,
                         uri        = URI } = State) ->
    Result = logplex_drain:exists(ChannelId, URI),
    {Result, Req, State}.

%% @private
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

%% @private
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

%% @private
delete_resource(Req, #state{ drain_id = DrainId } = State) ->
    ok = logplex_drain:delete(DrainId),
    {true, Req, State}.

%% @private
to_json(_, _) ->
    %% function is ignored
    {error, not_implemented}.

%% @private
from_json(Req, #state{ channel_id = ChannelId,
                       drain_id   = undefined,
                       uri        = URI} = State) ->
    %% This is a POST:
    case logplex_channel:can_add_drain(ChannelId) of
        cannot_add_drain ->
            Resp = jsx:encode([{<<"error">>, <<"maximum number of drains reached">>}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {ok, Req2} = cowboy_req:reply(422, Req1),
            {halt, Req2, State};
        can_add_drain ->
            %% Reserving drain in order to ensure DrainId is propgated back to ETS.
            {ok, DrainId, Token} = logplex_drain:reserve_token(),
            ok = logplex_drain:cache(DrainId, Token, ChannelId),
            case URI of
                undefined ->
                    Resp = jsx:encode([{<<"id">>, DrainId}, {<<"token">>, Token}]),
                    Req1 = cowboy_req:set_resp_body(Resp, Req),
                    Location = get_location(ChannelId, DrainId),
                    {{true, Location}, Req1, State};
                _ ->
                    case handle_create_or_update(DrainId, Token, ChannelId, URI, Req) of
                        {ok, Req1} ->
                            Location = get_location(ChannelId, DrainId),
                            {{true, Location}, Req1, State};
                        {halt, Req1} ->
                            {halt, Req1, State}
                    end
            end
    end;
from_json(Req, #state{ channel_id = ChannelId,
                       drain_id   = DrainId,
                       drain      = Drain,
                       uri        = URI } = State) ->
    case Drain of
        undefined ->
            %% we must prevent creation of drains without prior reservation
            {ok, Req1} = cowboy_req:reply(404, Req),
            {halt, Req1, State};
        Drain when is_record(Drain, drain) ->
            Token = logplex_drain:token(Drain),
            case handle_create_or_update(DrainId, Token, ChannelId, URI, Req) of
                {ok, Req1} ->
                    {true, Req1, State};
                {halt, Req1} ->
                    {halt, Req1, State}
            end
    end.

handle_create_or_update(DrainId, Token, ChannelId, URI, Req) ->
    %% /shrug create and update are equivalent for drains
    case logplex_drain:create(DrainId, Token, ChannelId, URI) of
        {drain, _, Token} ->
            Resp = jsx:encode([{<<"id">>, DrainId},
                               {<<"token">>, Token},
                               {<<"url">>, uri_to_binary(URI)}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {ok, Req1};
        {error, already_exists} ->
            Resp = jsx:encode([{<<"error">>, <<"already exists">>}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {ok, Req2} = cowboy_req:reply(409, Req1),
            {halt, Req2};
        {error, Reason} ->
            ?INFO("at=from_json channel_id=~s drain_token=~s drain_id=~b "
                  "error=failed_to_create_drain context=~p",
                  [ChannelId, Token, DrainId, Reason]),
            Resp = jsx:encode([{<<"error">>, <<"failed to create drain">>}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {ok, Req2} = cowboy_req:reply(500, Req1),
            {halt, Req2}
    end.

validate_payload(#{ <<"url">> := URL }) ->
    case logplex_drain:parse_url(URL) of
        {error, Reason} ->
            {error, Reason};
        ExURI ->
            {ok, ExURI}
    end;
validate_payload(#{}) ->
    {ok, undefined};
validate_payload(_) ->
    {error, invalid_payload}.


get_location(ChannelId, DrainId) ->
    URL = logplex_app:config(http_v3_url),
    list_to_binary([URL, <<"/v3/channels/">>, ChannelId, <<"/drains/">>, integer_to_list(DrainId)]).


uri_to_binary(Uri) ->
    iolist_to_binary(ex_uri:encode(Uri)).
