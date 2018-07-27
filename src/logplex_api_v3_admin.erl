-module(logplex_api_v3_admin).

-include("logplex.hrl").
-include("logplex_channel.hrl").
-include("logplex_logging.hrl").


-export([init/3,
         rest_init/2,
         service_available/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_accepted/2,
         from_json/2
        ]).

-record(state, {
          channel_ids :: [logplex_channel:id()]
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
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

%% @private
from_json(Req, #state{} = State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    try jsx:decode(Body, [return_maps]) of
        Map ->
            case validate_payload(Map) of
                {ok, ChannelIds} ->
                    logplex_channel:flag_no_redis(ChannelIds),
                    Resp = jsx:encode([{<<"success">>, <<"channels flagged as no_redis">>}]),
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

validate_payload(#{<<"channel_ids">> := ChannelIds}) ->
    true = lists:all(fun is_binary/1, ChannelIds),
    {ok, lists:usort(ChannelIds)};
validate_payload(_) ->
    {error, invalid_payload}.
