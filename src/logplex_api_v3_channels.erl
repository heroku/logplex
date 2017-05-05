-module(logplex_api_v3_channels).

-include("logplex.hrl").
-include("logplex_channel.hrl").
-include("logplex_logging.hrl").

-export([init/3,
         rest_init/2,
         service_available/2,
         allowed_methods/2,
         is_authorized/2,
         resource_exists/2,
         content_types_provided/2,
         to_json/2,
         content_types_accepted/2,
         from_json/2,
         delete_resource/2
        ]).

-record(state, {
          channel_id :: binary(),
          channel :: undefined | logplex_channel:channel(),
          tokens = [] :: [logplex_token:token()],
          drains = [] :: [logplex_drain:drain()]
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
            {ChannelId, Tokens, Drains} = logplex_channel:info(ChannelId),
            NewState = State#state{ channel = Channel,
                                    tokens  = Tokens,
                                    drains  = Drains },
            {true, Req, NewState};
        {error, not_found} ->
            %% channel was not found
            {false, Req, State}
    end.

%% @private
delete_resource(Req, #state{ channel_id = ChannelId } = State) ->
    ok = logplex_channel:delete(ChannelId),
    {true, Req, State}.

%% @private
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, to_json}], Req, State}.

%% @private
to_json(Req, #state{ channel_id = ChannelId,
                     tokens     = Tokens,
                     drains     = Drains } = State) ->
    Resp = serialize_channel(ChannelId, Tokens, Drains),
    {Resp, Req, State}.

%% @private
content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, from_json}], Req, State}.

%% @private
from_json(Req, #state{ channel_id = ChannelId,
                       channel    = Channel,
                       tokens     = OldTokens,
                       drains     = Drains } = State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    try jsx:decode(Body, [return_maps]) of
        Map ->
            case validate_payload(Map) of
                {ok, NewTokenNames} ->
                    ok = create_or_update_channel(ChannelId, Channel),
                    case create_or_update_tokens(ChannelId, OldTokens, NewTokenNames) of
                        {ok, Tokens} ->
                            Resp = serialize_channel(ChannelId, Tokens, Drains),
                            Req2 = cowboy_req:set_resp_body(Resp, Req1),
                            {true, Req2, State};
                        {error, Reason} ->
                            ?WARN("at=from_json channel_id=~s error=token_op_failed reason=\"~p\"",
                                  [ChannelId, Reason]),
                            Resp = jsx:encode([{<<"error">>, <<"idempotent token operation failed">>}]),
                            Req2 = cowboy_req:set_resp_body(Resp, Req1),
                            {ok, Req3} = cowboy_req:reply(500, Req2),
                            {halt, Req3, State}
                    end;
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

%% ----------------------------------------------------------------------------
%% private functions
%% ----------------------------------------------------------------------------

-spec validate_payload(map()) -> {ok, [binary()]} | {error, term()}.
validate_payload(#{<<"tokens">> := TokenNames}) ->
    true = lists:all(fun is_binary/1, TokenNames),
    {ok, lists:usort(TokenNames)};
validate_payload(#{}) ->
    {ok, []};
validate_payload(_) ->
    {error, invalid_payload}.

create_or_update_channel(ChannelId, undefined) ->
    Channel = logplex_channel:new(ChannelId),
    logplex_channel:store(Channel);
create_or_update_channel(_ChannelId, Channel) ->
    logplex_channel:store(Channel).

-spec create_or_update_tokens(logplex_channel:id(),
                              [logplex_token:token()],
                              [logplex_token:name()]) -> {ok, [logplex_token:token()]} |
                                                         {error, term()}.
create_or_update_tokens(ChannelId, OldTokens, NewTokenNames) ->
    try
        %% extract token names of old tokens
        OldTokenNames = [logplex_token:name(Token) || Token <- OldTokens],
        %% create the list of new tokens as token records
        NewTokens = [case logplex_token:create(ChannelId, Name) of
                         TokenId when is_binary(TokenId) ->
                             logplex_token:new(TokenId, ChannelId, Name);
                         {error, Reason} ->
                             throw({failed_to_create_token, Reason})
                     end || Name <- NewTokenNames,
                            not lists:member(Name, OldTokenNames)],
        %% filter tokens that we must be deleted
        {Keepers, DeletableTokens} = lists:partition(
                                       fun(Token) ->
                                               Name = logplex_token:name(Token),
                                               lists:member(Name, NewTokenNames)
                                       end, OldTokens),
        %% delete tokens
        [true = logplex_token:delete(Token) || Token <- DeletableTokens],
        {ok, NewTokens ++ Keepers}
    catch
        Exception:Reason ->
            {error, {Exception, Reason}}
    end.

serialize_channel(Channel, Tokens, Drains) ->
    jsx:encode([{<<"channel">>, Channel}] ++
               serialize_tokens(Tokens) ++
               serialize_drains(Drains)).

serialize_tokens([]) -> [];
serialize_tokens(Tokens) ->
    [{<<"tokens">>,
      lists:sort([{logplex_token:name(Token), logplex_token:id(Token)}
                  || Token = #token{} <- Tokens])
     }].

serialize_drains([]) -> [];
serialize_drains(Drains) ->
    [{<<"drains">>,
      lists:sort([[{<<"id">>, logplex_drain:id(Drain)},
                   {<<"token">>, logplex_drain:token(Drain)},
                   {<<"url">>, logplex_drain:uri_to_binary(logplex_drain:uri(Drain))}
                  ] || Drain <- Drains,
                       logplex_drain:has_valid_uri(Drain)
                 ])
     }].
