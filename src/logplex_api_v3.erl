-module(logplex_api_v3).

-include("logplex_logging.hrl").

%% setup functions
-export([setup_metrics/0,
         child_spec/0
        ]).

%% rest helpers
-export([service_available/2,
         is_authorized/2
        ]).

%% request helpers
-export([prepare/2,
         elapsed/1,
         request_id/1,
         done/4
        ]).

-define(HEALTHCHECK_PATH, "/v3/healthcheck").
-define(CHANNELS_PATH,    "/v3/channels/:channel_id").
-define(DRAINS_PATH,      "/v3/channels/:channel_id/drains/[:drain_id]").
-define(TOKENS_PATH,      "/v3/channels/:channel_id/tokens").
-define(SESSIONS_PATH,    "/v3/sessions/[:session_id]").

-define(BASIC_AUTH, <<"Basic realm=Logplex">>).
-define(REQUEST_ID_HEADER_KEY, <<"request-id">>).

%% ----------------------------------------------------------------------------
%% Setup helpers
%% ----------------------------------------------------------------------------

-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    ranch:child_spec(?MODULE, 100,
                     ranch_tcp,
                     [{port, logplex_app:config(http_v3_port)}],
                     cowboy_protocol,
                     [{env,
                       [{dispatch, dispatch()}]},
                      {onresponse, fun ?MODULE:done/4}
                     ]).

dispatch() ->
    cowboy_router:compile([{'_',
                            [healthcheck_path(),
                             channels_path(),
                             drains_path(),
                             sessions_path(),
                             tokens_path()
                            ]}]).

healthcheck_path() ->
    {?HEALTHCHECK_PATH, logplex_api_v3_healthcheck, []}.

channels_path() ->
    {?CHANNELS_PATH, logplex_api_v3_channels, [{route, ?CHANNELS_PATH}]}.

drains_path() ->
    {?DRAINS_PATH, [{drain_id, int}], logplex_api_v3_drains, [{route, ?DRAINS_PATH}]}.

sessions_path() ->
    {?SESSIONS_PATH, logplex_api_v3_sessions, [{route, ?SESSIONS_PATH}]}.

tokens_path() ->
    {?TOKENS_PATH, logplex_api_v3_tokens, [{route, ?TOKENS_PATH}]}.


%% ----------------------------------------------------------------------------
%% REST helpers
%%
%% The following functions are used by resources handler across the v3 API.
%% ----------------------------------------------------------------------------

%% @private
service_available(Req, State) ->
    case logplex_app:config(api_status, normal) of
        disabled ->
            {false, Req, State};
        read_only ->
            {Method, Req1} = cowboy_req:method(Req),
            {Method == <<"GET">>, Req1, State};
        normal ->
            {true, Req, State}
    end.

%% @private
is_authorized(Req, State) ->
    try
        AuthKey = logplex_app:config(auth_key),
        {<<"Basic ", Encoded/binary>>, Req1} = cowboy_req:header(<<"authorization">>, Req),
        case binary_to_list(Encoded) of
            AuthKey ->
                {true, Req1, State};
            _ ->
                {authorized, Cred} = logplex_cred:verify_basic(Encoded),
                permitted = logplex_cred:has_perm(full_api, Cred),
                {true, Req1, State}
        end
    catch
        error:{badmatch, {incorrect_pass, CredId}} ->
            ?INFO("at=authorize cred_id=~p error=incorrect_pass",
                  [CredId]),
            {{false, ?BASIC_AUTH}, Req, State};
        Class:Ex ->
            Stack = erlang:get_stacktrace(),
            ?WARN("at=authorize exception=~1000p",
                  [{Class, Ex, Stack}]),
            {{false, ?BASIC_AUTH}, Req, State}
    end.

%% ----------------------------------------------------------------------------
%% Request logging helpers
%%
%% The following functions support logging for all requests to the v3 API.
%%
%% (Code shamelessly taken and adapted from certs-db)
%% ----------------------------------------------------------------------------

%% @doc Setup logging metrics with folsom
-spec setup_metrics() -> ok.
setup_metrics() ->
    [begin
         LatencyMeasure = http_latency_measure(Endpoint),
         folsom_metrics:new_gauge(LatencyMeasure),
         [ begin
               StatusCount = http_status_count(Endpoint, resp_code_group(Status)),
               folsom_metrics:new_counter(StatusCount)
           end || Status <- [200, 300, 400, 500, 0]
         ]
     end || Endpoint <- ["channels", "drains", "sessions", "tokens"]
    ],
    ok.



%% @doc Prepare request logging.
%% Set the metadata on a request object relevant for logging.
%% @end
-spec prepare(Route, Req) -> Req when
      Route :: string(),
      Req :: cowboy_req:req().
prepare(Route, Req) ->
    Now = os:timestamp(),
    {RequestId, Req1} = case cowboy_req:header(?REQUEST_ID_HEADER_KEY, Req) of
                            {undefined, R1} ->
                                ReqId = uuid:to_binary(uuid:v4()),
                                R2 = cowboy_req:set_resp_header(?REQUEST_ID_HEADER_KEY, ReqId, R1),
                                {ReqId, R2};
                            {ReqId, R1} when is_binary(ReqId) ->
                                {ReqId, R1}
                        end,
    Meta = [{route, Route},
            {accepted, Now},
            {request_id, RequestId}
           ],
    lists:foldl(fun({Key, Val}, Acc) ->
                        cowboy_req:set_meta(Key, Val, Acc)
                end, Req1, Meta).

%% @doc Return the route defined with `prepare'.
-spec route(Req) -> {string() | undefined, Req} when
      Req :: cowboy_req:req().
route(Req) ->
    cowboy_req:meta(route, Req).

%% @doc Returns the elapsed time since the request was prepared for logging in
%% milliseconds.
-spec elapsed(Req) -> {integer() | undefined, Req} when
      Req :: cowboy_req:req().
elapsed(Req) ->
    case cowboy_req:meta(accepted, Req) of
        {undefined, Req1} ->
            %% Some requests may not have an accepted stamp (e.g., 404 requests
            %% with unknown route which don't have a handler that could set the
            %% stamp).
            {undefined, Req1};
        {Accepted, Req1} ->
            Now = os:timestamp(),
            Elapsed = timer:now_diff(Now, Accepted) div 1000,
            {Elapsed, Req1}
    end.

%% @doc Returns the request ID of a log prepared request.
-spec request_id(Req) -> {erequest_id:request_id(), Req} when
      Req :: cowboy_req:req().
request_id(Req) ->
    cowboy_req:meta(request_id, Req).

%% @doc Gathers request information and outputs log line.
%% Called from cowboy onrequest hook
-spec done(Status, Headers, Resp, Req) -> Req when
      Status :: cowboy:http_status(),
      Headers :: cowboy:http_headers(),
      Resp :: iodata(),
      Req :: cowboy_req:req().
done(Status, _Headers, _Resp, Req) ->
    {Path, Req1} = cowboy_req:path(Req),
    {Route, Req2} = route(Req1),
    Endpoint = maybe_get_endpoint(Route),
    maybe_log(Endpoint, Path, Status, Req2).

-spec maybe_log(Endpoint :: string(),
                Path :: binary(),
                Status :: cowboy:http_status(),
                Req :: cowboy_req:req()) -> cowboy_req:req().
maybe_log("unknown_resource", Path, Status, Req) ->
    %% Since unknown resources are not `prepare`ed for logging, we lack the
    %% request meta fields `route`, `accecpted`, etc. such that we cannot log
    %% the same things as for known resources, e.g., latency.
    {Method, Req1} = cowboy_req:method(Req),
    folsom_metrics:notify(<<"logplex.http.unknown_resource">>, {inc, 1}),
	?INFO("at=done method=~p resp_code=~p path=~p", [Method, Status, Path]),
    Req1;
maybe_log(Endpoint, Path, Status, Req) ->
    {Route, Req1} = route(Req),
    {RequestId, Req2} = request_id(Req1),
    {Method, Req3} = cowboy_req:method(Req2),
    {Elapsed, Req4} = elapsed(Req3),

    folsom_metrics:notify(http_latency_measure(Endpoint), Elapsed),
    folsom_metrics:notify(http_status_count(Endpoint, resp_code_group(Status)), {inc, 1}),

    ?INFO("at=done route=~s request_id=~s method=~s resp_code=~p path=~s",
          [Route, RequestId, Method, Status, Path]),

    Req4.

maybe_get_endpoint(?CHANNELS_PATH) -> "channels";
maybe_get_endpoint(?DRAINS_PATH) -> "drains";
maybe_get_endpoint(?SESSIONS_PATH) -> "sessions";
maybe_get_endpoint(?TOKENS_PATH) -> "tokens";
maybe_get_endpoint(_) -> "unknown_resource".

resp_code_group(Status) when Status >= 200, Status < 300 -> "2xx";
resp_code_group(Status) when Status >= 300, Status < 400 -> "3xx";
resp_code_group(Status) when Status >= 400, Status < 500 -> "4xx";
resp_code_group(Status) when Status >= 500 -> "5xx";
resp_code_group(_) -> "other".

http_latency_measure(Endpoint) ->
    list_to_binary(["logplex.http.", Endpoint, ".latency.ms"]).

http_status_count(Endpoint, RespCodeGroup) ->
    list_to_binary(["logplex.http.", Endpoint, ".status.", RespCodeGroup]).
