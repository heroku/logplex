-module(logplex_api_v3_channel_logs).

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
         to_logs/2
        ]).

-record(state, {
          channel_id :: binary(),
          num_logs = 100 :: integer() %% number of log lines to fetch from log buffers
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
    {[<<"GET">>], Req, State}.

%% @private
malformed_request(Req, State) ->
    case cowboy_req:qs_val(<<"num">>, Req) of
        {undefined, Req1} ->
            {false, Req1, State};
        {Val, Req1} ->
            try
                Num = list_to_integer(binary_to_list(Val)),
                NewState = State#state{ num_logs = Num },
                {false, Req1, NewState}
            catch
                _:_ ->
                    {false, Req1, State}
            end
    end.

%% @private
is_authorized(Req, State) ->
    logplex_api_v3:is_authorized(Req, State).

%% @private
resource_exists(Req, #state{ channel_id = ChannelId } = State) ->
    case logplex_channel:find(ChannelId) of
        {ok, _Channel} ->
            {true, Req, State};
        {error, not_found} ->
            %% channel was not found
            {false, Req, State}
    end.

%% @private
content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"logplex-1">>, []}, to_logs}], Req, State}.

%% @private
to_logs(Req, #state{ channel_id = ChannelId,
                     num_logs   = NumLogs } = State) ->
    %% fetch messages from log buffer
    case logplex_channel:logs(ChannelId, NumLogs) of
        {error, Reason} ->
            ?ERR("channel_id=~s err='failed to fetch channel logs' reason='~p'",
                 [ChannelId, Reason]),
            Resp = jsx:encode([{<<"error">>, <<"failed to fetch channel logs">>}]),
            Req1 = cowboy_req:set_resp_body(Resp, Req),
            {ok, Req2} = cowboy_req:reply(500, Req1),
            {halt, Req2, State};
        Logs when is_list(Logs) ->
            %% Cowboy hands us the function to write a chunk, here we need to return
            %% a function that passes our chunks to the chunk writer.
            ChunkedFun = fun(WriteChunkFun) -> serve_logs(Logs, WriteChunkFun) end,
            MsgCount = length(Logs),
            Req1 = cowboy_req:set_resp_header(<<"logplex-msg-count">>, integer_to_list(MsgCount), Req),
            Req2 = cowboy_req:set_resp_header(<<"connection">>, <<"close">>, Req1),
            {{chunked, ChunkedFun}, Req2, State}
    end.

serve_logs(Logs, WriteChunkFun) ->
    %% note we drop bad syslog messages
    [WriteChunkFun(Msg) || Msg <- prepare_logs(Logs, []), {error, bad_syslog_msg} /= Msg],
    ok.

prepare_logs([], Acc) ->
    Acc;
prepare_logs([Msg | Logs], Acc) ->
    prepare_logs(Logs, [prepare_msg(Msg) | Acc]).

prepare_msg(RawMsg) ->
    case logplex_syslog_utils:from_msg(RawMsg) of
        {Facility, Severity, Time, Source, Process, Msg} ->
            RFC5424Msg = logplex_syslog_utils:rfc5424(Facility, Severity, Time, "host", Source, Process, undefined, Msg),
            logplex_syslog_utils:frame([RFC5424Msg, $\n]);
        {error, bad_syslog_msg} = Error ->
            Error
    end.
