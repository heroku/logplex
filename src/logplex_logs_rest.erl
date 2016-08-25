%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Syslog/HTTP handler for Logplex.
%% @end
-module(logplex_logs_rest).

-include("logplex_logging.hrl").

-export([child_spec/0, dispatch/0]).

-export([init/3
         ,rest_init/2
         ,allowed_methods/2
         ,is_authorized/2
         ,known_content_type/2
         ,content_types_accepted/2
         ,malformed_request/2
         ,from_logplex/2
         ,content_types_provided/2
         ,to_response/2
        ]).

%% Healthcheck exports.
-export([handle/2
         ,terminate/3]).

-record(state, {token :: logplex_token:id() | 'any',
                name :: logplex_token:name(),
                channel_id :: logplex_channel:id() | 'any',
                msgs :: list(),
                compressed = false :: boolean(),
                zlib_port :: port()}).

-define(BASIC_AUTH, <<"Basic realm=Logplex">>).

child_spec() ->
    ranch:child_spec(?MODULE, 100,
                     ranch_tcp,
                     [{port, logplex_app:config(http_log_input_port)}],
                     cowboy_protocol,
                     [{env,
                       [{dispatch, dispatch()}]}]).

dispatch() ->
    cowboy_router:compile([{'_',
                            [{<<"/healthcheck">>, ?MODULE, [healthcheck]},
                             {<<"/logs">>, ?MODULE, [logs]},
                             % support for v2 API
                             {<<"/v2/[...]">>, ?MODULE, [api]},
                             % support for old v1 API
                             {<<"/channels/[...]">>, ?MODULE, [api]},
                             {<<"/sessions/[...]">>, ?MODULE, [api]}]}]).

init(_Transport, Req, [healthcheck]) ->
    {ok, Req, undefined};
init(_Transport, Req0, [api]) ->
    {ok, Req} = case logplex_app:config(api_endpoint_url, undefined) of
                     undefined ->
                         cowboy_req:reply(404, [], "", Req0);
                     Endpoint ->
                        {Path, Req1} = cowboy_req:path(Req0),
                        {Query, Req2} = cowboy_req:qs(Req1),
                        ?INFO("at=api_redirect path=~p query=~p", [Path, Query]),
                        cowboy_req:reply(302,
                                         [{<<"Location">>, [Endpoint, Path,
                                                            "?", Query]}],
                                         "redirecting", Req2)
                 end,
    {shutdown, Req, no_state};
init(_Transport, _Req, [logs]) ->
    {upgrade, protocol, cowboy_rest}.

%% Healthcheck implementation
handle(Req, State) ->
    {ok, Req2} = case logplex_app:elb_healthcheck() of
                     healthy ->
                         cowboy_req:reply(200, [], <<"OK">>, Req);
                     unhealthy ->
                         cowboy_req:reply(503, [],
                                               <<"SYSTEM BOOTING">>, Req)
                 end,
    {ok, Req2, State}.

terminate(_, _, #state{zlib_port = Z}) ->
    _ = (catch zlib:close(Z)),
    ok.

%% Logs cowboy_rest implementation
rest_init(Req, _Opts) ->
    State = #state{},
    ZPort = zlib:open(),
    ok = zlib:inflateInit(ZPort, 31),
    {ok, Req, #state{zlib_port = ZPort}}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        {<<"Basic ", Base64/binary>>, Req2} ->
            case binary:split(base64:decode(Base64), <<":">>) of
                [<<"token">>, TokenId = <<"t.", _/binary>>] ->
                    token_auth(State, Req2, TokenId);
                [CredId, Pass] ->
                    cred_auth(State, Req2, CredId, Pass);
                Else ->
                    ?INFO("at=authorization err=incorrect_auth_header hdr=~p", [Else]),
                    {{false, ?BASIC_AUTH}, Req2, State}
            end;
        {_, Req2} ->
            ?INFO("at=authorization err=missing_auth_header", []),
            {{false, ?BASIC_AUTH}, Req2, State}
    end.

token_auth(State, Req2, TokenId) ->
    case logplex_token:lookup(TokenId) of
        undefined ->
            logplex_realtime:incr(unknown_token),
            ?INFO("at=authorization token_id=~p msg=unknown_token", [TokenId]),
            {{false, ?BASIC_AUTH}, Req2, State};
        Token ->
            Name = logplex_token:name(Token),
            ChanId = logplex_token:channel_id(Token),
            {true, Req2,
             State#state{name=Name,
                         channel_id=ChanId,
                         token=logplex_token:id(Token)}}
    end.

cred_auth(State, Req2, CredId, Pass) ->
    case logplex_cred:auth(CredId, Pass) of
        {authorized, Cred} ->
            case logplex_cred:has_perm(any_channel, Cred) of
                permitted ->
                    {true, Req2, State#state{name = CredId,
                                             channel_id = any,
                                             token = any}};
                not_permitted ->
                    ?INFO("at=authorization err=any_channel_not_permitted"
                          " credid=~p", [CredId]),
                    respond(403, <<"Credential not permitted "
                                   "to write to any channel.">>,
                            Req2, State)
            end;
        {error, {incorrect_pass, _}} ->
            ?INFO("at=authorization err=invalid_credentials "
                  "credid=~p", [CredId]),
            {{false, ?BASIC_AUTH}, Req2, State};
        {error, What} ->
            ?INFO("at=authorization "
                  "credid=~p err=~1000p",
                  [CredId, What]),
            {{false, ?BASIC_AUTH}, Req2, State}
    end.

known_content_type(Req, State) ->
    case cowboy_req:header(<<"content-type">>, Req) of
        {<<"application/logplex-1">>, Req2} ->
            {true, Req2, State};
        {<<"application/x-logplex-1">>, Req2} ->
            {true, Req2, State};
        {_BadType, Req2} ->
            {false, Req2, State}
    end.

malformed_request(Req, State) ->
    {false, Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-logplex-1">>, []}, from_logplex},
      {{<<"application">>, <<"logplex-1">>, []}, from_logplex}],
     Req, State}.

from_logplex(Req, State = #state{token = Token,
                                 channel_id = ChannelId,
                                 name = Name}) ->
    case parse_logplex_body(Req, State) of
        {parsed, Req2, State2 = #state{msgs = Msgs}}
          when is_list(Msgs), is_binary(Token),
                 is_integer(ChannelId), is_binary(Name) ->
            logplex_message:process_msgs(Msgs, ChannelId, Token, Name),
            {true, Req2, State2#state{msgs = []}};
        {parsed, Req2, State2 = #state{msgs = Msgs}}
          when Token =:= any, ChannelId =:= any ->
            logplex_message:process_msgs(Msgs),
            {true, Req2, State2#state{msgs = []}};
        {{error, msg_count_mismatch}, Req2, State2} ->
            %% XXX - Add stat counter here?
            respond(400, <<"Message count mismatch">>, Req2, State2);
        {{error, malformed_messages}, Req2, State2} ->
            %% XXX - Add stat counter here?
            respond(400, <<"Malformed log messages">>, Req2, State2);
        {{error, Reason}, Req2, State2} when is_integer(ChannelId) ->
            ?WARN("at=parse_logplex_body channel_id=~p error=~p",
                  [ChannelId, Reason]),
            respond(400, <<"Bad request">>, Req2, State2);
        {{error, Reason}, Req2, State2} when ChannelId =:= any ->
            ?WARN("at=parse_logplex_body channel_id=~p error=~p",
                  [ChannelId, Reason]),
            respond(400, <<"Bad request">>, Req2, State2)
    end.

parse_logplex_body(Req, #state{zlib_port = ZPort} = State) ->
    case cowboy_req:body(Req) of
        {ok, Body0, Req2} ->
            case maybe_decompress_body(Body0, Req2, ZPort) of
                {Body, Req3} ->
                    parse_messages_from_body(Body, Req3, State);
                {error, CompReason, Req3} ->
                    ?WARN("at=parse_syslog_decompress reason=~p body=~1000p",
                          [CompReason, Body0]),
                    {{error, CompReason}, Req3, State}
            end;
        {error, Reason} ->
            {{error, Reason}, Req, State}
    end.

maybe_decompress_body(Body0, Req, ZPort) ->
    Res = cowboy_req:header(<<"content-encoding">>, Req, undefined),
    ct:pal("enc ~p", [Res]),
    case cowboy_req:header(<<"content-encoding">>, Req, undefined) of
        {<<"gzip">>, Req1} ->
            case decompress_body(Body0, ZPort) of
                {ok, Body} ->
                    {Body, Req1};
                {error, Reason} ->
                    {error, Reason, Req1}
            end;
        {_, Req1} ->
            {Body0, Req1}
    end.

parse_messages_from_body(Body, Req, State) ->
    case syslog_parser:parse(Body) of
        {ok, Msgs, _} ->
            check_messages(Msgs, Req, State);
        {{error, Reason}, _, _} ->
            ?WARN("at=parse_syslog reason=~p body=~1000p",
                  [Reason, Body]),
            {{error, Reason}, Req, State}
    end.

check_messages(Msgs, Req, State) ->
    case split_messages(Msgs) of
        {Good, []} ->
            check_message_count(Good, Req, State);
        {_,_Malformed} ->
            {{error, malformed_messages}, Req, State}
    end.

check_message_count(Good, Req, State) ->
    %% Logplex-Msg-Count header is optional,
    %% but if present, check our count matches sent count.
    case cowboy_req:header(<<"logplex-msg-count">>
                                , Req, false) of
        {false, Req2} ->
            {parsed, Req2, State#state{msgs=Good}};
        {Val, Req2} ->
            try
                Count = list_to_integer(binary_to_list(Val)),
                Count = length(Good),
                {parsed, Req2, State#state{msgs=Good}}
            catch
                _:_ ->
                    {{error, msg_count_mismatch}, Req2, State}
            end
    end.

split_messages(Messages) ->
    lists:partition(fun ({msg, _}) -> true;
                        ({malformed, _}) -> false;
                        (_) -> false
                    end,
                    Messages).

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, []}, to_response}],
     Req, State}.

to_response(Req, State) ->
    {"OK", Req, State}.


respond(Code, Text, Req, State) ->
    Req2 = cowboy_req:set_resp_header(
                   <<"Www-Authenticate">>, ?BASIC_AUTH, Req),
    Req3 = cowboy_req:set_resp_body(Text, Req2),
    {ok, Req4} = cowboy_req:reply(<<(integer_to_binary(Code))/binary, " ", Text/binary>>, Req3),
    {halt, Req4, State}.

decompress_body(CompressedBody, ZPort) ->
    try
        [Inflated] = zlib:inflate(ZPort, CompressedBody),
        %% reset rather than end so we can call this function again if
        %% need be.
        ok = zlib:inflateReset(ZPort),
        {ok, Inflated}
    catch _Class:Reason ->
            {error, Reason}
    end.
