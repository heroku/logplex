%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Syslog/HTTP handler for Logplex.
%% @end
-module(logplex_logs_rest).

-export([child_spec/0]).

-export([init/3
         ,rest_init/2
         ,allowed_methods/2
         ,is_authorized/2
         ,known_content_type/2
         ,malformed_request/2
         ,process_post/2
         ,content_types_provided/2
         ,to_response/2
        ]).

-record(state, {token :: logpex_token:id(),
                name :: logplex_token:name(),
                channel_id :: logplex_channel:id(),
                msgs :: list()}).

child_spec() ->
    cowboy:child_spec(?MODULE, 100,
                      cowboy_tcp_transport,
                      [{port, logplex_app:config(http_log_input_port)}],
                      cowboy_http_protocol,
                      [{dispatch,
                        [{'_', [{[<<"logs">>, token], ?MODULE, []}]}]}]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, undefined}.

allowed_methods(Req, State) ->
    {['POST'], Req, State}.

is_authorized(Req, State) ->
    case cowboy_http_req:header(<<"Authorization">>, Req) of
        {<<"Basic ", Base64/binary>>, Req2} ->
            case binary:split(base64:decode(Base64), <<":">>) of
                [_User, TokenId = <<"t.", _/binary>>] ->
                    case logplex_token:lookup(TokenId) of
                        undefined ->
                            {{false, <<"Basic realm=Logplex">>}, Req2, State};
                        Token ->
                            Name = logplex_token:name(Token),
                            ChanId = logplex_token:channel_id(Token),
                            {true, Req2, State#state{name=Name,
                                                     channel_id=ChanId}}
                    end;
                _ ->
                    {{false, <<"Basic realm=Logplex">>}, Req2, State}
            end;
        {_, Req2} ->
            {{false, <<"Basic realm=Logplex">>}, Req2, State}
    end.

known_content_type(Req, State) ->
    case cowboy_http_req:header(<<"Content-Type">>, Req) of
        {<<"application/logplex-1">>, Req2} ->
            {true, Req2, State};
        {_, Req2} ->
            {false, Req2, State}
    end.

malformed_request(Req, State) ->
    case has_chan_token(Req, State) of
        {true, Req2, State2} ->
            {false, Req2, State2};
        {false, Req2, State2} ->
            {true, Req2, State2}
    end.

has_chan_token(Req, State) ->
    case cowboy_http_req:header(<<"Logplex-Channel-Token">>, Req) of
        {undefined, Req2} ->
            {false, Req2, State};
        {Token, Req2} when is_binary(Token) ->
            {true, Req2, State#state{token=Token}}
    end.

%% XXX - Doesn't get used in current cowboy rest code. #fail
%% content_types_accepted(Req, State) ->
%%     {[{{<<"application">>, <<"x-logplex-1">>, []}, from_logplex},
%%       {{<<"application">>, <<"logplex-1">>, []}, from_logplex}],
%%      Req, State}.
%% from_logplex(Req, State) ->
%%     case parse_logplex_body(Req, State) of
%%         {parsed, Req2, State2} ->
%%             {true, Req2, State2};
%%         {{error, _Reason}, Req2, State2} ->
%%             {false, Req2, State2}
%%     end.

process_post(Req, State = #state{token = Token,
                                 channel_id = ChannelId,
                                 name = Name})
             when is_binary(Token) ->
    case parse_logplex_body(Req, State) of
        {parsed, Req2, State2 = #state{msgs = Msgs}} when is_list(Msgs)->
            logplex_message:process_msgs(Msgs, ChannelId, Token, Name),
            {true, Req2, State2#state{msgs = []}};
        {_, Req2, State2} ->
            %% XXX - Log parse failure
            {false, Req2, State2}
    end.

parse_logplex_body(Req, State) ->
    {ok, Body, Req2} = cowboy_http_req:body(Req),
    case syslog_parser:parse(Body) of
        {ok, Msgs, _} ->
            case logplex_http_req:header(<<"Logplex-Msg-Count">>, Req2) of
                {false, Req3} ->
                    {parsed, Req3, State#state{msgs=Msgs}};
                {Val, Req3} ->
                    try
                        Count = list_to_integer(binary_to_list(Val)),
                        Count = length(Msgs),
                        {parsed, Req3, State#state{msgs=Msgs}}
                    catch
                        _:_ ->
                            {{error, msg_count_mismatch}, Req3, State}
                    end
            end;
        {{error, Reason}, _, _} ->
            {{error, Reason}, Req2, State}
    end.


content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, []}, to_response}],
     Req, State}.

to_response(Req, State) ->
    {"OK", Req, State}.
