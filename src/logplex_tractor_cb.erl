-module(logplex_tractor_cb).

-export([sdiff_opts/1,
         parse_url/1,
         handle/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("logplex.hrl").
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").


sdiff_opts(Name) ->
    {Host, Port} = tractor_callback:parse_url(logplex_app:config(tractor_url)),
    Timeout = logplex_app:config(tractor_timeout),
    [Name,
     fun ?MODULE:handle/1,
     sdiff_access_tcp_client,
     {Host, Port, [], Timeout}].

parse_url(Url) when is_list(Url) ->
    parse_url(ex_uri:decode(Url));
parse_url(#ex_uri{authority=#ex_uri_authority{ host=Host, port=Port }}) ->
    {Host, Port}.

handle({write, <<"ch:", Rest/binary>>, Val}) ->
    Id = parse_id(Rest),
    Dict = erlang:binary_to_term(Val),
    create_channel(Id, Dict),
    ?INFO("at=set type=channel id=~p", [Id]);
handle({write, <<"tok:", Rest/binary>>, Val}) ->
    Id = parse_id(Rest),
    Dict = erlang:binary_to_term(Val),
    create_token(Id, Dict),
    ?INFO("at=set type=token id=~p", [Id]);
handle({write, <<"drain:", Rest/binary>>, Val}) ->
    Id = drain_id(parse_id(Rest)),
    Dict = erlang:binary_to_term(Val),
    create_or_update_drain(Id, Dict),
    ?INFO("at=set type=drain id=~p", [Id]);
handle({write, <<"cred:", Rest/binary>>, Val}) ->
    Id = logplex_cred:binary_to_id(parse_id(Rest)),
    Dict = erlang:binary_to_term(Val),
    create_cred(Id, Dict),
    ?INFO("at=set type=cred id=~p", [Id]);
handle({write, <<"session:", UUID/binary>>, Body}) when byte_size(UUID) =:= 36 ->
    catch logplex_session:store(UUID, Body),
    ?INFO("at=setex type=session id=~p", [UUID]);
handle({write, _Key, _Val}) ->
    ignored;
handle({delete, <<"ch:", Suffix/binary>>}) ->
    Id = logplex_channel:binary_to_id(parse_id(Suffix)),
    ?INFO("at=delete type=channel id=~p", [Id]),
    ets:delete(channels, Id);
handle({delete, <<"tok:", Suffix/binary>>}) ->
    Id = parse_id(Suffix),
    ?INFO("at=delete type=token id=~p", [Id]),
    logplex_token:delete_by_id(Id);
handle({delete, <<"drain:", Suffix/binary>>}) ->
    Id = drain_id(parse_id(Suffix)),
    ?INFO("at=delete type=drain id=~p", [Id]),
    catch logplex_drain:stop(Id),
    ets:delete(drains, Id);
handle({delete, <<"cred:", Suffix/binary>>}) ->
    Id = logplex_cred:binary_to_id(parse_id(Suffix)),
    ?INFO("at=delete type=cred id=~p", [Id]),
    logplex_cred:delete(Id);
handle({delete, <<"session", UUID/binary>>}) when byte_size(UUID) =:= 36 ->
    catch logplex_session:delete(UUID),
    ?INFO("at=delete type=session id=~p", [UUID]);
handle({delete, _Key}) ->
    ignored.

%% Helper functions
create_channel(ChannelId, Dict) ->
    try
        Name = dict_find(<<"name">>, Dict, <<"">>),
        Flags = dict_find(<<"flags">>, Dict, <<"">>),
        logplex_channel:cache(ChannelId, Name,
                              logplex_channel:binary_to_flags(Flags))
    catch
        C:E ->
            ?WARN("at=create_channel class=~p exception=e stack=~100p",
                  [C, E, erlang:get_stacktrace()]),
            {error, {C, E}}
    end.

create_token(Id, Dict) ->
    case find_token(Id, Dict) of
        {error, missing_channel} ->
            ?ERR("~p ~p ~p ~p",
                 [create_token, missing_ch, Id, dict:to_list(Dict)]);
        {ok, Token} ->
            logplex_token:cache(Token),
            Token
    end.

find_token(Id, Dict) ->
    case dict_find(<<"ch">>, Dict) of
        undefined ->
            {error, missing_channel};
        Val1 ->
            Ch = convert_to_integer(Val1),
            Name = dict_find(<<"name">>, Dict),
            Token = logplex_token:new(Id, Ch, Name),
            {ok, Token}
    end.

create_or_update_drain(Id, Dict) ->
    case dict_find(<<"ch">>, Dict) of
        undefined ->
            ?ERR("~p ~p ~p ~p",
                 [create_drain, missing_ch, Id, dict:to_list(Dict)]);
        Val1 ->
            Ch = convert_to_integer(Val1),
            case dict_find(<<"token">>, Dict) of
                undefined ->
                    ?ERR("~p ~p ~p ~p",
                         [create_drain, missing_token, Id, dict:to_list(Dict)]);
                Token when is_binary(Token) ->
                    case drain_uri(Dict) of
                        partial_drain_record ->
                            ?INFO("at=partial_drain_record drain_id=~p "
                                  "token=~p channel=~p",
                                  [Id, Token, Ch]),
                            logplex_drain:store_token(Id, Token, Ch);
                        Uri ->
                            case logplex_drain:valid_uri(Uri) of
                                {valid, Type, NewUri} ->
                                    Drain = logplex_drain:new(Id, Ch, Token,
                                                              Type, NewUri),
                                    ets:insert(drains, Drain),
                                    maybe_update_drain(logplex_drain:start(Drain), Drain),
                                    Drain;
                                {error, Reason} ->
                                    ?ERR("create_drain invalid_uri ~p ~p ~p",
                                         [Reason, Id, dict:to_list(Dict)])
                            end
                    end
            end
    end.

maybe_update_drain({ok, _Pid}, _Drain) ->
  ok;
maybe_update_drain({error, {already_started, _Pid}}, #drain{ id=Id }=Drain) ->
  logplex_drain:stop(Id),
  logplex_drain:start(Drain).

create_cred(Id, Dict) ->
    try logplex_cred:from_dict(Id, Dict) of
        Cred ->
            logplex_cred:cache(Cred)
    catch
        error:Why ->
            ?ERR("at=create_cred error=~1000p", [Why])
    end.

%% Until we can rely on every record containing a 'url' value, we need
%% this shim to convert old tcpsyslog drains.
drain_uri(Dict) ->
    case dict_find(<<"url">>, Dict) of
        URL when is_binary(URL) ->
            %% New style URI record
            logplex_drain:parse_url(URL);
        undefined ->
            case {dict_find(<<"host">>, Dict),
                  dict_find(<<"port">>, Dict)} of
                {undefined,_} ->
                    partial_drain_record;
                {_, undefined} ->
                    partial_drain_record;
                %% Old style Host/Port record
                {Host, Port} when is_binary(Host),
                                  is_binary(Port) ->
                    PortNo = list_to_integer(binary_to_list(Port)),
                    logplex_tcpsyslog_drain:uri(binary_to_list(Host), PortNo)
            end
    end.

parse_id(Bin) ->
    [Id | _] = binary:split(Bin, <<":">>),
    Id.

convert_to_integer(V) when is_binary(V) ->
    list_to_integer(binary_to_list(V));
convert_to_integer(V) when is_list(V) ->
    list_to_integer(V).

dict_find(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, Val} -> Val;
        _ -> undefined
    end.

dict_find(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        {ok, Val} -> Val;
        _ -> Default
    end.

drain_id(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).
