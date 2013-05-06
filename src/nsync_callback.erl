%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(nsync_callback).
-export([handle/1]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

%% nsync callbacks

%% LOAD
handle({load, <<"ch:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = logplex_channel:binary_to_id(parse_id(Rest)),
    create_channel(Id, Dict);

handle({load, <<"tok:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = parse_id(Rest),
    create_token(Id, Dict);

handle({load, <<"drain:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = drain_id(parse_id(Rest)),
    create_drain(Id, Dict);

handle({load, <<"cred:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = logplex_cred:binary_to_id(parse_id(Rest)),
    create_cred(Id, Dict);

handle({load, _Key, _Val}) ->
    ok;

handle({load, eof}) ->
    ?INFO("at=nsync_load_complete", []),
    error_logger:info_msg("NSYNC sync complete"),
    application:set_env(logplex, nsync_loaded, true),
    ok;

%% STREAM
handle({cmd, "hmset", [<<"ch:", Rest/binary>> | Args]}) ->
    Id = logplex_channel:binary_to_id(parse_id(Rest)),
    Dict = dict_from_list(Args),
    ?INFO("at=set type=channel id=~p", [Id]),
    create_channel(Id, Dict);

handle({cmd, "hmset", [<<"tok:", Rest/binary>> | Args]}) ->
    Id = parse_id(Rest),
    Dict = dict_from_list(Args),
    create_token(Id, Dict),
    ?INFO("at=set type=token id=~p", [Id]);

handle({cmd, "hmset", [<<"drain:", Rest/binary>> | Args]}) ->
    Id = drain_id(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_drain(Id, Dict),
    ?INFO("at=set type=drain id=~p", [Id]);

handle({cmd, "hmset", [<<"cred:", Rest/binary>> | Args]}) ->
    Id = logplex_cred:binary_to_id(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_cred(Id, Dict),
    ?INFO("at=set type=cred id=~p", [Id]);

handle({cmd, "setex", [<<"session:", UUID/binary>>, _Expiry, Body]})
  when byte_size(UUID) =:= 36 ->
    catch logplex_session:store(UUID, Body),
    ?INFO("at=setex type=session id=~p", [UUID]);

handle({cmd, "del", [<<"ch:", Rest/binary>> | _Args]}) ->
    Id = logplex_channel:binary_to_id(parse_id(Rest)),
    ?INFO("at=delete type=channel id=~p", [Id]),
    ets:delete(channels, Id);

handle({cmd, "del", [<<"tok:", Rest/binary>> | _Args]}) ->
    Id = parse_id(Rest),
    ?INFO("at=delete type=token id=~p", [Id]),
    logplex_token:delete(Id);

handle({cmd, "del", [<<"drain:", Rest/binary>> | _Args]}) ->
    Id = drain_id(parse_id(Rest)),
    ?INFO("at=delete type=drain id=~p", [Id]),
    catch logplex_drain:stop(Id),
    ets:delete(drains, Id);

handle({cmd, "del", [<<"cred:", Rest/binary>> | _Args]}) ->
    Id = logplex_cred:binary_to_id(parse_id(Rest)),
    ?INFO("at=delete type=cred id=~p", [Id]),
    logplex_cred:delete(Id);

handle({cmd, "del", [<<"session:", UUID/binary>> | _Args]})
  when byte_size(UUID) =:= 36 ->
    catch logplex_session:delete(UUID),
    ?INFO("at=delete type=session id=~p", [UUID]);

handle({cmd, _Cmd, [<<"redgrid", _/binary>>|_]}) ->
    ok;

handle({cmd, _Cmd, [<<"stats", _/binary>>|_]}) ->
    ok;

handle({cmd, "incr", [<<"channel_index", _/binary>> | _]}) ->
    %% ignore the channel_index traffic
    ok;
handle({cmd, "incr", [<<"healthcheck", _/binary>> | _]}) ->
    %% ignore the redis healthcheck traffic
    ok;

handle({cmd, "publish", _Args}) ->
    %% XXX - ignore publish commands like:
    %% <<"geoff.herokudev.com:stats">>,JSONBinary
    ok;

handle({cmd, Cmd, Args}) ->
    ?INFO("at=unknown_command cmd=~p args=~1000p",
          [Cmd, Args]),
    ok;

handle({error, closed}) ->
    ?ERR("msg=closed", []),
    exit({error, closed}),
    ok;

handle(_Other) ->
    ?ERR("msg=~p", [_Other]),
    ok.

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
    case dict_find(<<"ch">>, Dict) of
        undefined ->
            ?ERR("~p ~p ~p ~p",
                 [create_token, missing_ch, Id, dict:to_list(Dict)]);
        Val1 ->
            Ch = convert_to_integer(Val1),
            Name = dict_find(<<"name">>, Dict),
            Token = logplex_token:new(Id, Ch, Name),
            logplex_token:cache(Token),
            Token
    end.

create_drain(Id, Dict) ->
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
                                    logplex_drain:start(Drain),
                                    Drain;
                                {error, Reason} ->
                                    ?ERR("create_drain invalid_uri ~p ~p ~p",
                                         [Reason, Id, dict:to_list(Dict)])
                            end
                    end
            end
    end.

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

dict_from_list(List) ->
    dict_from_list(List, dict:new()).

dict_from_list([], Dict) ->
    Dict;

dict_from_list([Key, Val | Rest], Dict) ->
    dict_from_list(Rest, dict:store(Key, Val, Dict)).

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
