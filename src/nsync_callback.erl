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
    Id = channel_id(parse_id(Rest)),
    create_channel(Id, Dict);

handle({load, <<"tok:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = parse_id(Rest),
    create_token(Id, Dict);

handle({load, <<"drain:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = drain_id(parse_id(Rest)),
    create_drain(Id, Dict);

handle({load, _Key, _Val}) ->
    ok;

handle({load, eof}) ->
    ?INFO("at=nsync_load_complete", []),
    error_logger:info_msg("NSYNC sync complete"),
    application:set_env(logplex, nsync_loaded, true),
    ok;

%% STREAM
handle({cmd, "hmset", [<<"ch:", Rest/binary>> | Args]}) ->
    Id = channel_id(parse_id(Rest)),
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

handle({cmd, "setex", [<<"session:", UUID/binary>>, _Expiry, Body]})
  when byte_size(UUID) =:= 36 ->
    catch logplex_session:store(UUID, Body),
    ?INFO("at=setex type=session id=~p", [UUID]);

handle({cmd, "del", [<<"ch:", Rest/binary>> | _Args]}) ->
    Id = channel_id(parse_id(Rest)),
    ?INFO("at=delete type=channel id=~p", [Id]),
    ets:delete(channels, Id);

handle({cmd, "del", [<<"tok:", Rest/binary>> | _Args]}) ->
    Id = parse_id(Rest),
    ?INFO("at=delete type=token id=~p", [Id]),
    ets:delete(tokens, Id);

handle({cmd, "del", [<<"drain:", Rest/binary>> | _Args]}) ->
    Id = drain_id(parse_id(Rest)),
    ?INFO("at=delete type=drain id=~p", [Id]),
    catch logplex_drain:stop(Id),
    ets:delete(drains, Id);

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
create_channel(Id, _Dict) ->
    Channel = #channel{id=Id},
    ets:insert(channels, Channel),
    Channel.

create_token(Id, Dict) ->
    case dict_find(<<"ch">>, Dict) of
        undefined ->
            ?ERR("~p ~p ~p ~p",
                 [create_token, missing_ch, Id, dict:to_list(Dict)]);
        Val1 ->
            Ch = list_to_integer(binary_to_list(Val1)),
            Name = dict_find(<<"name">>, Dict),
            Token = #token{
                id=Id,
                channel_id=Ch,
                name=Name
            },
            ets:insert(tokens, Token),
            Token
    end.

create_drain(Id, Dict) ->
    case dict_find(<<"ch">>, Dict) of
        undefined ->
            ?ERR("~p ~p ~p ~p",
                 [create_drain, missing_ch, Id, dict:to_list(Dict)]);
        Val1 ->
            Ch = list_to_integer(binary_to_list(Val1)),
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
                                {valid, Type, Uri} ->
                                    Drain = logplex_drain:new(Id, Ch, Token,
                                                              Type, Uri),
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
                    {syslog, "", Host, PortNo, "/", []}
            end
    end.

parse_id(Bin) ->
    [Id | _] = binary:split(Bin, <<":">>),
    Id.

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


channel_id(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

drain_id(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).
