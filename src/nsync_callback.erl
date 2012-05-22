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
    Id = list_to_integer(parse_id(Rest)),
    create_channel(Id, Dict);

handle({load, <<"tok:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = list_to_binary(parse_id(Rest)),
    create_token(Id, Dict);

handle({load, <<"drain:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = list_to_integer(parse_id(Rest)),
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
    Id = list_to_integer(parse_id(Rest)),
    Dict = dict_from_list(Args),
    ?INFO("at=set type=channel id=~p", [Id]),
    create_channel(Id, Dict);

handle({cmd, "hmset", [<<"tok:", Rest/binary>> | Args]}) ->
    Id = list_to_binary(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_token(Id, Dict),
    ?INFO("at=set type=token id=~p", [Id]);

handle({cmd, "hmset", [<<"drain:", Rest/binary>> | Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    Dict = dict_from_list(Args),
    _Drain = create_drain(Id, Dict),
    ?INFO("at=set type=drain id=~p", [Id]);

handle({cmd, "del", [<<"ch:", Rest/binary>> | _Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    ?INFO("at=delete type=channel id=~p", [Id]),
    ets:delete(channels, Id);

handle({cmd, "del", [<<"tok:", Rest/binary>> | _Args]}) ->
    Id = list_to_binary(parse_id(Rest)),
    ?INFO("at=delete type=token id=~p", [Id]),
    ets:delete(tokens, Id);

handle({cmd, "del", [<<"drain:", Rest/binary>> | _Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    ?INFO("at=delete type=drain id=~p", [Id]),
    logplex_drain:stop(Id),
    ets:delete(drains, Id);

handle({cmd, _Cmd, [<<"redgrid", _/binary>>|_]}) ->
    ok;

handle({cmd, _Cmd, [<<"stats", _/binary>>|_]}) ->
    ok;

handle({cmd, _Cmd, [<<"heroku.com:stats", _/binary>>|_]}) ->
    ok;

handle({cmd, _Cmd, [<<"staging.herokudev.com:stats", _/binary>>|_]}) ->
    ok;

handle({cmd, _Cmd, _Args}) ->
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
                Token ->
                    case logplex_drain:valid_uri(drain_uri(Dict)) of
                        {valid, Type, Uri} ->
                            Drain = logplex_drain:new(Id, Ch, Token,
                                                      Type, Uri),
                            ets:insert(drains, Drain),
                            logplex_drain:start(Drain),
                            Drain
                    end
            end
    end.

%% Until we can rely on every record containing a 'url' value, we need
%% this shim to convert old tcpsyslog drains.
drain_uri(Dict) ->
    case dict_find(<<"url">>, Dict) of
        undefined ->
            %% Old style Host/Port record
            Host = dict_find(<<"host">>, Dict),
            Port =
                case dict_find(<<"port">>, Dict) of
                    undefined -> undefined;
                    Val2 -> list_to_integer(binary_to_list(Val2))
                end,
            {syslog, "", Host, Port, "/", []};
        url ->
            %% New style URI record
            URL = dict_find(<<"url">>, Dict),
            logplex_drain:parse_url(URL)
    end.

parse_id(Bin) ->
    parse_id(Bin, []).

parse_id(<<":", _/binary>>, Acc) ->
    lists:reverse(Acc);

parse_id(<<C, Rest/binary>>, Acc) ->
    parse_id(Rest, [C|Acc]).

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
