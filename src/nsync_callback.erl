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

-include_lib("logplex.hrl").

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
    populate_token_channel_data(ets:tab2list(tokens)),
    populate_token_drain_data(ets:tab2list(drains)),
    error_logger:info_msg("NSYNC sync complete"),
    application:set_env(logplex, read_only, false),
    ok;

%% STREAM
handle({cmd, "hmset", [<<"ch:", Rest/binary>> | Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_channel(Id, Dict);

handle({cmd, "hmset", [<<"tok:", Rest/binary>> | Args]}) ->
    Id = list_to_binary(parse_id(Rest)),
    Dict = dict_from_list(Args),
    Token = create_token(Id, Dict),
    populate_token_channel_data([Token]);

handle({cmd, "hmset", [<<"drain:", Rest/binary>> | Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    Dict = dict_from_list(Args),
    Drain = create_drain(Id, Dict),
    populate_token_drain_data([Drain]);

handle({cmd, "del", [<<"ch:", Rest/binary>> | _Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    ets:delete(channels, Id);

handle({cmd, "del", [<<"tok:", Rest/binary>> | _Args]}) ->
    Id = list_to_binary(parse_id(Rest)),
    ets:delete(tokens, Id);

handle({cmd, "del", [<<"drain:", Rest/binary>> | _Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    remove_token_drain_data(Id),
    ets:delete(drains, Id);

handle({cmd, _Cmd, _Args}) ->
    ok;

handle({error, closed}) ->
    io:format("Error NSYNC connection closed. Read-only mode enabled"),
    application:set_env(logplex, read_only, true),
    ok;

handle(_) ->
    ok.

%% Helper functions
create_channel(Id, Dict) ->
    case dict_find(<<"app_id">>, Dict) of
        undefined ->
            io:format("Error ~p ~p ~p ~p ~p~n", [?MODULE, create_channel, missing_app_id, Id, dict:to_list(Dict)]);
        Val ->
            AppId = list_to_integer(binary_to_list(Val)),
            Channel = #channel{id=Id,
                   name=dict_find(<<"name">>, Dict),
                   app_id=AppId},
            ets:insert(channels, Channel),
            Channel
    end.

create_token(Id, Dict) ->
    case dict_find(<<"ch">>, Dict) of
        undefined ->
            io:format("Error ~p ~p ~p ~p ~p~n", [?MODULE, create_token, missing_ch, Id, dict:to_list(Dict)]);
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
            io:format("Error ~p ~p ~p ~p ~p~n", [?MODULE, create_drain, missing_ch, Id, dict:to_list(Dict)]);
        Val1 ->
            Ch = list_to_integer(binary_to_list(Val1)),
            case dict_find(<<"port">>, Dict) of
                undefined ->
                    io:format("Error ~p ~p ~p ~p ~p~n", [?MODULE, create_drain, missing_port, Id, dict:to_list(Dict)]);
                Val2 ->
                    Port = list_to_integer(binary_to_list(Val2)),
                    case dict_find(<<"host">>, Dict) of
                        undefined ->
                            io:format("Error ~p ~p ~p ~p ~p~n", [?MODULE, create_drain, missing_host, Id, dict:to_list(Dict)]);
                        Host ->
                            Drain = #drain{
                                id=Id,
                                channel_id=Ch,
                                token=dict_find(<<"token">>, Dict),
                                host=Host,
                                port=Port
                            },
                            ets:insert(drains, Drain),
                            Drain
                    end
            end
    end.

populate_token_channel_data([]) ->
    ok;

populate_token_channel_data([Token|Tail]) when is_record(Token, token) ->
    case logplex_channel:lookup(Token#token.channel_id) of
        undefined ->
            io:format("Error ~p ~p ~p ~p~n", [?MODULE, populate_token_channel_data, undefined_channel, Token]);
        #channel{app_id=AppId} ->
            ets:insert(tokens, Token#token{app_id=AppId})
    end,
    populate_token_channel_data(Tail);

populate_token_channel_data([_|Tail]) ->
    populate_token_channel_data(Tail).

populate_token_drain_data([]) ->
    ok;

populate_token_drain_data([Drain|Tail]) when is_record(Drain, drain) ->
    case ets:match_object(tokens, #token{id='_', channel_id=Drain#drain.channel_id, name='_', app_id='_', drains='_'}) of
        [] ->
            io:format("Error ~p ~p ~p ~p~n", [?MODULE, populate_token_drain_data, undefined_tokens, Drain]);
        Tokens ->
            Drain1 = Drain#drain{resolved_host=logplex_utils:resolve_host(Drain#drain.host)},
            ets:insert(tokens, [Token#token{drains=[Drain1|Drains]} || #token{drains=Drains}=Token <- Tokens])
    end,
    populate_token_drain_data(Tail);

populate_token_drain_data([_|Tail]) ->
    populate_token_drain_data(Tail).

remove_token_drain_data(DrainId) ->
    case logplex_drain:lookup(DrainId) of
        undefined ->
            io:format("Error ~p ~p ~p ~p~n", [?MODULE, remove_token_drain_data, undefined_drain, DrainId]);
        Drain ->
            case ets:match_object(tokens, #token{id='_', channel_id=Drain#drain.channel_id, name='_', app_id='_', drains='_'}) of
                [] ->
                    io:format("Error ~p ~p ~p ~p~n", [?MODULE, remove_token_drain_data, undefined_tokens, Drain]);
                Tokens ->
                    ets:insert(tokens, [begin
                        Drains1 = lists:filter(fun(#drain{id=Id}) -> Id =/= DrainId end, Drains),
                        Token#token{drains=Drains1} 
                    end || #token{drains=Drains}=Token <- Tokens])
            end
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
