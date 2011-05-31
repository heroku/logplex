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

handle({load, <<"ch:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = list_to_integer(parse_id(Rest)),
    create_channel(Id, Dict),
    undefined;

handle({load, <<"tok:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = list_to_binary(parse_id(Rest)),
    create_token(Id, Dict),
    undefined;

handle({load, <<"drain:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = list_to_integer(parse_id(Rest)),
    create_drain(Id, Dict),
    undefined;

handle({load, _Key, _Val}) ->
    undefined;

handle({load, eof}) ->
    error_logger:info_msg("NSYNC sync complete"),
    application:set_env(logplex, read_only, false),
    ok;

handle({cmd, "hmset", [<<"ch:", Rest/binary>> | Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_channel(Id, Dict),
    undefined;

handle({cmd, "hmset", [<<"tok:", Rest/binary>> | Args]}) ->
    Id = list_to_binary(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_token(Id, Dict),
    undefined;

handle({cmd, "hmset", [<<"drain:", Rest/binary>> | Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_drain(Id, Dict),
    undefined;

handle({cmd, "del", [<<"ch:", Rest/binary>> | _Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    ets:delete(channels, Id),
    undefined;

handle({cmd, "del", [<<"tok:", Rest/binary>> | _Args]}) ->
    Id = list_to_binary(parse_id(Rest)),
    ets:delete(tokens, Id),
    undefined;

handle({cmd, "del", [<<"drain:", Rest/binary>> | _Args]}) ->
    Id = list_to_integer(parse_id(Rest)),
    ets:delete(drains, Id),
    undefined;

handle({cmd, "hset", [<<"ch:", Rest/binary>>, <<"addon">>, Addon]}) ->
    Id = list_to_integer(parse_id(Rest)),
    Channel = lookup_channel(Id),
    ets:insert(channels, Channel#channel{addon=Addon}),
    [ets:insert(tokens, Token#token{addon=Addon}) || Token <- logplex_channel:lookup_tokens(Id)],
    undefined;    
    
handle({cmd, _Cmd, _Args}) ->
    undefined;

handle({error, closed}) ->
    error_logger:error_msg("NSYNC connection closed. Read-only mode enabled"),
    application:set_env(logplex, read_only, true),
    ok;

handle(_) ->
    ok.

parse_id(Bin) ->
    parse_id(Bin, []).

parse_id(<<":", _/binary>>, Acc) ->
    lists:reverse(Acc);

parse_id(<<C, Rest/binary>>, Acc) ->
    parse_id(Rest, [C|Acc]).

create_channel(Id, Dict) ->
    AppId = case dict_find(<<"app_id">>, Dict) of
        undefined -> undefined;
        Val -> list_to_integer(binary_to_list(Val))
    end,
    Channel = #channel{id=Id,
                   name=dict_find(<<"name">>, Dict),
                   app_id=AppId,
                   addon=dict_find(<<"addon">>, Dict)},
    ets:insert(channels, Channel).

create_token(Id, Dict) ->
    ChannelId = list_to_integer(binary_to_list(dict_find(<<"ch">>, Dict))),
    Name = dict_find(<<"name">>, Dict), 
    {AppId, Addon} = case lookup_channel(ChannelId) of
        #channel{app_id=AppId0, addon=Addon0} -> {AppId0, Addon0};
        undefined -> {undefined, undefined}
    end,
    Token = #token{id=Id,
                   channel_id=ChannelId,
                   name=Name,
                   app_id=AppId,
                   addon=Addon},
    ets:insert(tokens, Token).

create_drain(Id, Dict) ->
    Ch = case dict_find(<<"ch">>, Dict) of
        undefined -> undefined;
        Val1 -> list_to_integer(binary_to_list(Val1))
    end,
    Port = case dict_find(<<"port">>, Dict) of
        undefined -> undefined;
        Val2 -> list_to_integer(binary_to_list(Val2))
    end,
    Drain = #drain{id=Id,
                   channel_id=Ch,
                   resolved_host=logplex_utils:resolve_host(dict_find(<<"host">>, Dict)),
                   host=dict_find(<<"host">>, Dict),
                   port=Port},
    ets:insert(drains, Drain).

lookup_channel(Id) ->
    case ets:lookup(channels, Id) of
        Channel when is_record(Channel, channel) -> Channel;
        _ -> redis_helper:lookup_channel(Id)
    end.            

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

