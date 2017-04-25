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
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").

%% nsync callbacks

%% LOAD
handle({load, <<"ch:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = parse_id(Rest),
    create_channel(Id, Dict);

handle({load, <<"tok:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = parse_id(Rest),
    load_token(Id, Dict);

handle({load, <<"drain:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = drain_id(parse_id(Rest)),
    create_or_update_drain(Id, Dict);

handle({load, <<"cred:", Rest/binary>>, Dict}) when is_tuple(Dict) ->
    Id = logplex_cred:binary_to_id(parse_id(Rest)),
    create_cred(Id, Dict);

handle({load, _Key, _Val}) ->
    ok;

handle({load, eof}) ->
    ?INFO("at=nsync_load_complete", []),
    {Time, Result} = timer:tc(fun logplex_token:reindex_tokens/0),
    Tokens = logplex_token:num_records(tokens),
    Idxs = logplex_token:num_records(token_idxs),
    ?INFO("at=tokens_reindexed time=~pus records=~p idxrecords=~p result=~p",
          [Time, Tokens, Idxs, Result]),
    error_logger:info_msg("NSYNC sync complete"),
    application:set_env(logplex, nsync_loaded, true),
    ok;

%% STREAM
handle({cmd, "hmset", [<<"ch:", Rest/binary>> | Args]}) ->
    Id = parse_id(Rest),
    Dict = dict_from_list(Args),
    ?INFO("at=set type=channel id=~s", [Id]),
    create_channel(Id, Dict);

handle({cmd, "hmset", [<<"tok:", Rest/binary>> | Args]}) ->
    Id = parse_id(Rest),
    Dict = dict_from_list(Args),
    create_token(Id, Dict),
    ?INFO("at=set type=token id=~p", [Id]);

handle({cmd, "hmset", [<<"drain:", Rest/binary>> | Args]}) ->
    Id = drain_id(parse_id(Rest)),
    Dict = dict_from_list(Args),
    create_or_update_drain(Id, Dict),
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

handle({cmd, "setbit", [<<"control_rod">>, <<"0">>, BinValue]}) ->
    Status = control_rod_flag(BinValue),
    OldStatus = logplex_api:set_status(Status),
    ?INFO("at=setbit type=control_rod was=~p now=~p", [OldStatus, Status]);

handle({cmd, "del", []}) ->
    ok;
handle({cmd, "del", [<<"ch:", Suffix/binary>> | Args]}) ->
    Id = parse_id(Suffix),
    ?INFO("at=delete type=channel id=~s", [Id]),
    ets:delete(channels, Id),
    handle({cmd, "del", Args});
handle({cmd, "del", [<<"tok:", Suffix/binary>> | Args]}) ->
    Id = parse_id(Suffix),
    ?INFO("at=delete type=token id=~p", [Id]),
    logplex_token:delete_by_id(Id),
    handle({cmd, "del", Args});
handle({cmd, "del", [<<"drain:", Suffix/binary>> | Args]}) ->
    Id = drain_id(parse_id(Suffix)),
    ?INFO("at=delete type=drain id=~p", [Id]),
    catch logplex_drain:stop(Id),
    ets:delete(drains, Id),
    handle({cmd, "del", Args});
handle({cmd, "del", [<<"cred:", Suffix/binary>> | Args]}) ->
    Id = logplex_cred:binary_to_id(parse_id(Suffix)),
    ?INFO("at=delete type=cred id=~p", [Id]),
    logplex_cred:delete(Id),
    handle({cmd, "del", Args});
handle({cmd, "del", [<<"session:", UUID/binary>> | Args]})
  when byte_size(UUID) =:= 36 ->
    catch logplex_session:delete(UUID),
    ?INFO("at=delete type=session id=~p", [UUID]),
    handle({cmd, "del", Args});

handle({cmd, _Cmd, [<<"redgrid", _/binary>>|_]}) ->
    ok;

handle({cmd, _Cmd, [<<"stats", _/binary>>|_]}) ->
    ok;
handle({cmd, Cmd, [<<"ch:", _/binary>> | _]})
  when Cmd =:= "lpush";
       Cmd =:= "ltrim";
       Cmd =:= "expire" ->
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

handle({cmd, "ping", _Args}) ->
    %% XXX - ignore ping commands
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
            Ch = iolist_to_binary(Val1),
            Name = dict_find(<<"name">>, Dict),
            Token = logplex_token:new(Id, Ch, Name),
            {ok, Token}
    end.

%% Loads tokens without indexing them. Can only be used by rdb-load
%% routines and must be followed by a logplex_token:reindex_tokens().
load_token(Id, Dict) ->
    case find_token(Id, Dict) of
        {error, missing_channel} ->
            ?ERR("~p ~p ~p ~p",
                 [create_token, missing_ch, Id, dict:to_list(Dict)]);
        {ok, Token} ->
            logplex_token:load(Token),
            Token
    end.

create_or_update_drain(Id, Dict) ->
    case dict_find(<<"ch">>, Dict) of
        undefined ->
            ?ERR("~p ~p ~p ~p",
                 [create_drain, missing_ch, Id, dict:to_list(Dict)]);
        Val1 ->
            Ch = iolist_to_binary(Val1),
            case dict_find(<<"token">>, Dict) of
                undefined ->
                    ?ERR("~p ~p ~p ~p",
                         [create_drain, missing_token, Id, dict:to_list(Dict)]);
                Token when is_binary(Token) ->
                    case drain_uri(Dict) of
                        partial_drain_record ->
                            ?INFO("at=partial_drain_record drain_id=~p "
                                  "token=~p channel=~s",
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

control_rod_flag(<<"1">>) -> read_only;
control_rod_flag(<<"0">>) -> normal.
