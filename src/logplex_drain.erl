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
-module(logplex_drain).

-export([whereis/1
         ,start/3
         ,stop/1
         ,stop/2
        ]).

-export([reserve_token/0, cache/3, create/5, create/4,
         delete/1, delete/3, clear_all/1, lookup/1]).

-export([url/1
        ]).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-compile({no_auto_import,[whereis/1]}).

-type id() :: integer().
-type token() :: binary().
-export_type([id/0
              ,token/0
             ]).

whereis({drain, _DrainId} = Name) ->
    gproc:lookup_local_name(Name).

-spec start('tcpsyslog_old' | 'tcpsyslog' | 'tcpsyslog2' | 'udpsyslog',
            id(), list()) -> any().
start(tcpsyslog_old, DrainId, Args) ->
    supervisor:start_child(logplex_drain_sup,
                           {DrainId,
                            {logplex_tcpsyslog_drain, start_link, Args},
                            transient, brutal_kill, worker,
                            [logplex_tcpsyslog_drain]});
start(Type, DrainId, Args)
  when Type =:= tcpsyslog;
       Type =:= tcpsyslog2 ->
    supervisor:start_child(logplex_drain_sup,
                           {DrainId,
                            {logplex_tcpsyslog_drain2, start_link, Args},
                            transient, brutal_kill, worker,
                            [logplex_tcpsyslog_drain]});
start(udpsyslog, DrainId, Args) ->
    supervisor:start_child(logplex_drain_sup,
                           {DrainId,
                            {logplex_udpsyslog_drain, start_link, Args},
                            transient, brutal_kill, worker,
                            [logplex_tcpsyslog_drain]}).


stop(DrainId) ->
    stop(DrainId, timer:seconds(5)).

%% Attempt a graceful shutdown of a drain process, followed by a
%% forceful supervisor based shutdown if that fails.
stop(DrainId, Timeout) ->
    DrainPid = whereis({drain, DrainId}),
    Ref = erlang:monitor(process, DrainPid),
    DrainPid ! shutdown,
    receive
        {'DOWN', Ref, process, DrainPid, _} ->
            ok
    after Timeout ->
            erlang:demonitor(Ref, [flush]),
            supervisor:terminate_child(logplex_drain_sup, DrainId)
    end,
    supervisor:delete_child(logplex_drain_sup, DrainId).

reserve_token() ->
    Token = new_token(),
    case redis_helper:drain_index() of
        DrainId when is_integer(DrainId) ->
           {ok, DrainId, Token};
        Err ->
            Err
    end.

cache(DrainId, Token, ChannelId)  when is_integer(DrainId),
                                        is_binary(Token),
                                        is_integer(ChannelId) ->
    true = ets:insert(drains, #drain{id=DrainId, channel_id=ChannelId, token=Token}).

create(DrainId, Token, ChannelId, Host, Port) when is_integer(DrainId),
                                                   is_binary(Token),
                                                   is_integer(ChannelId),
                                                   is_binary(Host),
                                                   (is_integer(Port) orelse Port == undefined) ->
    case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host=Host, port=Port, tcp='_'}) of
        [_] ->
            {error, already_exists};
        [] ->
            case logplex_utils:resolve_host(Host) of
                undefined ->
                    ?INFO("at=create_drain result=invalid dest=~s",
                          [logplex_logging:dest(Host, Port)]),
                    {error, invalid_drain};
                _Ip ->
                    case redis_helper:create_drain(DrainId, ChannelId, Token, Host, Port) of
                        ok ->
                            #drain{id=DrainId, channel_id=ChannelId, token=Token, host=Host, port=Port};
                        Err ->
                            Err
                    end
            end
    end.

create(DrainId, ChannelId, Host, Port) when is_integer(DrainId),
                                            is_integer(ChannelId),
                                            is_binary(Host) ->
    case ets:lookup(drains, DrainId) of
        [#drain{channel_id=ChannelId, token=Token}] ->
            case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host=Host, port=Port, tcp='_'}) of
                [_] ->
                    {error, already_exists};
                [] ->
                    case logplex_utils:resolve_host(Host) of
                        undefined ->
                            ?INFO("at=create_drain result=invalid dest=~s",
                                  [logplex_logging:dest(Host, Port)]),
                            {error, invalid_drain};
                        _Ip ->
                            case redis_helper:create_drain(DrainId, ChannelId, Token, Host, Port) of
                                ok -> #drain{id=DrainId, channel_id=ChannelId, token=Token, host=Host, port=Port};
                                Err -> Err
                            end
                    end
            end;
        _ ->
            {error, not_found}
    end.

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host=Host, port=Port1, tcp='_'}) of
        [#drain{id=DrainId}|_] ->
            delete(DrainId);
        _ ->
            {error, not_found}
    end.

delete(DrainId) when is_integer(DrainId) ->
    redis_helper:delete_drain(DrainId).

clear_all(ChannelId) when is_integer(ChannelId) ->
    List = ets:match_object(drains, #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host='_', port='_', tcp='_'}),
    [delete(DrainId) || #drain{id=DrainId} <- List],
    ok.

lookup(DrainId) when is_integer(DrainId) ->
    case ets:lookup(drains, DrainId) of
        [Drain] -> Drain;
        _ -> undefined
    end.

new_token() ->
    new_token(10).

new_token(0) ->
    exit({error, failed_to_reserve_drain_token});

new_token(Retries) ->
    Token = list_to_binary("d." ++ uuid:to_string(uuid:v4())),
    case ets:match_object(drains, #drain{id='_', channel_id='_', token=Token, resolved_host='_', host='_', port='_', tcp='_'}) of
        [#drain{}] -> new_token(Retries-1);
        [] -> Token
    end.

url(#drain{host=Host, port=Port}) ->
    [<<"syslog://">>, Host, ":", integer_to_list(Port)].
