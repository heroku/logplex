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
         ,start/1
         ,stop/1
         ,stop/2
        ]).

-export([reserve_token/0, cache/3,
         delete/1, delete/3, lookup/1
         ,delete_by_channel/1
         ,lookup_by_channel/1
         ,count_by_channel/1
         ,create/4
         ,lookup_token/1
        ]).

-export([new/5
         ,id/1
         ,token/1
         ,channel_id/1
         ,uri/1
        ]).

-export([parse_url/1
         ,valid_uri/1
         ,has_valid_uri/1
        ]).

-include("logplex.hrl").
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-compile({no_auto_import,[whereis/1]}).

-type id() :: integer().
-type token() :: binary().
-type type() :: 'tcpsyslog' | 'http' | 'udpsyslog'.
-type deprecated_types() :: 'tcpsyslog2' | 'tcpsyslog_old'.

-export_type([id/0
              ,token/0
              ,type/0
             ]).

new(Id, ChannelId, Token, Type, Uri) ->
    #drain{id=Id, channel_id=ChannelId, token=Token, type=Type, uri=Uri}.

id(#drain{id=Id}) -> Id.
token(#drain{token=Token}) -> Token.
channel_id(#drain{channel_id=CID}) -> CID.
uri(#drain{uri=Uri}) -> Uri.

start(#drain{type=Type, id=Id,
             channel_id=CID, token=Token,
             uri=Uri}) ->
    start(Type, Id, [CID, Id, Token, Uri]).

whereis({drain, _DrainId} = Name) ->
    gproc:lookup_local_name(Name).

start(Type, DrainId, Args) ->
    start_mod(mod(Type), DrainId, Args).

start_mod(Mod, DrainId, Args) when is_atom(Mod) ->
    supervisor:start_child(logplex_drain_sup,
                           {DrainId,
                            {Mod, start_link, Args},
                            transient, brutal_kill, worker,
                            [Mod]});
start_mod({error, _} = Err, _Drain, _Args) ->
    Err.

-spec mod(type() | deprecated_types()) -> atom() | {'error', term()}.
mod(tcpsyslog) -> logplex_tcpsyslog_drain2;
mod(udpsyslog) -> logplex_udpsyslog_drain;
mod(http) -> logplex_http_drain;
mod(tcpsyslog2) -> {error, deprecated};
mod(tcpsyslog_old) -> {error, deprecated};
mod(_) -> {error, unknown_drain_type}.

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

lookup_token(DrainId) when is_integer(DrainId) ->
    case ets:lookup(drains, DrainId) of
        [#drain{token=Token}] ->
            Token;
        [] -> not_found
    end.

cache(DrainId, Token, ChannelId)  when is_integer(DrainId),
                                        is_binary(Token),
                                        is_integer(ChannelId) ->
    true = ets:insert(drains, #drain{id=DrainId, channel_id=ChannelId, token=Token}).

-spec create(id(), token(), logplex_channel:id(), uri:parsed_uri()) ->
                    {'drain', id(), token()} |
                    {'error', term()}.
create(DrainId, Token, ChannelId, URI)
  when is_integer(DrainId),
       is_binary(Token),
       is_integer(ChannelId) ->
    case ets:match_object(drains, #drain{channel_id=ChannelId,
                                         uri=URI, _='_'}) of
        [_] ->
            {error, already_exists};
        [] ->
            case redis_helper:create_url_drain(DrainId, ChannelId,
                                               Token, uri:to_binary(URI)) of
                ok ->
                    {drain, DrainId, Token};
                Err ->
                    {error, Err}
            end
    end.

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case ets:match_object(drains, #drain{channel_id=ChannelId, host=Host, port=Port1, _='_'}) of
        [#drain{id=DrainId}|_] ->
            delete(DrainId);
        _ ->
            {error, not_found}
    end.

delete(DrainId) when is_integer(DrainId) ->
    redis_helper:delete_drain(DrainId).

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
    case ets:match_object(drains, #drain{token=Token, _='_'}) of
        [#drain{}] -> new_token(Retries-1);
        [] -> Token
    end.

parse_url(UrlBin) when is_binary(UrlBin) ->
    parse_url(binary_to_list(UrlBin));
parse_url(Url) when is_list(Url) ->
    case uri:parse(Url, [{scheme_defaults, uri_schemes()}]) of
        {ok, Uri} ->
            Uri;
        {error, _} = Err ->
            Err
    end.

-spec valid_uri(uri:parsed_uri() | {error, term()}) ->
                       {valid, type(), uri:parsed_uri()} |
                       {error, term()}.
valid_uri({syslog, _, _Host, _Port, _, _} = Uri) ->
    logplex_tcpsyslog_drain2:valid_uri(Uri);
valid_uri({udpsyslog, _, _Host, _Port, _, _} = Uri) ->
    logplex_udpsyslog_drain:valid_uri(Uri);
valid_uri({http, _, _, _, _, _} = Uri) ->
    {valid, http, Uri};
valid_uri({https, _, _, _, _, _} = Uri) ->
    {valid, http, Uri};
valid_uri({Scheme, _, _, _, _, _}) ->
    {error, {unknown_scheme, Scheme}};
valid_uri({error, _} = Err) -> Err.

uri_schemes() ->
    [{http,  80}
     ,{https, 443}
     ,{syslog, 601}
     ,{udpsyslog, 514}
    ].

has_valid_uri(#drain{uri=Uri}) ->
    case valid_uri(Uri) of
        {valid, _, _} -> true;
        _ -> false
    end.

delete_by_channel(ChannelId) when is_integer(ChannelId) ->
    ets:select_delete(drains,
                      ets:fun2ms(fun (#drain{channel_id=C})
                                     when C =:= ChannelId ->
                                         true
                                 end)).

count_by_channel(ChannelId) when is_integer(ChannelId) ->
    ets:select_count(drains,
                      ets:fun2ms(fun (#drain{channel_id=C})
                                     when C =:= ChannelId ->
                                         true
                                 end)).


lookup_by_channel(ChannelId) when is_integer(ChannelId) ->
    ets:select(drains,
               ets:fun2ms(fun (#drain{channel_id=C})
                                when C =:= ChannelId ->
                                  object()
                          end)).
