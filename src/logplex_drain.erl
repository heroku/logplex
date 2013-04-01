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

-export([reserve_token/0, cache/3
         ,delete/1, lookup/1
         ,delete_by_channel/1
         ,delete_partial_drain/2
         ,lookup_by_channel/1
         ,count_by_channel/1
         ,create/2
         ,create/4
         ,store/1
         ,lookup_token/1
         ,poll_token/1
         ,store_token/3
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
         ,uri_schemes/0
        ]).

-export([register/4
         ,register/3
        ]).

-export([by_dest/0
         ,pids/0
        ]).

-include("logplex.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
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
mod(tcpsyslog) -> logplex_tcpsyslog_drain;
mod(udpsyslog) -> logplex_udpsyslog_drain;
mod(http) -> logplex_http_drain;
mod(_) -> {error, unknown_drain_type}.

stop(DrainId) ->
    stop(DrainId, timer:seconds(5)).

%% Attempt a graceful shutdown of a drain process, followed by a
%% forceful supervisor based shutdown if that fails.
stop(DrainId, Timeout) ->
    case whereis({drain, DrainId}) of
        DrainPid when is_pid(DrainPid) ->
            Ref = erlang:monitor(process, DrainPid),
            DrainPid ! shutdown,
            receive
                {'DOWN', Ref, process, DrainPid, {shutdown,call}} ->
                    ok;
                {'DOWN', Ref, process, DrainPid, _} -> % bad reason!
                    supervisor:terminate_child(logplex_drain_sup, DrainId)
            after Timeout ->
                    erlang:demonitor(Ref, [flush]),
                    supervisor:terminate_child(logplex_drain_sup, DrainId)
            end,
            supervisor:delete_child(logplex_drain_sup, DrainId);
        _ -> ok
    end.

reserve_token() ->
    Token = new_token(),
    case redis_helper:drain_index() of
        DrainId when is_integer(DrainId) ->
           {ok, DrainId, Token};
        Err ->
            Err
    end.

-spec poll_token(id()) -> token() | {'error', 'timeout'} |
                          {'error', any()}.
poll_token(DrainId) ->
    logplex_db:poll(fun () ->
                            case lookup_token(DrainId) of
                                not_found -> not_found;
                                T -> {found, T}
                            end
                    end,
                    logplex_app:config(default_redis_poll_ms, 2000)).

lookup_token(DrainId) when is_integer(DrainId) ->
    case ets:lookup(drains, DrainId) of
        [#drain{token=Token}] ->
            Token;
        [] -> not_found
    end.

store_token(DrainId, Token, ChannelId) when is_integer(DrainId),
                                            is_binary(Token),
                                            is_integer(ChannelId) ->
    true = ets:insert(drains, #drain{id=DrainId, token=Token,
                                     channel_id=ChannelId}),
    ok.

cache(DrainId, Token, ChannelId) when is_integer(DrainId),
                                      is_binary(Token),
                                      is_integer(ChannelId) ->
    redis_helper:reserve_drain(DrainId, Token, ChannelId).

-spec create(logplex_channel:id(), #ex_uri{}) ->
                    {'drain', id(), token()} |
                    {'error', term()}.
create(ChannelId, URL) when is_list(URL);
                            is_binary(URL) ->
    {ok, URI, _} = parse_url(URL),
    create(ChannelId, URI);
create(ChannelId, URI) ->
    reserve_token(),
    {ok, DrainId, Token} = reserve_token(),
    create(DrainId, Token, ChannelId, URI).

-spec create(id(), token(), logplex_channel:id(), #ex_uri{}) ->
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
                                               Token, uri_to_binary(URI)) of
                ok ->
                    {drain, DrainId, Token};
                Err ->
                    {error, Err}
            end
    end.

store(#drain{id=DrainId, token=Token,
             channel_id=ChannelId, uri=URI}) ->
    create(DrainId, Token, ChannelId, URI).

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
    case ex_uri:decode(Url) of
        {ok, Uri, _} ->
            Uri;
        {error, _} = Err ->
            Err
    end.

-spec valid_uri(#ex_uri{} | {error, term()}) ->
                       {valid, type(), #ex_uri{}} |
                       {error, term()}.
valid_uri(#ex_uri{scheme=Syslog} = Uri) when Syslog =:= "syslog";
                                             Syslog =:= "tcpsyslog" ->
    logplex_tcpsyslog_drain:valid_uri(Uri);
valid_uri(#ex_uri{scheme="udpsyslog"} = Uri) ->
    logplex_udpsyslog_drain:valid_uri(Uri);
valid_uri(#ex_uri{scheme="http" ++ _} = Uri) ->
    logplex_http_drain:valid_uri(Uri);
valid_uri(#ex_uri{scheme=Scheme}) ->
    {error, {unknown_scheme, Scheme}};
valid_uri({error, _} = Err) -> Err.

uri_schemes() ->
    [{http,  80}
     ,{https, 443}
     ,{syslog, 601}
     ,{udpsyslog, 514}
    ].

has_valid_uri(#drain{uri=undefined}) -> false;
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
                     ets:fun2ms(fun (#drain{channel_id=C,
                                            uri = Uri})
                                     when C =:= ChannelId,
                                          Uri =/= undefined ->
                                        true
                                end)).


lookup_by_channel(ChannelId) when is_integer(ChannelId) ->
    ets:select(drains,
               ets:fun2ms(fun (#drain{channel_id=C})
                                when C =:= ChannelId ->
                                  object()
                          end)).

-spec register(id(), logplex_channel:id(), atom(), term()) -> ok.
register(DrainId, ChannelId, Type, Dest)
  when is_integer(DrainId), is_integer(ChannelId) ->
    logplex_channel:register({channel, ChannelId}),
    register(DrainId, Type, Dest).

-spec register(id(), atom(), term()) -> ok.
register(DrainId, Type, Dest) ->
    put(logplex_drain_id, DrainId), %% post mortem debug info
    put(logplex_drain_dest, Dest), %% post mortem debug info
    put(logplex_drain_type, Type), %% post mortem debug info

    gproc:reg({n, l, {drain, DrainId}}, undefined),
    gproc:mreg(p, l, [{drain_dest, Dest},
                      {drain_type, Type}]),
    ok.

%% unregister_from_gproc(#state{drain_id=DrainId,
%%                              channel_id=ChannelId}) ->
%%     gproc:unreg({n, l, {drain, DrainId}}),
%%     gproc:munreg(p, l, [{channel, ChannelId},
%%                         drain_dest,
%%                         drain_type]).

delete_partial_drain(DrainId, Token) when is_integer(DrainId),
                                          is_binary(Token) ->
    case lookup(DrainId) of
        #drain{token = Token,
               uri = undefined} ->
            delete(DrainId),
            deleted;
        _ -> not_deleted
    end.

uri_to_binary(#ex_uri{} = Uri) ->
    iolist_to_binary(ex_uri:encode(Uri)).

by_dest() ->
    gproc:lookup_local_properties(drain_dest).

pids() ->
    gproc:select({l, n},
                 [{ { {n, l, {drain, '$1'}}, '$2', '_'},
                    [], [{{'$1', '$2'}}]}]).
