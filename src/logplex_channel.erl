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
-module(logplex_channel).

-export([whereis/1
         ,post_msg/2
         ,post_msg/3
        ]).

-export([create/0, delete/1, lookup/1,
         lookup_tokens/1, lookup_drains/1, logs/2, info/1]).

-compile({no_auto_import,[whereis/1]}).

-include("logplex.hrl").
-include("logplex_logging.hrl").

-type id() :: integer().
-export_type([id/0]).

whereis({channel, _ChannelId} = Name) ->
    [ Pid || {Pid, true} <- gproc:lookup_local_properties(Name) ].

post_msg(Where, Msg) when is_binary(Msg) ->
    case logplex_syslog_utils:from_msg(Msg) of
        {error, _} = E -> E;
        ParsedMsg -> post_msg(Where, ParsedMsg)
    end;
post_msg({channel, ChannelId} = Name, Msg) when is_tuple(Msg) ->
    logplex_stats:incr(#channel_stat{channel_id=ChannelId, key=channel_post}),
    gproc:send({p, l, Name}, {post, Msg}),
    ok.

post_msg(Where, Msg, Drains) when is_binary(Msg) ->
    case logplex_syslog_utils:from_msg(Msg) of
        {error, _} = E -> E;
        ParsedMsg -> post_msg(Where, ParsedMsg, Drains)
    end;
post_msg({channel, ChannelId} = Name, Msg, Drains) when is_tuple(Msg) ->
    Pids = whereis(Name),
    logplex_stats:incr(#channel_stat{channel_id=ChannelId, key=channel_post}),
    [ Pid ! {post, Msg} || Pid <- Pids ],
    case length(Drains) - length(Pids) of
        0 -> ok; % everything is OK
        N when N > 0 ->
            logplex_drain:diagnose_drains(Name, Drains);
        N when N < 0 ->
            %% More pids than drains - assume operator intervention
            ok
    end.

-spec create() -> id() | {'error', term()}.
create() ->
    case redis_helper:channel_index() of
        ChannelId when is_integer(ChannelId) ->
            case redis_helper:create_channel(ChannelId) of
                ok ->
                    ChannelId;
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

delete(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        undefined ->
            {error, not_found};
        _ ->
            [logplex_token:delete(TokenId) || #token{id=TokenId} <- lookup_tokens(ChannelId)],
            [logplex_drain:delete(DrainId) || #drain{id=DrainId} <- lookup_drains(ChannelId)], 
            redis_helper:delete_channel(ChannelId)
    end.

lookup(ChannelId) when is_integer(ChannelId) ->
    case ets:lookup(channels, ChannelId) of
        [Channel] -> Channel;
        _ -> undefined
    end.

lookup_tokens(ChannelId) when is_integer(ChannelId) ->
    ets:match_object(tokens, token_match_expr(ChannelId)).

lookup_drains(ChannelId) when is_integer(ChannelId) ->
    ets:match_object(drains, drain_match_expr(ChannelId)).

token_match_expr(ChannelId) ->
    T = logplex_utils:empty_token(),
    T#token{channel_id=ChannelId}.

drain_match_expr(ChannelId) ->
    #drain{id='_', channel_id=ChannelId, token='_', resolved_host='_', host='_', port='_', tcp='_'}.

logs(ChannelId, Num) when is_integer(ChannelId), is_integer(Num) ->

    {Map, Interval, _TS} = logplex_shard_info:read(logplex_read_pool_map),
    Index = redis_shard:key_to_index(integer_to_list(ChannelId)),
    {_RedisUrl, Pool} = redis_shard:get_matching_pool(Index, Map, Interval),
    Cmd = [<<"LRANGE">>, iolist_to_binary(["ch:", integer_to_list(ChannelId), ":spool"]), <<"0">>, list_to_binary(integer_to_list(Num))],
    case catch redo:cmd(Pool, Cmd) of
        {'EXIT', Err} ->
            ?ERR("at=fetch_logs channel_id=~p err=\"~p\"",
                 [ChannelId, Err]),
            [];
        Logs ->
            Logs
    end.

info(ChannelId) when is_integer(ChannelId) ->
    case lookup(ChannelId) of
        #channel{} ->
            Tokens = lookup_tokens(ChannelId),
            Drains = lookup_drains(ChannelId),
            [{channel_id, ChannelId},
             {tokens, lists:sort([{Name, Token} || #token{id=Token, name=Name} <- Tokens])},
             {drains, [iolist_to_binary([<<"syslog://">>, Host, ":", integer_to_list(Port)]) || #drain{host=Host, port=Port} <- Drains, Port>0]}];
        _ ->
            []
    end.
