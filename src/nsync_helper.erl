-module(nsync_helper).
-export([start_link/0, callback/7]).
-export([tab_channel/0, tab_channel_tokens/0, tab_channel_drains/0, 
	 tab_tokens/0, tab_drains/0, tab_session/0]).
-include_lib("logplex.hrl").

start_link() ->
    Opts = [
        {block, true},
        {callback, {?MODULE, callback, [logplex_channel, 
					logplex_channel_tokens,
					logplex_channel_drains,
					logplex_token, 
					logplex_drain, 
					logplex_session]}}
    ],
    {ok, _Pid} = nsync:start_link(Opts).

callback(Channels, ChannelTokens, ChannelDrains, 
	 Tokens, Drains, Sessions, {load, Key, _Val}) ->
    case Key of
        <<"ch:", Rest/binary>> -> 
	    type2tab(Rest, [{"data", Channels}, {"tok", ChannelTokens}, {"drain", ChannelDrains}]);
        <<"tok:", Rest/binary>> ->
	    type2tab(Rest, [{"data", Tokens}]);
        <<"drain:", Rest/binary>> -> 
	    type2tab(Rest, [{"data", Drains}]);
        <<"/sessions", _/binary>> -> Sessions;
        _ -> undefined
    end;

callback(_Channels, _ChannelTokens, _ChannelDrains, 
	 _Tokens, _Drains, _Sessions, {load, eof}) ->
    gen_server:cast(logplex_drain, refresh_dns),
    ok;

callback(Channels,  ChannelTokens, ChannelDrains, 
	 Tokens, Drains, Sessions, {cmd, Cmd, Args}) ->
    case Args of
        [<<"ch:", Rest/binary>> | _] -> 
	    type2tab(Rest, [{"data", Channels}, {"tok", ChannelTokens}, {"drain", ChannelDrains}]);
        [<<"tok:", Rest/binary>> | _] -> 
	    type2tab(Rest, [{"data", Tokens}]);
        [<<"drain:", Rest/binary>> | _] -> 
	    case type2tab(Rest, [{"data", Drains}]) of
		Drains -> 
		    drain_trigger(Cmd, Args),
		    Drains;
		Other -> Other
	    end;
        [<<"/sessions", _/binary>> | _] -> Sessions;
        _ -> undefined
    end.

tab_channel() ->
    nsync:tid(logplex_channel).
tab_channel_tokens() ->
    nsync:tid(logplex_channel_tokens).
tab_channel_drains() ->
    nsync:tid(logplex_channel_drains).
tab_tokens() ->
    nsync:tid(logplex_token).
tab_drains() ->
    nsync:tid(logplex_drain).
tab_session() ->
    nsync:tid(logplex_session).

type2tab(_Binary, []) ->
    undefined;
type2tab(Binary, [{Word, Tab} | T]) ->
    case second_token(Binary, Word) of
	true -> Tab;
	false -> type2tab(Binary, T)
    end.

second_token(Binary, Word) ->
    string:sub_word(binary_to_list(Binary), 2, $:) == Word.


drain_trigger("hmset", [Drain | Rest]) ->
    case logplex_utils:resolve_host(search_next(<<"host">>, Rest)) of
	undefined -> ok;
	Ip -> 
	    DrainId = list_to_integer(string:sub_word(binary_to_list(Drain), 2, $:)),
	    gen_server:cast(logplex_drain, {create_drain, DrainId, Ip})
    end;
drain_trigger("del", [Drain | _Rest]) ->
    DrainId = list_to_integer(string:sub_word(binary_to_list(Drain), 2, $:)),
    gen_server:cast(logplex_drain, {delete_drain, DrainId});
drain_trigger(_Cmd, _Args) ->
    ok.

search_next(_Value, []) ->
    [];
search_next(Value, [Value, Next | _]) ->
    Next;
search_next(Value, [_ | T]) ->
    search_next(Value, T).
