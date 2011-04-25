-module(nsync_helper).
-export([start_link/0, callback/5]).
-export([tab_channel/0,tab_tokens/0,tab_drains/0,tab_session/0]).
-include_lib("logplex.hrl").

start_link() ->
    Opts = [
        {block, true},
        {callback, {?MODULE, callback, [logplex_channel, 
					logplex_token, 
					logplex_drain, 
					logplex_session]}}
    ],
    {ok, _Pid} = nsync:start_link(Opts).

callback(Channels, Tokens, Drains, Sessions, {load, Key, _Val}) ->
    case Key of
        <<"ch:", Rest/binary>> -> tab(Rest, Channels);
        <<"tok:", Rest/binary>> -> tab(Rest, Tokens);
        <<"drains:", Rest/binary>> -> tab(Rest, Drains);
        <<"/sessions", _/binary>> -> Sessions;
        _ -> undefined
    end;

callback(_Channels, _Tokens, _Drains, _Sessions, {load, eof}) ->
    ok;

callback(Channels, Tokens, Drains, Sessions, {cmd, _Cmd, Args}) ->
    case Args of
        [<<"ch:", Rest/binary>> | _] -> tab(Rest, Channels);
        [<<"tok:", Rest/binary>> | _] -> tab(Rest, Tokens);
        [<<"drains:", Rest/binary>> | _] -> tab(Rest, Drains);
        [<<"/sessions", _/binary>> | _] -> Sessions;
        _ -> undefined
    end.

tab(Binary, Tab) ->
    case string:sub_word(binary_to_list(Binary),2,$:) == "data" of
	true ->
	    Tab;
	false ->
	    undefined
    end.

tab_channel() ->
    nsync:tid(logplex_channel).
tab_tokens() ->
    nsync:tid(logplex_token).
tab_drains() ->
    nsync:tid(logplex_drain).
tab_session() ->
    nsync:tid(logplex_session).
