%% Copyright (c) 2010 Edward Wang <edward.yujiang.wang@gmail.com>
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
-module(logplex_nsync).
-behavior(gen_server).

%% gen_server callbacks
-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% nsync callback
-export([callback/6]).

-export([refresh_dns/0]).

-include_lib("logplex.hrl").

%%===================================================================
%% API
%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% customized callback for nsync
callback(Ch, Token, Drain, ChTokens, ChDrains, {load, Key, _Val}) ->
    case Key of
        <<"ch:", _/binary>> -> Ch;
        <<"tok:", _/binary>> -> Token;
        <<"drain:", _/binary>> -> Drain;
        <<"chtoks:", _/binary>> -> ChTokens;
        <<"chdrains:", _/binary>> -> ChDrains;
        _ -> undefined
    end;
callback(_Ch, _Token, _Drain, _ChTokens, _ChDrains, {load, eof}) ->
    ok;
callback(Ch, Token, Drain, ChTokens, ChDrains, {cmd, _Cmd, Args}) ->
    case Args of
        [<<"ch:", _/binary>> | _] -> Ch;
        [<<"tok:", _/binary>> | _] -> Token;
        [<<"drain:", _/binary>> | _] -> Drain;
        [<<"chtoks:", _/binary>> | _] -> ChTokens;
        [<<"chdrains:", _/binary>> | _] -> ChDrains;
        _ -> undefined
    end;
callback(_Ch, _Token, _Drain, _ChTokens, _ChDrains, _) ->
    ok.

%%===================================================================
%% gen_server callbacks
%%===================================================================
init(Args) ->
    Channel = ets:new(logplex_channel, [public, named_table, set]),
    Token = ets:new(logplex_token, [public, named_table, set]),
    Drain = ets:new(logplex_drain, [public, named_table, set]),
    ChTokens = ets:new(logplex_channel_tokens, [public, named_table, set]),
    ChDrains = ets:new(logplex_channel_drains, [public, named_table, set]),
    Opts = [{callback, {?MODULE, callback, [Channel, Token, Drain, ChTokens, ChDrains]}}],
    {ok, _Pid} = nsync:start_link(Opts),
    spawn_link(fun refresh_dns/0),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({resolve_host, Ip, Drain}, State) ->
    #drain{id=Id, channel_id=ChannelId, host=Host, port=Port} = Drain,
    redis_helper:delete_drain(Id),
    redis_helper:create_drain(Id, ChannelId, Ip, Host, Port),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    ok.

code_change(_OldVer, S, _Extra) ->
    {ok, S}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
refresh_dns() ->
    timer:sleep(60 * 1000),
    [begin
        case logplex_utils:resolve_host(Host) of
            undefined -> ok;
            Ip -> gen_server:cast(?MODULE, {resolve_host, Ip, Drain})
        end
    end || #drain{host=Host}=Drain <- redis_helper:lookup_drains()],
    ?MODULE:refresh_dns().
