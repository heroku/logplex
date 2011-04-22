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
-export([callback/3]).

%%===================================================================
%% API
%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% customized callback for nsync
callback(Ch, Token, {load, Key, _Val}) ->
    case Key of
        <<"ch:", _/binary>> -> Ch;
        <<"tok:", _/binary>> -> Token;
        _ -> undefined
    end;
callback(_Ch, _Token, {load, eof}) ->
    ok;
callback(Ch, Token, {cmd, _Cmd, Args}) ->
    case Args of
        [<<"ch:", _/binary>> | _] -> Ch;
        [<<"tok:", _/binary>> | _] -> Token;
        _ -> undefined
    end;
callback(_Ch, _Token, _) ->
    ok.

%%===================================================================
%% gen_server callbacks
%%===================================================================
init(Args) ->
    Ch = ets:new(logplex_channel, [public, named_table, set]),
    Token = ets:new(logplex_token, [public, named_table, set]),
    Opts = [{callback, {?MODULE, callback, [Ch, Token]}}],
    {ok, _Pid} = nsync:start_link(Opts),
    {ok, []}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    ok.

code_change(_OldVer, S, _Extra) ->
    {ok, S}.
