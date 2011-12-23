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
-module(logplex_realtime).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/1, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([incr/1, incr/2]).

-record(state, {instance_name,
                opts,
                conn}).

-include_lib("logplex.hrl").

%% API functions
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

incr(Key) ->
    incr(Key, 1).

incr("work_queue_dropped", Inc) when is_integer(Inc) ->
    incr(work_queue_dropped, Inc);

incr("drain_buffer_dropped", Inc) when is_integer(Inc) ->
    incr(drain_buffer_dropped, Inc);

incr("redis_buffer_dropped", Inc) when is_integer(Inc) ->
    incr(redis_buffer_dropped, Inc);

incr(Key, Inc) when is_integer(Inc) ->
    ets:update_counter(?MODULE, Key, Inc).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Opts]) ->
    ets:new(?MODULE, [named_table, set, public]),
    ets:insert(?MODULE, [{message_received, 0},
                        {message_processed, 0},
                        {message_routed, 0},
                        {message_dropped, 0},
                        {work_queue_dropped, 0},
                        {drain_buffer_dropped, 0},
                        {redis_buffer_dropped, 0}]),
    Self = self(),
    spawn_link(fun() -> flush(Self) end),
    spawn_link(fun() -> register() end),
    case redis:start_link(undefined, Opts) of
        {ok, Conn} ->
            {ok, #state{conn=Conn, opts=Opts}};
        Error ->
            {stop, Error}
    end.

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

handle_cast(_Msg, State) ->
    io:format("realtime: recv'd ~p~n", [_Msg]),
    {noreply, State}.

handle_info(flush, #state{instance_name=undefined}=State) ->
    InstanceName = logplex_utils:instance_name(),
    handle_info(flush, State#state{instance_name=InstanceName});

handle_info(flush, #state{instance_name=InstanceName, conn=Conn}=State) ->
    Stats = ets:tab2list(?MODULE),
    [ets:update_counter(?MODULE, Key, -1 * Val) || {Key, Val} <- Stats],
    HerokuDomain = heroku_domain(),
    spawn(fun() ->
        Stats1 = [{instance_name, list_to_binary(InstanceName)},
                  {branch, git_branch()},
                  {'AZ', availability_zone()},
                  proplists:lookup(message_received, Stats),
                  proplists:lookup(message_processed, Stats),
                  proplists:lookup(message_routed, Stats),
                  proplists:lookup(message_dropped, Stats),
                  proplists:lookup(work_queue_dropped, Stats),
                  proplists:lookup(drain_buffer_dropped, Stats),
                  proplists:lookup(redis_buffer_dropped, Stats)],
        Json = iolist_to_binary(mochijson2:encode({struct, Stats1})),
        redis:q(Conn, [<<"PUBLISH">>, iolist_to_binary([HerokuDomain, <<":stats">>]), Json], 60000)
    end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

flush(Pid) ->
    timer:sleep(1000),
    Pid ! flush,
    flush(Pid).

register() ->
    redis_helper:register_stat_instance(),
    timer:sleep(10 * 1000),
    register().

heroku_domain() ->
    case get(heroku_domain) of
        undefined ->
            Domain = 
                case os:getenv("HEROKU_DOMAIN") of
                    false -> <<"">>;
                    Val -> list_to_binary(Val)
                end,
            put(heroku_domain, Domain),
            Domain;
        Domain -> Domain
    end.

git_branch() ->
    case application:get_env(logplex, git_branch) of
        {ok, Val} -> list_to_binary(Val);
        undefined -> <<>>
    end.

availability_zone() ->
    case application:get_env(logplex, availability_zone) of
        {ok, Val} -> list_to_binary(Val);
        undefined -> <<>>
    end.

