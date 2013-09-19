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

-include("logplex.hrl").
-include("logplex_logging.hrl").

%% API functions
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

incr(Key) ->
    incr(Key, 1).

-spec incr(string() | key(), integer()) -> any().
incr(Key, Inc) when is_atom(Key), is_integer(Inc) ->
    ets:update_counter(?MODULE, Key, Inc);

incr(_Key, _Inc) ->
    ok.

-type key() :: 'message_received' |
               'message_processed' |
               'drain_delivered' |
               'drain_dropped'.
-spec keys() -> [key()].
keys() ->
    [message_received
     ,message_processed
     ,drain_delivered
     ,drain_dropped
    ].

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Opts]) ->
    ets:new(?MODULE, [named_table, set, public, {write_concurrency,true}]),
    ets:insert(?MODULE, [{K, 0} || K <- keys()]),
    timer:send_interval(timer:seconds(1), flush),
    case redo:start_link(undefined, Opts) of
        {ok, Conn} ->
            {ok, #state{conn=Conn, opts=Opts}};
        Error ->
            {stop, Error}
    end.

handle_call(Msg, _From, State) ->
    ?WARN("err=unexpected_call data=~p", [Msg]),
    {noreply, State}.

handle_cast(Msg, State) ->
    ?WARN("err=unexpected_cast data=~p", [Msg]),
    {noreply, State}.

handle_info(flush, #state{instance_name=undefined}=State) ->
    InstanceName = logplex_app:config(instance_name),
    handle_info(flush, State#state{instance_name=InstanceName});

handle_info(flush, #state{instance_name=InstanceName, conn=Conn}=State) ->
    Stats = ets:tab2list(?MODULE),
    Time = os:timestamp(),
    [ets:update_counter(?MODULE, Key, -1 * Val)
     || {Key, Val} <- Stats,
        lists:member(Key, keys())],
    CloudName = logplex_app:config(cloud_name),
    spawn(fun() ->
        Stats1 = [{instance_name, list_to_binary(InstanceName)},
                  {branch, git_branch()},
                  {'AZ', availability_zone()} |
                   [ proplists:lookup(K, Stats)
                     || K <- keys() ]],
        publish_stats_to_channel(Time, InstanceName, Stats),
        % Publish to redis
        Json = iolist_to_binary(mochijson2:encode({struct, Stats1})),
        redo:cmd(Conn, [<<"PUBLISH">>, iolist_to_binary([CloudName, <<":stats">>]), Json], 60000)
    end),
    {noreply, State};

handle_info(Msg, State) ->
    ?WARN("err=unexpected_info data=~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

git_branch() ->
    case logplex_app:config(git_branch) of
        undefined -> <<>>;
        Val -> list_to_binary(Val)
    end.

assemble_stat_log_msgs(InstanceName, Time, Stats) when is_list(Stats) ->
    [assemble_stat_log_msg(InstanceName, Time, Key, Val)
     || {Key, Val} <- Stats,
        lists:member(Key, keys())].

assemble_stat_log_msg(InstanceName, Time, Key, Val) ->
    logplex_syslog_utils:fmt(
        local0,
        info,
        Time,
        "app",
        "logplex",
        "measure.logplex.~s.~s=~B source=~s branch=~s az=~s",
        [logplex_app:config(metrics_namespace, "dev"), % Prefix metric name
         atom_to_binary(Key, utf8),
         Val,
         InstanceName,
         git_branch(),
         availability_zone()]).

availability_zone() ->
    case logplex_app:config(availability_zone) of
        undefined -> <<>>;
        Val -> list_to_binary(Val)
    end.

publish_stats_to_channel(Time, InstanceName, Stats) ->
    % Publish to internal metrics drain
    case logplex_app:config(metrics_channel_id, undefined) of
        undefined -> ok; % do nothing
        InternalChannelId ->
            Msgs = assemble_stat_log_msgs(InstanceName, Time, Stats),
            [ logplex_channel:post_msg({channel, InternalChannelId}, Msg)
              || Msg <- Msgs ]
    end.

