%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Table manager for redis channel quarantines.
%% @end
%%%-------------------------------------------------------------------
-module(logplex_redis_quarantine).

-behaviour(gen_server).

-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0
         ,channel/1
         ,quarantine_channel/1
         ,unquarantine_channel/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tab}).
-define(TABLE, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec channel(ChannelId::integer()) -> quarantined | not_quarantined.
channel(ChannelId) when is_integer(ChannelId) ->
    try ets:lookup(?TABLE, {channel, ChannelId}) of
        [] ->
            not_quarantined;
        [_] ->
            quarantined
    catch
        error:badarg ->
            not_quarantined
    end.

quarantine_channel(ChannelId) when is_integer(ChannelId) ->
    case whereis(?MODULE) of
        undefined -> ignore;
        Pid ->
            gen_server:call(Pid, {quarantine_channel, ChannelId})
    end.

unquarantine_channel(ChannelId) when is_integer(ChannelId) ->
    gen_server:call(?MODULE, {unquarantine_channel, ChannelId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    Tab = ets:new(?MODULE, [named_table, protected, set]),
    {ok, #state{tab=Tab}}.

%% @private
handle_call({quarantine_channel, ChannelId}, _From, State)
  when is_integer(ChannelId) ->
    Result = ets:insert(?TABLE, {{channel, ChannelId},
                                 os:timestamp()}),
    {reply, Result, State};
handle_call({unquarantine_channel, ChannelId}, _From, State)
  when is_integer(ChannelId) ->
    Result = ets:delete(?TABLE, {channel, ChannelId}),
    {reply, Result, State};
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%% @private
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
