-module(logplex_mon_serv).
-include("logplex_logging.hrl").

-behaviour(gen_server).

-define(DELAY, 300000). % 5 minutes
-define(TABLE, ?MODULE).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {ref}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Ref = erlang:start_timer(?DELAY, self(), check),
    ?TABLE = ets:new(?TABLE,[named_table, set, protected, {read_concurrency,true}]),
    {ok, #state{ref=Ref}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, Ref, check}, State=#state{ref=Ref}) ->
    check_orphans(),
    check_drain_count(),
    check_chan_count(),
    NewRef = erlang:start_timer(?DELAY, self(), check),
    {noreply, State#state{ref=NewRef}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
check_orphans() ->
    case logplex_drain:orphans() of
        [] ->
            {0,0,0};
        List ->
            {All, Drain, Channels} =
              lists:foldl(fun categorize/2, {0,0,0}, List),
            %% any: total number of orphan pids
            %% all: orphans that are not in the drains table,
            ?WARN("at=check_orphans any=~b all=~b drain=~b "
                  "channel=~b",
                  [length(List), All, Drain, Channels]),
            {All,Drain,Channels}
    end.

check_drain_count() ->
    Num = logplex_drain:num_drains(),
    ets:insert(?TABLE, {drain_count, Num}),
    compare(?TABLE, drain_count, Num),
    Num.

check_chan_count() ->
    Num = logplex_channel:num_channels(),
    ets:insert(?TABLE, {channel_count, Num}),
    compare(?TABLE, channel_count, Num),
    Num.

categorize({_Id,_Pid,List}, Acc) -> categorize1(List,Acc).

categorize1([ets_drain,ets_channel], {All,Drain,Chan}) ->
    {All+1,Drain+1,Chan+1};
categorize1([ets_drain|Rest], {All,Drain,Chan}) ->
    categorize1(Rest, {All,Drain+1,Chan});
categorize1([ets_channel], {All,Drain,Chan}) ->
    {All,Drain,Chan+1};
categorize1([_], {All,Drain,Chan}) ->
    {All,Drain,Chan};
categorize1([], Acc) ->
    Acc.

compare(Table, Key, OwnVal) ->
    spawn_link(fun() -> compare1(Table, Key, OwnVal) end).

compare1(Table, Key, OwnVal) ->
    {Good,_Bad} = rpc:multicall(ets, lookup, [Table, Key]),
    case [V || [{K,V}] <- Good, K=:=Key] of
        [] -> ok;
        [Valid|Valids] ->
            {Min,Max,Sum,Len} = lists:foldl(
                fun(Val, {Min,Max,Sum,Len}) ->
                        if Val < Min -> {Val,Max,Sum+Val,Len+1};
                           Val > Max -> {Min,Val,Sum+Val,Len+1};
                           Val >= Min, Val =< Max -> {Min,Max,Sum+Val,Len+1}
                        end
                end,
                {Valid,Valid,Valid,1},
                Valids
            ),
            ?INFO("at=check_~p val=~b min=~b max=~b avg=~p",
                  [Key, OwnVal, Min, Max, Sum/Len])
    end.

