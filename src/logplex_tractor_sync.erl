-module(logplex_tractor_sync).
-behaviour(gen_server).

%% API
-export([start_link/1, init_tab/0, done/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("logplex_logging.hrl").

-define(SERVER, ?MODULE).
-define(TAB, tractor_shards).
-record(state, {client=undefined :: pid() | undefined,
                name=undefined :: atom()}).

%%% API

start_link([Name | _]=Opts) when is_list(Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [Opts], []).

init_tab() ->
    ets:new(tractor_shards, [set, named_table, public]).

done() ->
    lists:all(fun ({_Name, done}) ->
                      true;
                  ({_Name, _State}) ->
                      false
              end,
              ets:tab2list(?TAB)).

%%% gen_server callbacks

init([[Name, CallbackMod, Access, AccessArgs]]) ->
    process_flag(trap_exit, true),
    {ok, Client} = sdiff_client:start_link(CallbackMod, Access, AccessArgs),
    ets:insert(?TAB, {Name, disconnected}),

    ?INFO("at=init client=~p name=~p", [Client, Name]),
    {ok, #state{ client=Client, name=Name }, 0}.

handle_call(Request, _From, State) ->
    {stop, {invalid_call, Request}, State}.

handle_cast(Msg, State) ->
    {stop, {invalid_cast, Msg}, State}.

handle_info(timeout, #state{ client=Client, name=Name }=State) ->
    sdiff_client:ready(Client),
    ?INFO("at=ready client=~p name=~p", [Client, Name]),
    {ok, Status} = sdiff_client:sync_diff(Client, 600000),
    ets:insert(?TAB, {Name, Status}),
    Timeout = case Status of
                  done -> hibernate;
                  _ -> 5000
              end,
    ?INFO("at=sync_diff client=~p name=~p status=", [Client, Name, Status]),
    {noreply, State, Timeout};
handle_info(Info, State) ->
    {stop, {invalid_info, Info}, State}.

terminate(Reason, #state{ client=Client, name=Name }) ->
    ?INFO("at=terminate client=~p name=~p reason=~p", [Client, Name, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
