%% @doc
%% Simple ETS based logs per app counter that resets every second.
%% @end
-module(logplex_logs_counter).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([increment_message_counter/1]).
-export([delete_expired_counters/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/3
        ]).

%% Need to include this because we're using a match spec below.
-include_lib("stdlib/include/ms_transform.hrl").

%% ============================================================================
%% API functions
%% ============================================================================

%% @doc Start the logs counter process and link it to the caller process.
-spec start_link() -> {ok, Pid} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Updates the message counter for the given channel and returns the new message
%% count for the current epoch.
-spec increment_message_counter(ChannelId::logplex_channel:id()) -> non_neg_integer().
increment_message_counter(ChannedId) ->
    Epoch = epoch(),
    Key = {ChannedId, Epoch},
    try ets:update_counter(?MODULE, Key, 1) of
        Count -> Count
    catch
        error:badarg ->
            %% row didn't exist, create it
            %% use insert_new to avoid races
            case ets:insert_new(?MODULE, {Key, 1}) of
                true ->
                    1;
                false ->
                    %% someone beat us to it
                    ets:update_counter(?MODULE, Key, 1)
            end
    end.

%% @doc Explicit call to initialize garbage collection (for testing).
-spec delete_expired_counters() -> ok.
delete_expired_counters() ->
    Epoch = epoch(),
    gen_server:cast(?MODULE, {delete_expired_counters, Epoch}).

%% ============================================================================
%% private functions
%% ============================================================================

init([]) ->
    ets:new(?MODULE, [public,
                      named_table,
                      set,
                      {write_concurrency, true}
                     ]),
    erlang:send_after(timer:seconds(3), self(), garbage_collection_cycle),
    {ok, no_state}.

handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

handle_cast({delete_expired_counters, Epoch}, State) ->
    ExpiredKeysMatchSpec = ets:fun2ms(fun({{_, E}, _}) when E < Epoch -> true end),
    ets:select_delete(?MODULE, ExpiredKeysMatchSpec),
    {noreply, State}.
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(garbage_collection_cycle, State) ->
    delete_expired_counters(),
    erlang:send_after(timer:seconds(3), self(), garbage_collection_cycle),
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% @doc Return current epoch.
epoch() ->
    {Mega, Sec, _} = os:timestamp(),
    (Mega * 1000000 + Sec).
