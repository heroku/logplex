%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Buffer process for logplex messages.
%% @end
%%%-------------------------------------------------------------------

-module(logplex_drain_buffer).
-behaviour(gen_fsm).

-include("logplex.hrl").
-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {buf = logplex_msg_buffer:new() :: logplex_msg_buffer:buf(),
                channel_id :: logplex_channel:id(),
                owner :: pid(),
                active_fun :: 'undefined' | logplex_msg_buffer:framing_fun()
               }).

%-type pstates() :: 'passive' | 'active'.
-type rx_msgs() :: {'post', Msg::term()}.
-type tx_msgs() :: {'logplex_drain_data', pid(), Data::term()}.

-export_type([rx_msgs/0, tx_msgs/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2
         ,start_link/1
         ,set_active/2
        ]).

-export([active/2,
         passive/2]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ChannelId) ->
    start_link(ChannelId, self()).

start_link(ChannelId, Owner) ->
    gen_fsm:start_link(?MODULE, #state{channel_id = ChannelId,
                                       owner = Owner}, []).

-spec set_active(pid() | atom(), logplex_msg_buffer:framing_fun()) -> any().
set_active(Buffer, Fun) when is_function(Fun, 1) ->
    Buffer ! {active, Fun}.

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

active(Msg, S = #state{}) ->
    ?WARN("state=~p Unexpected msg ~p", [active, Msg]),
    {next_state, active, S}.

passive(Msg, S = #state{}) ->
    ?WARN("state=~p Unexpected msg ~p", [passive, Msg]),
    {next_state, passive, S}.

%% @private
init(S = #state{channel_id = ChannelId}) ->
    logplex_channel:register({channel_id, ChannelId}),
    {ok, passive, S}.

%% @private
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
handle_sync_event(Event, _From, StateName, State) ->
    ?WARN("[state ~p] Unexpected event ~p",
          [StateName, Event]),
    {next_state, StateName, State}.

%% @private
handle_info({post, Msg}, StateName, S = #state{buf = OldBuf}) ->
    NewState =
        S#state{buf = logplex_msg_buffer:push(Msg, OldBuf)},
    case StateName of
        passive ->
            {next_state, passive, NewState};
        active ->
            send(NewState)
    end;

handle_info({active, _}, active, S = #state{}) ->
    %% XXX - duplicate active once - is ignoring this the right thing
    %% to do? Otherwise report an error to parent?
    {next_state, active, S};

handle_info({active, Fun}, passive, S = #state{}) ->
    become_active(S#state{active_fun=Fun});

handle_info(Info, StateName, State) ->
    ?WARN("state=~p Unexpected info ~p", [StateName, Info]),
    {next_state, StateName, State}.

%% @private
terminate(_Reason, _StateName, _State) ->
    ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

become_active(S = #state{buf = Buf}) ->
    case logplex_msg_buffer:empty(Buf) of
        empty ->
            {next_state, active, S};
        not_empty ->
            send(S)
    end.

send(S = #state{owner = Owner, buf = Buf,
                active_fun = Fun}) ->
    {Data, _Count, NewBuf} = logplex_msg_buffer:to_pkts(Buf, 4096, Fun),
    Owner ! {logplex_drain_data, self(), Data},
    {next_state, passive,
     S#state{buf=NewBuf}}.
