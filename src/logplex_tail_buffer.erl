%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Buffer process for logplex messages.
%% @end
%%%-------------------------------------------------------------------

-module(logplex_tail_buffer).
-behaviour(gen_fsm).

-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {buf = logplex_drain_buffer:new() :: logplex_drain_buffer:buf(),
                channel_id :: logplex_channel:id(),
                owner :: pid()}).

%-type pstates() :: 'passive' | 'active'.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2
         ,active_once/1
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

start_link(ChannelId, Owner) ->
    gen_fsm:start_link(?MODULE, #state{channel_id = ChannelId,
                                       owner = Owner}, []).

active_once(Buffer) ->
    Buffer ! {active, once}.

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
    logplex_tail:register(ChannelId),
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
handle_info({log, Msg}, StateName, S = #state{buf = OldBuf}) ->
    NewState =
        S#state{buf = logplex_drain_buffer:push(Msg, OldBuf)},
    case StateName of
        passive ->
            {next_state, passive, NewState};
        active ->
            send(NewState)
    end;

handle_info({active, once}, passive, S = #state{}) ->
    become_active(S);
handle_info({active, once}, active, S = #state{}) ->
    %% XXX - duplicate active once - is ignoring this the right thing
    %% to do? Otherwise report an error to parent?
    {next_state, active, S};

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
    case logplex_drain_buffer:empty(Buf) of
        empty ->
            {next_state, active, S};
        not_empty ->
            send(S)
    end.

send(S = #state{owner = Owner, buf = Buf}) ->
    Msgs = logplex_drain_buffer:to_list(Buf),
    Owner ! {logplex_tail_msgs, self(), Msgs},
    {next_state, passive,
     S#state{buf=logplex_drain_buffer:new()}}.
