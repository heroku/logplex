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
                on_activation :: 'undefined' |
                                 {TargBytes::pos_integer(),
                                  logplex_msg_buffer:framing_fun()}
               }).

%% -type mode() :: 'passive' | 'active' | 'notify'.

-type rx_msgs() :: {'post', Msg::term()}.
-type tx_msgs() :: {'logplex_drain_buffer', pid(), 'new_data'} |
                   {'logplex_drain_buffer', pid(),
                    {frame, Frame::iolist(), Count::non_neg_integer()}}.

-export_type([rx_msgs/0, tx_msgs/0]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2
         ,start_link/1
         ,start_link/3
         ,set_active/3
         ,notify/1
        ]).

-export([active/2,
         passive/2,
         notify/2
        ]).

-export([post/2
        ]).

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
    start_link(ChannelId, Owner, notify).

-spec start_link(ChannelId::logplex_channel:id(),
                 Owner::pid(),
                 {active, TargBytes::pos_integer(),
                  Fun::logplex_msg_buffer:framing_fun()} |
                 'passive' | 'notify') -> any().
start_link(ChannelId, Owner, {active, TargBytes, Fun})
  when is_integer(TargBytes), TargBytes > 0,
       is_function(Fun, 1) ->
    gen_fsm:start_link(?MODULE, {active,
                                 #state{channel_id = ChannelId,
                                        owner = Owner,
                                        on_activation = {TargBytes, Fun}}}, []);
start_link(ChannelId, Owner, Mode)
  when Mode =:= passive; Mode =:= notify ->
    gen_fsm:start_link(?MODULE, {Mode,
                                 #state{channel_id = ChannelId,
                                        owner = Owner,
                                        on_activation = undefined}}, []).



notify(Buffer) ->
    gen_fsm:send_event(Buffer, notify).

set_active(Buffer, TargBytes, Fun)
  when is_integer(TargBytes), TargBytes > 0,
       is_function(Fun, 1) ->
    gen_fsm:send_event(Buffer, {set_active, TargBytes, Fun}).

%% ------------------------------------------------------------------
%% Hand crafted API for humans.
%% ------------------------------------------------------------------

post(Buffer, Msg) ->
    Buffer ! {post, Msg}.

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

%% @private
init({Mode, S = #state{channel_id = ChannelId,
                       owner = Owner}})
  when Mode =:= notify orelse Mode =:= passive,
       is_pid(Owner), is_integer(ChannelId) ->
    logplex_channel:register({channel, ChannelId}),
    {ok, Mode, S}.


%% @private
active({set_active, TargBytes, Fun},
       S = #state{})
  when is_integer(TargBytes), TargBytes > 0,
       is_function(Fun, 1) ->
    {next_state, active, S#state{on_activation={TargBytes, Fun}}};
active(notify, S = #state{}) ->
    ?WARN("state=active error=duplicate_activation", []),
    {next_state, notify, S#state{on_activation=undefined}};
active(Msg, S = #state{}) ->
    ?WARN("state=active Unexpected msg ~p", [Msg]),
    {next_state, active, S}.


%% @private
passive(notify, S = #state{buf = Buf}) ->
    NewState = S#state{on_activation=undefined},
    case logplex_msg_buffer:empty(Buf) of
        empty ->
            {next_state, notify, NewState};
        not_empty ->
            send_notification(NewState)
    end;
passive({set_active, TargBytes, Fun}, S = #state{buf = Buf})
  when is_integer(TargBytes), TargBytes > 0,
       is_function(Fun, 1) ->
    NewState = S#state{on_activation={TargBytes, Fun}},
    case logplex_msg_buffer:empty(Buf) of
        empty ->
            {next_state, active, NewState};
        not_empty ->
            send(NewState)
    end;
passive(Msg, S = #state{}) ->
    ?WARN("state=passive Unexpected msg ~p", [Msg]),
    {next_state, passive, S}.


%% @private
notify({set_active, TargBytes, Fun},
       S = #state{})
  when is_integer(TargBytes), TargBytes > 0,
       is_function(Fun, 1) ->
    {next_state, active, S#state{on_activation={TargBytes, Fun}}};
notify(notify, S = #state{}) ->
    ?WARN("state=notify error=duplicate_notification", []),
    {next_state, notify, S};
notify(Msg, S = #state{}) ->
    ?WARN("state=notify Unexpected msg ~p", [Msg]),
    {next_state, active, S}.



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
            send(NewState);
        notify ->
            send_notification(NewState)
    end;

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

%% @private
send_notification(S = #state{owner = Owner}) ->
    Owner ! {logplex_drain_buffer, self(), new_data},
    {next_state, passive, S}.

%% @private
send(S = #state{owner = Owner, buf = Buf,
                on_activation = {Targ, Fun}}) ->
    {Frame, Count, NewBuf} = logplex_msg_buffer:to_pkts(Buf, Targ, Fun),
    NewState = S#state{buf=NewBuf, on_activation=undefined},
    Owner ! {logplex_drain_buffer, self(), {frame, Frame, Count}},
    {next_state, passive, NewState}.
