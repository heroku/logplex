%% Copyright (c) 2014 Alex Arnell <alex.arnell@gmail.com>
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
-module(logplex_alarm_handler).
-behaviour(gen_event).

-export([boot_alarm_handler/0,
         clear_alarm/1,
         get_alarms/0,
         set_alarm/1]).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-include("logplex_logging.hrl").

-record(state, {silent=true,
                alarms=[]}).

boot_alarm_handler() ->
    gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {?MODULE, []}).

get_alarms() ->
    gen_event:call(alarm_handler, logplex_alarm_handler, get_alarms).

set_alarm(Alarm) when is_tuple(Alarm) ->
    alarm_handler:set_alarm(Alarm);
set_alarm(AlarmId) ->
    set_alarm({AlarmId, undefined}).

clear_alarm(AlarmId) ->
    alarm_handler:clear_alarm(AlarmId).

init({[], _OldArgs}) ->
    State = case logplex_app:config(pagerduty_key, undefined) of
        undefined -> #state{};
        ServiceKey ->
            ok = application:load(pagerduty),
            application:set_env(pagerduty, service_key, ServiceKey),
            logplex_app:a_start(pagerduty, temporary),
            #state{ silent=false }
    end,
    ?INFO("at=init silent=~p", [State#state.silent]),
    {ok, State}.

handle_event({set_alarm, Alarm}, State=#state{ alarms=Alarms }) ->
    error_logger:info_report([{alarm_handler, {set, Alarm}}]),
    trigger_page(State#state.silent, Alarm),
    {ok, State#state{ alarms=[Alarm | Alarms] }};
handle_event({clear_alarm, AlarmId}, State=#state{ alarms=Alarms }) ->
    error_logger:info_report([{alarm_handler, {clear, AlarmId}}]),
    NewAlarms = case lists:keydelete(AlarmId, 1, Alarms) of
                    Alarms -> Alarms;
                    Trimmed ->
                        case lists:keyfind(AlarmId, 1, Trimmed) of
                            {AlarmId, _} -> skip;
                            false ->
                                resolve_page(State#state.silent, AlarmId)
                        end,
                        Trimmed
                end,
    {ok, State#state{ alarms=NewAlarms }}.

handle_call(get_alarms, State) ->
    {ok, State#state.alarms, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% private functions
trigger_page(Silent, {AlarmId, AlarmDesc}) ->
    ?ALARM("trigger silent=~p alarm_id=~p alarm_desc=~p", [Silent, AlarmId, AlarmDesc]),
    Silent orelse pagerduty:trigger("Logplex", to_bin(fmt_desc(AlarmId)), AlarmDesc).

resolve_page(SilentOnAlarm, AlarmId) ->
    ?ALARM("resolve silent=~p alarm_id=~p", [SilentOnAlarm, AlarmId]),
    SilentOnAlarm orelse pagerduty:resolve("Logplex", to_bin(fmt_desc(AlarmId))).

fmt_desc(Format) ->
    io_lib:format("~p: " ++ Format, [instance_name()]).

to_bin(IOList) when is_list(IOList) ->
    iolist_to_binary(IOList).

instance_name() ->
    logplex_app:config(instance_name).
