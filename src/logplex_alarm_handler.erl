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

-record(state, {active=false,
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
            #state{ active=true }
    end,
    ?INFO("at=init paging=~p", [State#state.active]),
    {ok, State}.

handle_event({set_alarm, Alarm}, State=#state{ alarms=Alarms0 }) ->
    error_logger:info_report([{alarm_handler, {set, Alarm}}]),
    State#state.active andalso trigger_page(Alarm),
    {ok, State#state{ alarms=[Alarm | Alarms0]}};
handle_event({clear_alarm, AlarmId}, State0=#state{ alarms=Alarms0 }) ->
    error_logger:info_report([{alarm_handler, {clear, AlarmId}}]),
    Alarms = resolve_page(AlarmId, lists:keydelete(AlarmId, 1, Alarms0)),
    {ok, State0#state{ alarms=Alarms }}.

handle_call(get_alarms, State) ->
    {ok, State#state.alarms, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% private functions
trigger_page({AlarmId, AlarmDesc}) ->
    pagerduty:trigger("Logplex", to_bin(fmt_desc(AlarmId)), AlarmDesc).

resolve_page(AlarmId, Alarms) ->
    case lists:keyfind(AlarmId, 1, Alarms) of
        true -> ok;
        false ->
            %% pagerduty:resolve("Logplex", to_bin(fmt_desc(AlarmId)), AlarmDesc),
            ok
    end,
    Alarms.

fmt_desc(Format) ->
    io_lib:format("~p: " ++ Format, [instance_name()]).

to_bin(IOList) when is_list(IOList) ->
    iolist_to_binary(IOList).

instance_name() ->
    logplex_app:config(instance_name).
