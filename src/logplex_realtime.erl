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

-export([incr/1, incr/2,
         incr_gauge/1, incr_gauge/2,
         decr_gauge/1, decr_gauge/2,
         set_gauge/2,
         setup_metrics/0]).

-include("logplex_logging.hrl").

%%====================================================================
%% API functions
%%====================================================================

incr(Key) ->
    incr(Key, 1).

-spec incr(atom(), pos_integer()) -> any().
incr(Key, Inc) when is_atom(Key), is_integer(Inc) ->
    incr_meter(convert_key(Key), Inc);

incr(_Key, _Inc) ->
    ok.

-spec incr_gauge(atom()) -> ok.
incr_gauge(Key) ->
    incr_gauge(Key, 1).

-spec incr_gauge(atom(), pos_integer()) -> ok.
incr_gauge(Key, Inc) when is_atom(Key) ->
    % folsom counters are not monotonically increasing, so
    % when exporting treat them like gauges for librato
    folsom_metrics:notify(Key, {inc, Inc}, counter).

-spec decr_gauge(atom()) -> ok.
decr_gauge(Key) ->
    decr_gauge(Key, 1).

-spec decr_gauge(atom(), pos_integer()) -> ok.
decr_gauge(Key, Dec) when is_atom(Key) ->
    % folsom counters are not monotonically increasing, so
    % when exporting treat them like gauges for librato
    folsom_metrics:notify(Key, {dec, Dec}, counter).

-spec set_gauge(atom(), pos_integer()) -> ok.
set_gauge(Key, Val) when is_atom(Key) ->
    folsom_metrics:notify(Key, Val, gauge).

-spec incr_meter(atom(), pos_integer()) -> ok.
incr_meter(Key, Inc) when is_atom(Key) ->
    folsom_metrics:notify(Key, Inc).

setup_metrics() ->
    [create_counter_metric(Key) || Key <- keys()].

%%====================================================================
%% Internal functions
%%====================================================================


-type key() :: 'message.received' |
               'message.processed' |
               'drain.delivered' |
               'drain.dropped'.
-spec keys() -> [key()].
keys() ->
    ['message.received',
     'message.processed',
     'drain.delivered',
     'drain.dropped'].

create_counter_metric(Key) ->
    handle_new_metric_reply(folsom_metrics:new_meter_reader(Key)).

handle_new_metric_reply(ok) ->
    ok;
handle_new_metric_reply({error, Key, metric_already_exists}) ->
    ?INFO("error=metric_already_exists metric=~p", [Key]),
    ok.

convert_key(message_received=Key) ->
    log_deprecated_key_usage(Key),
    'message.received';
convert_key(message_processed=Key) ->
    log_deprecated_key_usage(Key),
    'message.processed';
convert_key(drain_delivered=Key) ->
    log_deprecated_key_usage(Key),
    'drain.delivered';
convert_key(drain_dropped=Key) ->
    log_deprecated_key_usage(Key),
    'drain.dropped';
convert_key(Key) when is_atom(Key) ->
    Key.

log_deprecated_key_usage(Key) ->
    ?INFO("deprecated-key-usage key=~p", [Key]).
