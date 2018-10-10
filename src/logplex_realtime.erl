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

-export([incr/1, incr/2, setup_metrics/0, create_counter_metric/1]).

-include("logplex_logging.hrl").

%%====================================================================
%% API functions
%%====================================================================

incr(Key) ->
    incr(Key, 1).

-spec incr(string() | key(), integer()) -> any().
incr(Key, Inc) when is_atom(Key), is_integer(Inc) ->
    folsom_metrics:notify(convert_key(Key), {inc, Inc}, counter);

incr(_Key, _Inc) ->
    ok.

setup_metrics() ->
    [create_counter_metric(Key) || Key <- keys()].

%%====================================================================
%% Internal functions
%%====================================================================

-type key() :: 'message.received' |
               'message.processed' |
               'drain.delivered' |
               'drain.dropped' |
               'message_batch.processed'|
               'message_batch.failed'.
-spec keys() -> [key()].
keys() ->
    ['message.received',
     'message.processed',
     'drain.delivered',
     'drain.dropped',
     'message_batch.processed',
     'message_batch.failed'
    ].

create_counter_metric(Key) ->
    handle_new_metric_reply(folsom_metrics:new_counter(Key)).

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
convert_key(Key) ->
    Key.

log_deprecated_key_usage(Key) ->
    ?INFO("deprecated-key-usage key=~p", [Key]).
