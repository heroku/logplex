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

-export([incr/1, incr/2, setup_metrics/0]).

%%====================================================================
%% API functions
%%====================================================================

incr(Key) ->
    incr(Key, 1).

-spec incr(string() | key(), integer()) -> any().
incr(Key, Inc) when is_atom(Key), is_integer(Inc) ->
    folsom_metrics:notify(Key, {inc, Inc}, counter);

incr(_Key, _Inc) ->
    ok.

setup_metrics() ->
    [ok = folsom_metrics:new_counter(Key) || Key <- keys()].

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
