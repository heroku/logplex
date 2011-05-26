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
-module(logplex_db).
-export([setup/0, dump/0]).

-include_lib("logplex.hrl").

setup() ->
    create_ets_tables(),
    NumObjs = open_dets_tables(),
    populate_ets([channels, tokens, drains]),
    spawn_link(fun sync_dets/0),
    NumObjs > 0.

create_ets_tables() ->
    ets:new(channels, [named_table, public, set, {keypos, 2}]),
    ets:new(tokens,   [named_table, public, set, {keypos, 2}]),
    ets:new(drains,   [named_table, public, set, {keypos, 2}]),
    ets:new(sessions, [named_table, public, set, {keypos, 2}]),
    ok.

open_dets_tables() ->
    {ok, _} = dets:open_file(channels, [{keypos, 2}, {type, set}]),
    {ok, _} = dets:open_file(tokens,   [{keypos, 2}, {type, set}]),
    {ok, _} = dets:open_file(drains,   [{keypos, 2}, {type, set}]),
    lists:foldl(fun(Name, Acc) -> dets:info(Name, no_objects) + Acc end, 0, [channels, tokens, drains]).

populate_ets([]) -> ok;

populate_ets([Name|Rest]) ->
    Name = dets:to_ets(Name, Name),
    populate_ets(Rest).

sync_dets() ->
    timer:sleep(10000),
    dump(),   
    sync_dets().

dump() ->
    dets:from_ets(channels, channels), dets:sync(channels),
    dets:from_ets(tokens, tokens), dets:sync(tokens),
    dets:from_ets(drains, drains), dets:sync(drains),
    ok.
