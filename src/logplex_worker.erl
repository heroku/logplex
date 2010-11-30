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
-module(logplex_worker).
-export([start_link/0, init/1, loop/1]).

-include_lib("logplex.hrl").

-record(state, {regexp}).

%% API functions
start_link() ->
    proc_lib:start_link(?MODULE, init, [self()], 5000).

init(Parent) ->
    {ok, RE} = re:compile("^<\\d+>\\S+ \\S+ \\S+ (t[.]\\S+) "),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{regexp=RE}).

loop(#state{regexp=RE}=State) ->
    case logplex_queue:out() of
        undefined -> timer:sleep(10);
        Msg ->
            case re:run(Msg, RE, [{capture, all_but_first, list}]) of
                {match, [Token]} ->
                    route(list_to_binary(Token), Msg);
                _ ->
                    ok
            end
    end,
    loop(State).

route(Token, Msg) when is_binary(Token), is_binary(Msg) ->
    case logplex_token:lookup(Token) of
        #token{channel_id=ChannelId, name=TokenName, addon=Addon} ->
            Count = logplex_stats:incr(ChannelId),
            case exceeded_threshold(Count, Addon) of
                true ->
                    ok;
                notify ->
                    {{Year,Month,Day},{Hour,Min,Sec}} = Local = erlang:localtime(),
                    UTC = erlang:universaltime(),
                    {_, {Offset, _, _}} = calendar:time_difference(Local, UTC),
                    Msg1 = iolist_to_binary(io_lib:format("<40>1 ~w-~w-~wT~w:~w:~w-0~w:00 - heroku logplex - - You have exceeded ~w logs/min. Please upgrade your logging addon for higher throughput.", [Year, Month, Day, Hour, Min, Sec, Offset, throughput(Addon)])),
                    process(ChannelId, Msg1);
                false ->
                    Msg1 = re:replace(Msg, Token, TokenName),
                    Msg2 = iolist_to_binary(Msg1),
                    process(ChannelId, Msg2)
            end;
        _ ->
            ok
    end.

process(ChannelId, Msg) ->
    logplex_tail:route(ChannelId, Msg),
    [logplex_drain_pool:route(Host, Port, Msg) || #drain{host=Host, port=Port} <- logplex_channel:drains(ChannelId)],
    logplex_buffer:in(redis_helper:build_push_msg(ChannelId, Msg)).

throughput(<<"basic">>) -> ?BASIC_THROUGHPUT;
throughput(<<"expanded">>) -> ?EXPANDED_THROUGHPUT.

exceeded_threshold(_, <<"advanced">>) -> false;
exceeded_threshold(Count, <<"expanded">>) when Count =< ?EXPANDED_THROUGHPUT -> false;
exceeded_threshold(Count, <<"expanded">>) when Count == (?EXPANDED_THROUGHPUT + 1) -> notify;
exceeded_threshold(Count, <<"basic">>) when Count =< ?BASIC_THROUGHPUT -> false;
exceeded_threshold(Count, <<"basic">>) when Count == (?BASIC_THROUGHPUT + 1) -> notify;
exceeded_threshold(_, _) -> true.