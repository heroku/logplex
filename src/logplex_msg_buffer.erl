%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Capped size log message buffer with loss recording.
%% @end
-module(logplex_msg_buffer).

-record(lpdb, {messages = queue:new() :: queue(),
               max_size = 1024 :: size(),
               size = 0,
               loss_start = undefined :: 'undefined' | erlang:timestamp(),
               loss_count = 0 :: non_neg_integer()
              }).
-define(OLDBUF, {lpdb, _, _, _, _}).

-type msg() :: binary() | tuple().
-type size() :: pos_integer().
-type loss_indication() :: {loss_indication,
                            N::non_neg_integer(),
                            When::erlang:timestamp()}.
-type framing_fun()::fun (({msg, msg()} | loss_indication()) ->
                                 {frame, iolist()} | skip).
-opaque buf() :: #lpdb{}.
-export_type([buf/0, framing_fun/0, size/0, loss_indication/0]).

-export([new/0
         ,new/1
         ,push/2
         ,push_ext/2
         ,len/1
         ,max_size/1
         ,full/1
         ,empty/1
         ,pop/1
         ,resize/2
         ,to_list/1
         ,from_list/1
         ,to_pkts/3
         ,lose/2
         ,lost/1
         ,drop/2
         ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec new() -> buf().
new() ->
    #lpdb{}.

-spec new(pos_integer()) -> buf().
new(Max) when is_integer(Max), Max > 0 ->
    #lpdb{max_size=Max}.

-spec push(msg(), buf()) -> buf().
push(Msg, Buf = #lpdb{}) ->
    {_, NewBuf} = push_ext(Msg, Buf),
    NewBuf.

-spec push_ext(msg(), buf()) -> {'displace' | 'insert', buf()}.
push_ext(Msg, Buf = #lpdb{}) ->
    case full(Buf) of
        full ->
            {displace, displace(Msg, Buf)};
        have_space ->
            {insert, insert(Msg, Buf)}
    end.

-spec pop(buf()) -> {empty, buf()} |
                    {{msg, msg()}, buf()} |
                    {loss_indication(), buf()}.
pop(Buf = #lpdb{loss_count = 0,
                size=S,
                messages = Q}) ->
    case queue:out(Q) of
        {empty, Q1} ->
            {empty, Buf#lpdb{messages = Q1}};
        {{value, Item}, Q2} ->
            {{msg, Item}, Buf#lpdb{messages = Q2, size=S-1}}
    end;
pop(Buf = #lpdb{loss_count = N,
                loss_start = When})
  when N > 0 ->
    {{loss_indication, N, When},
     Buf#lpdb{loss_count = 0,
              loss_start = undefined}}.

full(#lpdb{max_size = Max, size = N}) when N >= Max -> full;
full(#lpdb{}) -> have_space.

empty(#lpdb{size = 0}) -> empty;
empty(#lpdb{}) -> not_empty.

len(#lpdb{size=Len}) -> Len.

max_size(#lpdb{max_size=Max}) -> Max.

-spec to_list(buf()) -> [msg()].
to_list(#lpdb{messages = Q,
              loss_count = 0}) ->
    queue:to_list(Q);
to_list(#lpdb{messages = Q,
              loss_count = N,
              loss_start = When})
  when N > 0 ->
    [{loss_indication, N, When} |
     queue:to_list(Q)].

-spec from_list([msg()]) -> #lpdb{}.
from_list(Msgs) ->
    #lpdb{messages = queue:from_list(Msgs), size = length(Msgs)}.

insert(Msg, Buf = #lpdb{messages = Q, size = Len}) ->
    Buf#lpdb{messages = queue:in(Msg, Q), size = Len+1}.

displace(Msg, Buf = #lpdb{}) ->
    insert(Msg, lose(1, drop(1, Buf))).

-ifdef(TEST).

displace_test_() ->
    [ ?_assertMatch([{loss_indication, 1, _}, <<"two">>],
                    to_list(displace(<<"two">>,
                                     insert([<<"one">>], new(1))))),
      ?_assertMatch([{loss_indication, 2, _}, <<"three">>],
                    to_list(displace(<<"three">>,
                                     displace(<<"two">>,
                                              insert([<<"one">>], new(1))))))
    ].

-endif.

-spec drop(Count::non_neg_integer(), buf()) -> buf().
drop(0, Buf = #lpdb{}) -> Buf;
drop(1, Buf = #lpdb{messages = OldQueue, size=Size}) ->
    {_, NewQueue} = queue:out(OldQueue),
    Buf#lpdb{messages = NewQueue, size=Size-1};
drop(N, Buf = #lpdb{messages = Queue, size=Size})
  when is_integer(N), N >= 0 ->
    {NewQueue,NewSize} = if Size >= N ->
                                {_, Queue1} = queue:split(N, Queue),
                                {Queue1, Size-N};
                            Size < N -> % Trying to drop all (or more) items in queue
                                {queue:new(),0}
                         end,
    Buf#lpdb{messages = NewQueue, size=NewSize}.

-ifdef(TEST).

drop_test_() ->
    M = 10,
    Messages = [ list_to_binary(integer_to_list(N)) || N <- lists:seq(1,M)],
    [ ?_assertMatch(L when M - length(L) =:= N,
                    to_list(drop(N, from_list(Messages))))
      || N <- lists:seq(1, M) ].

-endif.

%% lose(Buf) -> lose(os:timestamp(), 1, Buf).
lose(Count, Buf) -> lose(os:timestamp(), Count, Buf).

-spec lose(erlang:timestamp(), non_neg_integer(), buf()) -> buf().

lose(_Time, 0, Buf = #lpdb{}) -> Buf;
lose(Time = {_,_,_}, Count, Buf = #lpdb{loss_count=0})
  when Count > 0 ->
    Buf#lpdb{loss_count=Count,
             loss_start=Time};
lose(_Time, NewCount, Buf = #lpdb{loss_count=OldCount})
  when NewCount > 0, is_integer(OldCount) ->
    Buf#lpdb{loss_count=NewCount + OldCount}.

-ifdef(TEST).
lose_test_() ->
    [ ?_assertMatch(#lpdb{loss_count = L} when L =:= N,
                    lose(N, new(1)))
      || N <- lists:seq(1,10)
    ].

lose2_test_() ->
    [ ?_assertMatch([{loss_indication, 1, _}],
                    to_list(lose(1, new(1))))
    ].
-endif.

-spec lost(buf()) -> non_neg_integer().
lost(#lpdb{loss_count=N}) -> N.

-spec to_pkts(buf(), IdealBytes::pos_integer(),
              framing_fun()) ->
                     {iolist(), Count::non_neg_integer(), buf()}.
to_pkts(Buf = #lpdb{},
        Bytes, Fun) when is_integer(Bytes),
                         is_function(Fun, 1) ->
    to_pkts(Buf, Bytes, Bytes, Fun).

to_pkts(Buf, BytesTotal, BytesRemaining, Fun)
  when BytesRemaining > 0 ->
    {Item, NewBuf} = pop(Buf),
    Msg = case Item of
              empty ->
                  finished;
              {loss_indication, _N, _When} ->
                  Fun(Item);
              {msg, _M} ->
                  Fun(Item)
          end,
    case Msg of
        finished ->
            {[], 0, NewBuf};
        skip ->
            to_pkts(NewBuf, BytesTotal, BytesRemaining, Fun);
        {frame, Data} ->
            DataSize = iolist_size(Data),
            case BytesRemaining - DataSize of
                Remaining when Remaining > 0 ->
                    {Rest, Count, FinalBuf} = to_pkts(NewBuf,
                                                      BytesTotal,
                                                      Remaining,
                                                      Fun),
                    {[Data, Rest], Count + 1, FinalBuf};
                _ when DataSize >= BytesTotal ->
                    %% We will exceed bytes remaining, but this
                    %% message is a pig, so send it anyway.
                    {Data, 1, NewBuf};
                _ ->
                    %% Would have exceeded BytesRemaining, pretend we
                    %% didn't pop it.
                    {[], 0, Buf}
            end
    end.

resize(NewSize, Buf = #lpdb{max_size=OldSize})
  when is_integer(NewSize),
       NewSize > 0,
       NewSize >= OldSize ->
    Buf#lpdb{max_size=NewSize};
resize(NewSize, Buf = #lpdb{size=Len})
  when is_integer(NewSize),
       NewSize > 0 ->
    case Len - NewSize of
        ToDrop when ToDrop > 0 ->
            lose(ToDrop,
                 drop(ToDrop, Buf#lpdb{max_size=NewSize}));
        _ ->
            Buf#lpdb{max_size=NewSize}
    end.

-ifdef(TEST).

resize_test_() ->
    Messages = [<<"msg 1">>, <<"msg 2">>, <<"msg 3">>],
    [ ?_assertMatch(List when List =:= Messages,
                    to_list(from_list(Messages))),
      ?_assertMatch(List when List =:= Messages,
                    to_list(resize(3, from_list(Messages)))),
      ?_assertMatch(List when List =/= Messages andalso
                              length(List) =:= length(Messages) andalso
                              tl(List) =:= tl(Messages),
                    to_list(resize(2, from_list(Messages))))
    ].

-endif.
