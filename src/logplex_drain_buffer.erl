%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Capped size log message buffer with loss recording.
%% @end
-module(logplex_drain_buffer).

-record(lpdb, {messages = queue:new(),
               max_size = 1024 :: pos_integer(),
               loss_start = undefined :: 'undefined' | erlang:timestamp(),
               loss_count = 0 :: non_neg_integer()
              }).

-type msg() :: binary() | tuple().
-opaque buf() :: #lpdb{}.
-export_type([buf/0]).

-export([new/0
         ,new/1
         ,push/2
         ,push_ext/2
         ,pop/1
         ,to_list/1
         ]).

-include_lib("proper/include/proper.hrl").


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
                    {{loss_indication, N::non_neg_integer(),
                      When::erlang:timestamp()}}.
pop(Buf = #lpdb{loss_count = 0,
                messages = Q}) ->
    case queue:out(Q) of
        {empty, Q1} ->
            {empty, Buf#lpdb{messages = Q1}};
        {{value, Item}, Q2} ->
            {{msg, Item}, Buf#lpdb{messages = Q2}}
    end;
pop(Buf = #lpdb{loss_count = N,
                loss_start = When})
  when N > 0 ->
    {{loss_indication, N, When},
     Buf#lpdb{loss_count = 0,
              loss_start = undefined}}.

full(#lpdb{max_size = Max, messages = Q}) ->
    case queue:len(Q) of
        N when N >= Max ->
            full;
        N when N < Max ->
            have_space
    end.

to_list(#lpdb{messages = Q}) ->
    queue:to_list(Q).

insert(Msg, Buf = #lpdb{messages = Q}) ->
    Buf#lpdb{messages = queue:in(Msg, Q)}.

displace(Msg, Buf = #lpdb{messages = Q,
                          loss_count = 0}) ->
    {_Drop, Q1} = queue:out(Q),
    NewQueue = queue:in(Msg, Q1),
    Buf#lpdb{messages = NewQueue,
             loss_count = 1,
             loss_start = os:timestamp()};
displace(Msg, Buf = #lpdb{messages = Q,
                          loss_count = N}) when N > 0 ->
    {_Drop, Q1} = queue:out(Q),
    NewQueue = queue:in(Msg, Q1),
    Buf#lpdb{messages = NewQueue,
             loss_count = N + 1}.

prop_push_msgs() ->
    ?FORALL(MsgList, list(g_log_msg()),
            begin
                Buf = lists:foldl(fun push/2,
                                  new(),
                                  MsgList),
                lists:foldl(fun (Msg, B) ->
                                    {{msg, Msg}, B1} =  pop(B),
                                    B1
                            end,
                            Buf,
                            MsgList),
                true
            end).

g_log_msg() ->
    ?LET({F, S, D, M},
         {integer(0, 23), % Facility
          integer(0, 7), % severity
          integer(-86400, 86400),
          binary()},
         iolist_to_binary(io_lib:format("<~p>~p ~s ~s",
                                        [F, S, g_date(D), M]))).

g_date(Offset) ->
    Date = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(os:timestamp())),
    {{Y,M,D},{H,MM,S}} = calendar:gregorian_seconds_to_datetime(Date + Offset),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B"
                  "Z+00:00",
                  [Y,M,D, H,MM,S]).
