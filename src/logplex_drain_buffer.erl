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

-type msg() :: binary().
-opaque lp_msg_buffer() :: #lpdb{}.

-export([new/0
         ,push/2
         ,pop/1
         ]).


-spec new() -> lp_msg_buffer().
new() ->
    #lpdb{}.

-spec push(msg(), lp_msg_buffer()) -> lp_msg_buffer().
push(Msg, Buf = #lpdb{}) ->
    case full(Buf) of
        full ->
            displace(Msg, Buf);
        have_space ->
            insert(Msg, Buf)
    end.

-spec pop(lp_msg_buffer()) -> {empty, lp_msg_buffer()} |
                              {{msg, msg()}, lp_msg_buffer()} |
                              {{loss_indication, N::non_neg_integer(),
                                When::erlang:timestamp()}}.
pop(Buf = #lpdb{loss_count = 0,
                messages = Q}) ->
    case queue:out(Q) of
        {empty, Q1} ->
            {empty, Buf#lpdb{messages = Q1}};
        {Item, Q2} ->
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
