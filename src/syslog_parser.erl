%% @copyright Heroku 2011
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Re-entrant syslog tcp stream parser.
%% @end
-module(syslog_parser).

-export([new/0, push/2]).

-export([parse/1]).


-define(SYSLOG_MAX_SIZE, 10240). % 10Kb max in a single log message.

-record(buf, {bytes = <<>> :: binary(),
              waiting_for = unknown :: 'unknown' | non_neg_integer()}).

-opaque parse_buffer() :: #buf{}.
-type syslog_message() :: {msg, binary()} | {malformed, binary()}.
-type syslog_messages() :: [syslog_message()].

-export_type([parse_buffer/0]).

%% Syslog frames
%% a) <Length as ascii integer><space><msg:Length>"
%% b) <msg>\n.
%% ZOMGWTFBBQ? Two framing formats and they get switched between
%% randomly? #killmaimdestroy

new() ->
    #buf{}.

parse(Bytes) when is_binary(Bytes) ->
    push(Bytes, new()).

-spec push(Bytes::binary(), parse_buffer()) ->
                  { ok | {error, term()},
                    syslog_messages(),
                    parse_buffer()}.

push(Bytes, Buf = #buf{bytes=OldBuf, waiting_for=WF})
  when is_binary(Bytes) ->
    push(iolist_to_binary([OldBuf, Bytes]), WF, Buf).

push(Bytes, RequiredLength, Buf)
  when is_integer(RequiredLength),
       byte_size(Bytes) < RequiredLength ->
    %% Haven't accumulated enough bytes to complete a parse yet.
    {ok, [], Buf#buf{bytes=Bytes}};
push(Bytes, _, _) ->
    parse_recursive(Bytes, []).


-spec parse_recursive(binary(), Acc::syslog_messages()) ->
                             {'ok' | {'error', term()},
                              syslog_messages(), parse_buffer()}.
parse_recursive(<<>>, Acc) ->
    {ok, lists:reverse(Acc), #buf{}};
parse_recursive(Buf, Acc) ->
    case parse_beginning(Buf) of
        {incomplete, N, Rest} ->
            {ok, lists:reverse(Acc), #buf{bytes=Rest, waiting_for=N}};
        {_Msg, Buf} ->
            {{error, {parser_made_no_progress, Buf}},
             lists:reverse(Acc), #buf{}};
        {Msg, Rest} ->
            parse_recursive(Rest, [Msg | Acc])
    end.

%% Can only be used at the beginning of a message. Will give bogus
%% results otherwise.
-spec msg_type(Buf::binary()) -> {'length_prefixed',
                                  Length::non_neg_integer(),
                                  Offset::non_neg_integer()} |
                                 'nl_terminated' |
                                 'incomplete'.
msg_type(<<FirstChar, _/binary>> = Buf)
  when $1 =< FirstChar, FirstChar =< $9 ->
    case find_int(Buf, 0) of
        {Len, Offset} when Len =< ?SYSLOG_MAX_SIZE ->
            {length_prefixed, Len, Offset};
        {Len, _} when Len > ?SYSLOG_MAX_SIZE ->
            nl_terminated;
        bad_int_or_missing_space ->
            nl_terminated;
        incomplete ->
            incomplete
    end;
msg_type(_) ->
    nl_terminated.

-spec parse_beginning(binary()) ->
                             {syslog_message(), Remaining::binary()} |
                             {'incomplete', 'unknown' | non_neg_integer(),
                              Remaining::binary()}.
parse_beginning(Buf) ->
    case msg_type(Buf) of
        {length_prefixed, Len, Offset} ->
            RequiredLength = Offset + Len,
            if byte_size(Buf) >= RequiredLength ->
                    <<_:Offset/binary,
                      Msg:Len/binary,
                      Rest/binary>> = Buf,
                    {{msg, Msg}, Rest};
               true ->
                    {incomplete, RequiredLength - byte_size(Buf), Buf}
            end;
        incomplete ->
            {incomplete, unknown, Buf};
        nl_terminated ->
            parse_to_nl(Buf)
    end.

%% Idx 0 must be 1-9.
%% Finds the ascii encoded integer terminated by a space.
-spec find_int(binary(), non_neg_integer()) ->
                      'incomplete' |
                      {Length::non_neg_integer(),
                       MsgOffset::non_neg_integer()} |
                      'bad_int_or_missing_space'.
find_int(Buf, Idx) when byte_size(Buf) =< Idx ->
    incomplete;
find_int(Buf, Idx) ->
    case binary:at(Buf, Idx) of
        C when $0 =< C, C =< $9 ->
            find_int(Buf, Idx + 1);
        $\s ->
            {list_to_integer(binary:bin_to_list(Buf, 0, Idx)), Idx + 1};
        _ ->
            bad_int_or_missing_space
    end.

-spec parse_to_nl(binary()) -> {'incomplete', 'unknown', binary()} |
                               {{'malformed', binary()}, Rest::binary()}.
parse_to_nl(Buf) ->
    case binary:split(Buf, [<<"\r\n">>, <<"\n">>]) of
        [Msg, Rest] ->
            {{malformed, Msg}, Rest};
        [Buf] ->
            {incomplete, unknown, Buf}
    end.
