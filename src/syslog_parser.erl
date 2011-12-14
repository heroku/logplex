%% @copyright Heroku 2011
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Re-entrant syslog tcp stream parser.
%% @end
-module(syslog_parser).

-export([new/0, push/2]).

-export([example1/0, parse/1]).


-define(SYSLOG_MAX_SIZE, 4192).

-record(buf, {bytes = <<>> :: binary(),
              waiting_for = unknown :: 'unknown' | non_neg_integer()}).

%% Syslog frames
%% a) <Length as ascii integer><space><msg:Length>"
%% b) <msg>\n.
%% ZOMGWTFBBQ? Two framing formats and they get switched between
%% randomly? #killmaimdestroy

example1() ->
    <<"1753626010 web.23 - - Started GET \"/default/payload\" for 69.179.15.165 at 2011-12-12 15:20:13 -0800\n218 <13>1 2011-12-12T23:20:13+00:00 runtime.60161@heroku.com t.fa298f04-b533-4a04-8b47-1e1753626010 web.9 - - cache: [GET /?a=gd&v=2&m=263100500876800&f=%2C21474836474ecc3d2c729cd7.34662728%2C1%2C3%2Cgrupo%2Cgrupov1] miss\n200 <158>1 2011-12-12T23:20:13+00:00 hermes-argon.60125@heroku.com t.12d76170-aa5c-4f25-8f3a-bf214f93d208 router - - POST p.pingme.net/v1/poll dyno=web.26 queue=0 wait=0ms service=61ms status=200 bytes=2\n202 <158>1 2011-12-12T23:20:13+00:00 hermes-argon.60125@heroku.com t.922a3f60-85bc-4d99-9d24-1a4c6caa7fb2 router - - GET d.lqw.me/delivery.js dyno=web.24 queue=0 wait=0ms service=83ms status=200 bytes=3187\n204 <158>1 2011-12-12T23:20:13+00:00 hermes-argon.60125@heroku.com t.1681f42f-75e0-484b-bdcd-4fd3b8487f92 router - - GET bid.tapengage.com/admeld dyno=web.13 queue=0 wait=0ms service=11ms status=200 bytes=53\n288 <13>1 2011-12-12T23:20:13+00:00 runtime.60159@heroku.com t.223ccc64-e9fb-4570-ba">>.

new() ->
    #buf{}.

parse(Bytes) when is_binary(Bytes) ->
    push(Bytes, new()).

push(Bytes, #buf{bytes=OldBuf, waiting_for=unknown})
  when is_binary(Bytes), is_binary(OldBuf) ->
    NewBuf = iolist_to_binary([OldBuf, Bytes]),
    parse_recursive(NewBuf, []);
push(Bytes, Buf = #buf{bytes=OldBuf, waiting_for=RequiredLength}) ->
    NewBuf = iolist_to_binary([OldBuf, Bytes]),
    case byte_size(NewBuf) >= RequiredLength of
        true ->
            parse_recursive(NewBuf, []);
        false ->
            {ok, [], Buf#buf{bytes=NewBuf}}
    end.


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

parse_to_nl(Buf) ->
    case binary:split(Buf, [<<"\r\n">>, <<"\n">>]) of
        [Msg, Rest] ->
            {{malformed, Msg}, Rest};
        [Buf] ->
            {incomplete, Buf}
    end.
