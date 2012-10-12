%% Copyright (c) 2012 Heroku <nem@erlang.geek.nz>
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
-module(logplex_http_drain).

-include("logplex.hrl").
-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                uri :: #ex_uri{},
                buf :: pid(),
                client :: cowboy_client:client(),
                out_q = queue:new() :: queue(),
                reconnect_tref :: reference() | 'undefined'
               }).

-record(frame, {frame :: iolist(),
                msg_count :: non_neg_integer(),
                tries = 0 :: non_neg_integer(),
                id :: binary()
               }).

-define(CONTENT_TYPE, <<"application/logplex-1">>).
-define(HTTP_VERSION, {1,1}).
-define(RECONNECT_MSG, reconnect).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([valid_uri/1
         ,start_link/4
        ]).

-export([user_agent/0
        ]).

%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([disconnected/2,
         connected/2
        ]).

-export([init/1,  handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ChannelID, DrainID, DrainTok,
           Uri) ->
    gen_fsm:start_link(?MODULE,
                       #state{drain_id=DrainID,
                              drain_tok=DrainTok,
                              channel_id=ChannelID,
                              uri = Uri},
                       []).

valid_uri({Scheme, _, _, _, _, _} = Uri)
  when Scheme =:= http orelse Scheme =:= https ->
    {valid, http, Uri};
valid_uri(_) ->
    {error, invalid_http_uri}.

user_agent() ->
    [<<"Logplex">>, $/, logplex_app:config(git_branch)].

%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

%% @private
init(State0 = #state{uri=URI,
                     drain_id=DrainId,
                     channel_id=ChannelId}) ->
    try
        Dest = uri_to_string(URI),
        Size = logplex_app:config(http_drain_buffer_size, 1024),
        {ok, Buf} = logplex_drain_buffer:start_link(ChannelId, self(),
                                                    notify, Size),
        logplex_drain:register(DrainId, http, Dest),
        State = State0#state{buf = Buf},
        ?INFO("drain_id=~p channel_id=~p dest=~s at=spawn",
              log_info(State, [])),
        {ok, disconnected,
         State, hibernate}
    catch
        error:badarg -> ignore
    end.

%% @private
disconnected({logplex_drain_buffer, Buf, new_data},
             State = #state{buf = Buf}) ->
    try_connect(cancel_reconnect(State));
disconnected({logplex_drain_buffer, Buf, {frame, Frame, MsgCount}},
             State = #state{buf = Buf}) ->
    try_connect(cancel_reconnect(push_frame(Frame, MsgCount, State)));
disconnected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=disconnected",
          log_info(State, [Msg])),
    {next_state, disconnected, State}.

%% @private
connected({logplex_drain_buffer, Buf, {frame, Frame, MsgCount}},
          State = #state{buf = Buf}) ->
    ready_to_send(push_frame(Frame, MsgCount, State));
connected({logplex_drain_buffer, Buf, new_data},
          State = #state{buf = Buf}) ->
    ready_to_send(State);
connected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=~p state=connected",
          log_info(State, [Msg])),
    {next_state, connected, State}.

%% @private
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%% @private
handle_sync_event(notify, _From, StateName, State = #state{buf = Buf}) ->
    logplex_drain_buffer:notify(Buf),
    {reply, ok, StateName, State};
handle_sync_event(Event, _From, StateName, State) ->
    ?WARN("[state ~p] Unexpected event ~p",
          [StateName, Event]),
    {next_state, StateName, State}.

%% @private
handle_info({timeout, Ref, ?RECONNECT_MSG}, disconnected,
            State = #state{reconnect_tref = Ref,
                           buf = Buf,
                           out_q = Q}) ->
    NewState = State#state{reconnect_tref=undefined},
    case queue:is_empty(Q) of
        true ->
            logplex_drain_buffer:notify(Buf),
            {next_state, disconnected, NewState};
        false ->
            try_connect(NewState)
    end;
handle_info({timeout, Ref, ?RECONNECT_MSG}, StateName,
            State = #state{reconnect_tref = Ref}) ->
    {next_state, StateName, State#state{reconnect_tref=undefined}};
handle_info({timeout, _Ref, ?RECONNECT_MSG}, StateName,
            State = #state{}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s at=reconnect_timeout "
          "err=invalid_timeout state=~p",
          log_info(State, [StateName])),
    {next_state, StateName, State};

handle_info(shutdown, _StateName, State) ->
    {stop, shutdown, State};
handle_info(Info, StateName, State) ->
    ?MODULE:StateName(Info, State).



%% @private
try_connect(State = #state{uri=Uri,
                           client=undefined}) ->
    {ok, Client0} = cowboy_client:init([]),
    {Scheme, Host, Port} = connection_info(Uri),
    try cowboy_client:connect(scheme_to_transport(Scheme),
                              Host, Port, Client0) of
        {ok, Client} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=try_connect "
                  "attempt=success",
                  log_info(State, [])),
            ready_to_send(State#state{client=Client});
        {error, Why} ->
            ?WARN("drain_id=~p channel_id=~p dest=~s at=try_connect "
                  "attempt=fail tcp_err=~p",
                  log_info(State, [Why])),
            http_fail(State)
    catch
        Class:Err ->
            Report = {Class, Err, erlang:get_stacktrace()},
            ?WARN("drain_id=~p channel_id=~p dest=~s at=connect "
                  "attempt=fail err=exception data=~p next_state=disconnected",
                  log_info(State, [Report])),
            http_fail(State)
    end.

%% @private
http_fail(State = #state{}) ->
    %% XXX - forcibly shutdown client if necessary.
    {next_state, disconnected,
     set_reconnect_timer(State#state{client=undefined})}.

%% @private
ready_to_send(State = #state{buf = Buf, drain_tok=Token,
                             out_q = Q}) ->
    case queue:out(Q) of
        {empty, Q} ->
            logplex_drain_buffer:set_active(Buf, target_bytes(),
                                            framing_fun(Token)),
            {next_state, connected, State};
        {{value, Frame}, Q2} ->
            try_send(Frame, State#state{out_q = Q2})
    end.

try_send(Frame = #frame{tries = Tries},
         State = #state{client = Client})
  when Tries > 0 ->
    Req = request_to_iolist(Frame, State),
    try cowboy_client:raw_request(Req, Client) of
        {ok, Client2} ->
            wait_response(Frame, State#state{client=Client2});
        {error, Why} ->
            ?WARN("drain_id=~p channel_id=~p dest=~s at=send_request"
                  " tcp_err=~1000p",
                  log_info(State, [Why])),
            http_fail(retry_frame(Frame, State))
    catch
        Class:Err ->
            Report = {Class, Err, erlang:get_stacktrace()},
            ?WARN("drain_id=~p channel_id=~p dest=~s at=send_request "
                  "attempt=fail err=exception data=~p next_state=disconnected",
                  log_info(State, [Report])),
            http_fail(retry_frame(Frame,State))
    end;
try_send(Frame = #frame{tries = 0, msg_count=C}, State = #state{}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s at=try_send result=tries_exceeded "
          "frame_tries=0 dropped_msgs=~p",
          log_info(State, [C])),
    ready_to_send(drop_frame(Frame, State)).

%% @private Decide what happened to the frame based on the http status
%% code. Back of the napkin algorithm - 2xx is success, 4xx (client
%% errors) are perm failures, so drop the frame and anything else is a
%% temp failure, so retry the frame.
status_action(N) when 200 =< N, N < 300 -> success;
status_action(N) when 400 =< N, N < 500 -> perm_fail;
status_action(_) -> temp_fail.

wait_response(Frame = #frame{},
              State = #state{client = Client}) ->
    try cowboy_client:response(Client) of
        {ok, Status, _Headers, Client2} ->
            Result = status_action(Status),
            ?INFO("drain_id=~p channel_id=~p dest=~s at=response "
                  "result=~p status=~p msg_count=~p",
                  log_info(State, [Result, Status, Frame#frame.msg_count])),
            case Result of
                success ->
                    ready_to_send(sent_frame(Frame,
                                             State#state{client = Client2}));
                temp_fail ->
                    cowboy_client:close(Client2),
                    http_fail(retry_frame(Frame,
                                          State#state{client = Client2}));
                perm_fail ->
                    ready_to_send(drop_frame(Frame,
                                             State#state{client = Client2}))
            end;
        {error, Why} ->
            ?WARN("drain_id=~p channel_id=~p dest=~s at=wait_response"
                  " result=error tcp_err=~10000p",
                  log_info(State, [Why])),
            http_fail(retry_frame(Frame, State))
    catch
        Class:Err ->
            Report = {Class, Err, erlang:get_stacktrace()},
            catch cowboy_client:close(Client),
            ?WARN("drain_id=~p channel_id=~p dest=~s at=wait_response "
                  "attempt=fail err=exception data=~p next_state=disconnected",
                  log_info(State, [Report])),
            http_fail(retry_frame(Frame,State))
    end.

%% @private
terminate(_Reason, _StateName, _State) ->
    ok.

%% @private
code_change("v49", StateName, State, _Extra) when tuple_size(State) =:= 8 ->
    %% Adds new field, reconnect_tref - default value: undefined.
    NewState = list_to_tuple(tuple_to_list(State) ++ [undefined]),
    {ok, StateName, NewState};
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% @private
log_info(#state{drain_id=DrainId, channel_id=ChannelId, uri=URI}, Rest)
  when is_list(Rest) ->
    [DrainId, ChannelId, uri_to_string(URI) | Rest].

%% @private
scheme_to_transport("https") -> cowboy_ssl_transport;
scheme_to_transport("http") -> cowboy_tcp_transport.

%% @private
request_to_iolist(#frame{frame = Body,
                         msg_count = Count,
                         id = Id},
                  #state{uri = URI = #ex_uri{},
                         drain_tok = Token}) ->
    AuthHeader = auth_header(URI),
    MD5Header = case logplex_app:config(http_body_checksum, none) of
                    md5 -> [{<<"Content-MD5">>,
                             base64:encode(crypto:md5(Body))}];
                    none -> []
                end,
    Headers = MD5Header ++ AuthHeader ++
        [{<<"Content-Type">>, ?CONTENT_TYPE},
         {<<"Logplex-Msg-Count">>, integer_to_list(Count)},
         {<<"Logplex-Frame-Id">>, frame_id_to_iolist(Id)},
         {<<"Logplex-Drain-Token">>, Token},
         {<<"User-Agent">>, user_agent()}
        ],
    cowboy_client:request_to_iolist(<<"POST">>,
                                    Headers,
                                    Body,
                                    ?HTTP_VERSION,
                                    full_host_iolist(URI),
                                    uri_ref(URI)).

framing_fun(Token) ->
    Frame = fun ({Facility, Severity, Time, Source, Ps, Content}) ->
                    SyslogMsg = logplex_syslog_utils:rfc5424(Facility, Severity,
                                                             Time, Source, Ps,
                                                             Token, undefined,
                                                             Content),
                    logplex_syslog_utils:frame(SyslogMsg)
            end,
    fun ({loss_indication, N, When}) ->
            case logplex_app:config(http_send_loss_msg, send) of
                dont_send ->
                    skip;
                _ ->
                    {frame,
                     Frame(logplex_syslog_utils:overflow_msg(N, When))}
            end;
        ({msg, MData}) ->
            {frame, Frame(MData)}
    end.

-spec target_bytes() -> pos_integer().
%% @private
target_bytes() ->
    logplex_app:config(http_drain_target_bytes,
                       4096).

%% @private
%% @doc Called on frames we've decided to drop. Records count of
%% messages dropped (not frame count).
drop_frame(#frame{msg_count=Count}, State) ->
    msg_stat(drain_dropped, Count, State),
    State.

%% @private
sent_frame(#frame{msg_count=Count}, State) ->
    msg_stat(drain_delivered, Count, State),
    State.

-spec msg_stat('drain_dropped' | 'drain_buffered' | 'drain_delivered',
               non_neg_integer(), #state{}) -> any().
%% @private
msg_stat(Key, N,
         #state{drain_id=DrainId, channel_id=ChannelId}) ->
    logplex_stats:incr(#drain_stat{drain_id=DrainId,
                                   channel_id=ChannelId,
                                   key=Key}, N).

%% @private
%% Turn a Frame::iolist(), MsgCoung::non_neg_integer() into a #frame
%% and enqueue it.
push_frame(FrameData, MsgCount, State = #state{out_q = Q})
  when not is_record(FrameData, frame) ->
    Retries = logplex_app:config(http_frame_retries, 1),
    Tries = Retries + 1,
    Frame = #frame{frame=FrameData, msg_count=MsgCount,
                   tries = Tries,
                   id = frame_id()},
    NewQ = queue:in(Frame, Q),
    State#state{out_q = NewQ}.

frame_id() ->
    crypto:md5(term_to_binary({self(), now()})).

frame_id_to_iolist(ID) when is_binary(ID) ->
    [ hd(integer_to_list(I, 16)) || <<I:4>> <= ID ].

%% @private
%% @doc Frame has just consumed a try. Decrement #frame.tries. If it
%% has at least one try remaining, push it on to the front of the
%% outbound frame queue. If it is out of tries, drop it.
retry_frame(Frame = #frame{tries = N},
            State = #state{out_q = Q}) when N >= 2 ->
    NewQ = queue:in_r(Frame#frame{tries = N - 1}, Q),
    State#state{out_q = NewQ};
retry_frame(Frame = #frame{tries = N}, State) when N < 2 ->
    drop_frame(Frame, State).

uri_to_string(Uri) ->
    ex_uri:encode(ex_uri:hide_userinfo(Uri)).

auth_header(#ex_uri{authority=#ex_uri_authority{userinfo=Info}})
  when Info =/= undefined ->
    cowboy_client:auth_header(Info);
auth_header(_) ->
    [].

full_host_iolist(#ex_uri{authority=#ex_uri_authority{host=Host,
                                                     port=Port}})
  when is_integer(Port), Host =/= undefined ->
    [Host, ":", integer_to_list(Port)];
full_host_iolist(#ex_uri{authority=#ex_uri_authority{host=Host}})
  when Host =/= undefined ->
    Host.

uri_ref(#ex_uri{path=Path, q=Q}) ->
    Ref = #ex_uri_ref{path = case Path of
                                 "" -> "/";
                                 _ -> Path
                             end, q=Q},
    ex_uri:encode(Ref).

connection_info(#ex_uri{scheme = Scheme,
                        authority=#ex_uri_authority{host=Host,
                                                    port=Port}}) ->
    {Scheme, Host,
     case Port of
         Int when is_integer(Int) -> Int;
         undefined when Scheme =:= "http" -> 80;
         undefined when Scheme =:= "https" -> 443
     end}.

set_reconnect_timer(State = #state{reconnect_tref=undefined}) ->
    Time = timer:seconds(logplex_app:config(http_reconnect_time_s,1)),
    reconnect_in(Time, State);
set_reconnect_timer(State = #state{}) -> State.


reconnect_in(MS, State = #state{}) ->
    Ref = erlang:start_timer(MS, self(), ?RECONNECT_MSG),
    ?INFO("drain_id=~p channel_id=~p dest=~s at=reconnect_delay delay=~p "
          "ref=~p",
          log_info(State, [MS, Ref])),
    State#state{reconnect_tref = Ref}.

cancel_reconnect(State = #state{reconnect_tref=Ref}) when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            %% Flush msg q.
            receive
                {timeout, Ref, _} -> ok
            after 0 -> ok
            end;
         _Time -> ok
    end,
    State#state{reconnect_tref=undefined};
cancel_reconnect(State = #state{reconnect_tref=undefined}) ->
    State.
