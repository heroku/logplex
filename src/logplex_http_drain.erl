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
-define(HIBERNATE_TIMEOUT, 5000).
-define(SHRINK_TIMEOUT, timer:minutes(5)).
-define(SHRINK_BUF_SIZE, 10).

-type drop_info() :: {erlang:timestamp(), pos_integer()}.


-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                uri :: #ex_uri{},
                buf :: pid(),
                client :: pid(),
                out_q = queue:new() :: queue(),
                reconnect_tref :: reference() | 'undefined',
                reconnect_attempt = 0 :: pos_integer(),
                close_tref :: reference() | 'undefined',
                drop_info :: drop_info() | 'undefined',
                %% Last time we connected or successfully sent data
                last_good_time = never :: 'never' | erlang:timestamp(),
                service = normal :: 'normal' | 'degraded',
                %% Time of last successful connection
                connect_time :: 'undefined' | erlang:timestamp()
               }).

-record(frame, {frame :: iolist(),
                msg_count :: non_neg_integer(),
                loss_count = 0 :: non_neg_integer(),
                tries = 0 :: non_neg_integer(),
                id :: binary()
               }).

-define(CONTENT_TYPE, <<"application/logplex-1">>).
-define(HTTP_VERSION, 'HTTP/1.1').
-define(RECONNECT_MSG, reconnect).
-define(CLOSE_TIMEOUT_MSG, close_timeout).
-define(CONNECT_TIMEOUT, 3000).
-define(REQUEST_TIMEOUT, 5000).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([valid_uri/1
         ,start_link/4
        ]).

-export([user_agent/0
         ,drain_buf_framing/1
         ,backoff_slot/1
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

valid_uri(#ex_uri{scheme=Http,
                  authority = #ex_uri_authority{host = Host}} = Uri)
  when (Http =:= "http" orelse Http =:= "https"),
       is_list(Host), Host =/= "" ->
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
                     drain_id=DrainId}) ->
    process_flag(trap_exit, true),
    random:seed(os:timestamp()),
    try
        Dest = uri_to_string(URI),
        State = start_drain_buffer(State0),
        logplex_drain:register(DrainId, http, Dest),
        ?INFO("drain_id=~p channel_id=~p dest=~s at=spawn",
              log_info(State, [])),
        {ok, disconnected,
         State, hibernate}
    catch
        error:badarg -> ignore
    end.

%% @private
disconnected({logplex_drain_buffer, Buf, new_data},
             State = #state{buf = Buf, reconnect_tref = TRef}) ->
    cancel_timeout(TRef, ?RECONNECT_MSG),
    try_connect(State#state{reconnect_tref=undefined});
disconnected({logplex_drain_buffer, Buf, {frame, Frame, MsgCount, Lost}},
             State = #state{buf = Buf, reconnect_tref = TRef}) ->
    NewState = push_frame(Frame, MsgCount, Lost, State),
    cancel_timeout(TRef, ?RECONNECT_MSG),
    try_connect(NewState#state{reconnect_tref=undefined});
disconnected(timeout, S = #state{}) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, disconnected, S, hibernate};
disconnected({timeout, _Ref, ?CLOSE_TIMEOUT_MSG}, State) ->
    {next_state, disconnected, State, hibernate};
disconnected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=\"~1000p\" state=disconnected",
          log_info(State, [Msg])),
    {next_state, disconnected, State, ?HIBERNATE_TIMEOUT}.

%% @private
connected({logplex_drain_buffer, Buf, {frame, Frame, MsgCount, Lost}},
          State = #state{buf = Buf}) ->
    ready_to_send(push_frame(Frame, MsgCount, Lost, State));
connected({logplex_drain_buffer, Buf, new_data},
          State = #state{buf = Buf}) ->
    ready_to_send(State);
connected(timeout, S = #state{}) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, connected, S, hibernate};
connected({timeout, TRef, ?CLOSE_TIMEOUT_MSG}, State=#state{close_tref=TRef}) ->
    case close_if_idle(State) of
        {closed, ClosedState} ->
            {next_state, disconnected, ClosedState, ?HIBERNATE_TIMEOUT};
        {not_closed, State} ->
            case close_if_old(State) of
                {closed, ClosedState} ->
                    {next_state, disconnected, ClosedState, ?HIBERNATE_TIMEOUT};
                {not_closed, ContinueState} ->
                    {next_state, connected, ContinueState}
            end
    end;
connected(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s err=unexpected_info "
          "data=\"~1000p\" state=connected",
          log_info(State, [Msg])),
    {next_state, connected, State, ?HIBERNATE_TIMEOUT}.

%% @private
handle_event(Event, StateName, State) ->
    ?WARN("state=~p at=unexpected_event event=\"~1000p\"",
          [StateName, Event]),
    {next_state, StateName, State, ?HIBERNATE_TIMEOUT}.

%% @private
handle_sync_event(buf_alive, _From, StateName,
                  State = #state{buf = Buf}) ->
    {reply, {Buf, erlang:is_process_alive(Buf)}, StateName, State, ?HIBERNATE_TIMEOUT};
handle_sync_event(restart_buf, _From, StateName,
                  State = #state{buf = Buf}) ->
    case erlang:is_process_alive(Buf) of
        true ->
            {reply, buf_alive, StateName, State};
        false ->
            NewState = start_drain_buffer(State#state{buf = undefined}),
            {reply, {new_buf, NewState#state.buf}, StateName, NewState, ?HIBERNATE_TIMEOUT}
    end;
handle_sync_event(notify, _From, StateName, State = #state{buf = Buf}) ->
    logplex_drain_buffer:notify(Buf),
    {reply, ok, StateName, State, ?HIBERNATE_TIMEOUT};
handle_sync_event(Event, _From, StateName, State) ->
    ?WARN("state=~p at=unexpected_sync_event event=\"~1000p\"",
          [StateName, Event]),
    {next_state, StateName, State, ?HIBERNATE_TIMEOUT}.

%% @private
handle_info({timeout, Ref, ?RECONNECT_MSG}, disconnected,
            State = #state{reconnect_tref = Ref,
                           buf = Buf,
                           out_q = Q}) ->
    NewState = State#state{reconnect_tref=undefined},
    case queue:is_empty(Q) of
        true ->
            logplex_drain_buffer:notify(Buf),
            {next_state, disconnected, NewState, ?HIBERNATE_TIMEOUT};
        false ->
            try_connect(NewState)
    end;
handle_info({timeout, Ref, ?RECONNECT_MSG}, StateName,
            State = #state{reconnect_tref = Ref}) ->
    {next_state, StateName, State#state{reconnect_tref=undefined}, ?HIBERNATE_TIMEOUT};
handle_info({timeout, _Ref, ?RECONNECT_MSG}, StateName,
            State = #state{}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s at=reconnect_timeout "
          "err=invalid_timeout state=~p",
          log_info(State, [StateName])),
    {next_state, StateName, State, ?HIBERNATE_TIMEOUT};

handle_info(shutdown, _StateName, State) ->
    {stop, {shutdown,call}, State};

handle_info({'EXIT', BufPid, Reason}, StateName,
            State = #state{buf = BufPid}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s at=drain_buffer_exit "
          "state=~p buffer_pid=~p err=~1000p",
          log_info(State, [StateName, BufPid, Reason])),
    NewState = start_drain_buffer(State#state{buf = undefined}),
    {next_state, StateName, NewState, ?HIBERNATE_TIMEOUT};

handle_info({'EXIT', ClientPid, Reason}, StateName,
            State = #state{client = ClientPid}) ->
    ?WARN("drain_id=~p channel_id=~p dest=~s at=http_client_exit "
          "state=~p client_pid=~p err=~1000p",
          log_info(State, [StateName, ClientPid, Reason])),
    {next_state, StateName, State, ?HIBERNATE_TIMEOUT};

%% close_timeout used to be called idle_timeout; remove once we are on v72+
handle_info({timeout, TRef, idle_timeout}, StateName, State) ->
    apply(?MODULE, StateName, [{timeout, TRef, ?CLOSE_TIMEOUT_MSG}, State]);

handle_info(timeout, StateName, State) ->
    %% Sleep when inactive, trigger fullsweep GC & Compact
    {next_state, StateName, State, hibernate};

handle_info(Info, StateName, State) ->
    ?MODULE:StateName(Info, State).



%% @private
try_connect(State = #state{uri=Uri,
                           drain_id=DrainId,
                           channel_id=ChannelId,
                           client=undefined}) ->
    {Scheme, Host, Port} = connection_info(Uri),
    ConnectStart = os:timestamp(),
    case logplex_http_client:start_link(DrainId, ChannelId,
                                        uri_to_string(Uri),
                                        Scheme, Host,
                                        Port, ?CONNECT_TIMEOUT) of
        {ok, Pid} ->
            ConnectEnd = os:timestamp(),
            ?INFO("drain_id=~p channel_id=~p dest=~s at=try_connect "
                  "attempt=success connect_time=~p",
                  log_info(State, [ltcy(ConnectStart, ConnectEnd)])),
            NewTimerState = start_close_timer(State),
            ready_to_send(NewTimerState#state{client=Pid,
                                              connect_time=ConnectEnd,
                                              reconnect_attempt=0});
        ignore ->
            http_fail(State);
        Why ->
            ConnectEnd = os:timestamp(),
            ?WARN("drain_id=~p channel_id=~p dest=~s at=try_connect "
                  "attempt=fail reason=~100p connect_time=~p",
                  log_info(State, [Why, ltcy(ConnectStart, ConnectEnd)])),
            http_fail(State)
    end.

%% @private
http_fail(State = #state{client=Client}) ->
    %% Close any existing client connection.
    ClosedState = case Client of
                      Pid when is_pid(Pid) ->
                          logplex_http_client:close(Pid),
                          State#state{client = undefined};
                      undefined ->
                          State
                  end,
    NewState = maybe_shrink(ClosedState),
    %% We hibernate only when we need to reconnect with a timer. The timer
    %% acts as a rate limiter! If you remove the timer, you must re-think
    %% the hibernation.
    case set_reconnect_timer(NewState) of
        NewState ->
            {next_state, disconnected, NewState};
        ReconnectState ->
            {next_state, disconnected, ReconnectState, hibernate}
    end.

%% @private
ready_to_send(State = #state{buf = Buf,
                             out_q = Q}) ->
    case queue:out(Q) of
        {empty, Q} ->
            logplex_drain_buffer:set_active(Buf, target_bytes(),
                                            fun ?MODULE:drain_buf_framing/1),
            {next_state, connected, State, ?HIBERNATE_TIMEOUT};
        {{value, Frame}, Q2} ->
            try_send(Frame, State#state{out_q = Q2})
    end.

try_send(Frame = #frame{tries = Tries},
         State = #state{client = Pid})
  when Tries > 0 ->
    Req = request_to_iolist(Frame, State),
    ReqStart = os:timestamp(),
    try logplex_http_client:raw_request(Pid, Req, ?REQUEST_TIMEOUT) of
        {ok, Status, _Headers} ->
            ReqEnd = os:timestamp(),
            handle_response_status(Status, Frame, State, ltcy(ReqStart, ReqEnd));
        {error, Why} ->
            ?WARN("drain_id=~p channel_id=~p dest=~s at=send_request"
                  " tcp_err=~1000p",
                  log_info(State, [Why])),
            http_fail(retry_frame(Frame, State))
    catch
        exit:{timeout, _} ->
            ReqEnd = os:timestamp(),
            ?WARN("drain_id=~p channel_id=~p dest=~s at=send_request "
                  "attempt=fail err=timeout req_time=~p "
                  "next_state=disconnected",
                  log_info(State, [ltcy(ReqStart, ReqEnd)])),
            http_fail(retry_frame(Frame,State));
        Class:Err ->
            ReqEnd = os:timestamp(),
            Report = {Class, Err, erlang:get_stacktrace()},
            ?WARN("drain_id=~p channel_id=~p dest=~s at=send_request "
                  "attempt=fail err=exception req_time=~p "
                  "next_state=disconnected data=~1000p",
                  log_info(State, [ltcy(ReqStart, ReqEnd), Report])),
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
handle_response_status(Status, Frame, State, _Latency) when 200 =< Status, Status < 300 ->
    ready_to_send(sent_frame(Frame, State));
handle_response_status(Status, Frame, State, Latency) when 400 =< Status, Status < 500 ->
    ?INFO("drain_id=~p channel_id=~p dest=~s at=response "
          "result=~p status=~p msg_count=~p req_time=~p",
          log_info(State, [perm_fail, Status, Frame#frame.msg_count, Latency])),
    ready_to_send(drop_frame(Frame, State));
handle_response_status(Status, Frame, State, Latency) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s at=response "
          "result=~p status=~p msg_count=~p req_time=~p",
          log_info(State, [temp_fail, Status, Frame#frame.msg_count, Latency])),
    http_fail(retry_frame(Frame, State)).

%% @private
terminate(_Reason, _StateName, _State) ->
    ok.

%% @private
code_change("v78", StateName,
            {state, DrainId, DrainTok, ChannelId, Uri, Buf, Client, OutQ,
             ReconnectTref, CloseTref, DropInfo, _LastGoodTime, Service,
             ConnectTime}, undefined) ->
    State = #state{drain_id=DrainId,
                   drain_tok=DrainTok,
                   channel_id=ChannelId,
                   uri=Uri,
                   buf=Buf,
                   client=Client,
                   out_q=OutQ,
                   reconnect_tref=ReconnectTref,
                   reconnect_attempt=0,
                   close_tref=CloseTref,
                   drop_info=DropInfo,
                   last_good_time=never,
                   service=Service,
                   connect_time=ConnectTime},
    {ok, StateName, State};
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% @private
log_info(#state{drain_id=DrainId, channel_id=ChannelId, uri=URI}, Rest)
  when is_list(Rest) ->
    [DrainId, ChannelId, uri_to_string(URI) | Rest].

%% @private

%% @private
request_to_iolist(#frame{frame = Body0,
                         msg_count = Count0,
                         loss_count=Lost,
                         id = Id},
                  #state{uri = URI = #ex_uri{},
                         drop_info=Drops,
                         drain_tok = Token}) ->
    {Body, Count} = case {Drops,Lost} of
        {undefined,0} -> {Body0, Count0};
        {undefined,_} ->
            T0 = os:timestamp(),
            Msg = frame(
                logplex_syslog_utils:overflow_msg(Lost,T0)
            ),
            {[Msg, Body0],Count0+1};
        {{T0,Dropped},_} ->
            Msg = frame(
                logplex_syslog_utils:overflow_msg(Dropped+Lost,T0)
            ),
            {[Msg, Body0],Count0+1}
    end,
    AuthHeader = auth_header(URI),
    MD5Header = case logplex_app:config(http_body_checksum, none) of
                    md5 -> [{<<"Content-MD5">>,
                             base64:encode(crypto:hash(md5,Body))}];
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

rfc5424({Facility, Severity, Time, Source, Process, Msg}) ->
    logplex_syslog_utils:rfc5424(Facility, Severity, Time, "host",
                                 Source, Process, undefined, Msg).

frame(LogTuple) ->
    logplex_syslog_utils:frame(rfc5424(LogTuple)).

drain_buf_framing({loss_indication, _N, _When}) ->
    skip;
drain_buf_framing({msg, MData}) ->
    {frame, frame(MData)}.

-spec target_bytes() -> pos_integer().
%% @private
target_bytes() ->
    logplex_app:config(http_drain_target_bytes,
                       102400).

%% @private
%% @doc Called on frames we've decided to drop. Records count of
%% messages dropped (not frame count).
drop_frame(#frame{msg_count=Msgs, loss_count=Lost}, State) ->
    lost_msgs(Msgs+Lost, State).

%% @private
%% @doc Accounts for losses reported in the frame, globally.
lost_msgs(0, State) -> State;
lost_msgs(Lost, S=#state{drop_info=undefined}) ->
    S#state{drop_info={os:timestamp(), Lost}};
lost_msgs(Lost, S=#state{drop_info={TS,Dropped}}) ->
    S#state{drop_info={TS,Dropped+Lost}}.

%% @private
%% if we had failures, they should have been delivered with this frame
sent_frame(#frame{msg_count=Count, loss_count=Lost}, State0=#state{drop_info=Drop}) ->
    State = maybe_resize(State0),

    msg_stat(drain_delivered, Count, State),
    logplex_realtime:incr('drain.delivered', Count),
    case {Lost,Drop} of
        {0, undefined} ->
            State;
        {_, undefined} ->
            logplex_realtime:incr('drain.dropped', Lost),
            msg_stat(drain_dropped, Lost, State),
            State;
        {_, {_,Dropped}} ->
            logplex_realtime:incr('drain.dropped', Lost+Dropped),
            msg_stat(drain_dropped, Lost+Dropped, State),
            State#state{drop_info=undefined}
    end.

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
push_frame(FrameData, MsgCount, Lost, State = #state{out_q = Q})
  when not is_record(FrameData, frame) ->
    Retries = logplex_app:config(http_frame_retries, 1),
    Tries = Retries + 1,
    Frame = #frame{frame=FrameData, msg_count=MsgCount,
                   loss_count=Lost,
                   tries = Tries,
                   id = frame_id()},
    NewQ = queue:in(Frame, Q),
    State#state{out_q = NewQ}.

frame_id() ->
    crypto:hash(md5, term_to_binary({self(), now()})).

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
    reconnect_in(backoff_time(State#state.reconnect_attempt), State);
set_reconnect_timer(State = #state{}) -> State.

backoff_time(Attempt) when is_integer(Attempt), Attempt > 0 ->
    logplex_app:config(http_reconnect_time, 10) * backoff_slot(Attempt);
backoff_time(_) ->
    0.

backoff_slot(Attempt) when is_integer(Attempt), Attempt > 0 ->
    Slot = erlang:min(logplex_app:config(http_max_retries, 10), Attempt),
    trunc(erlang:max(0, (math:pow(2, random:uniform(Slot)) - 1) / 2)).

reconnect_in(MS, State = #state{ reconnect_attempt=N }) ->
    Ref = erlang:start_timer(MS, self(), ?RECONNECT_MSG),
    ?INFO("drain_id=~p channel_id=~p dest=~s at=reconnect_delay attempt=~p delay=~p "
          "ref=~p",
          log_info(State, [N, MS, Ref])),
    State#state{reconnect_tref = Ref, reconnect_attempt=N+1}.

cancel_timeout(undefined, _Msg) -> undefined;
cancel_timeout(Ref, Msg)
  when is_reference(Ref) ->
    case erlang:cancel_timer(Ref) of
        false ->
            %% Flush expired timer message
            receive
                {timeout, Ref, Msg} -> undefined
            after 0 -> undefined
            end;
        _Time ->
            %% Timer didn't fire, so no message to worry about
            undefined
      end.

ltcy(Start, End) ->
    timer:now_diff(End, Start).

start_close_timer(State=#state{close_tref = CloseTRef}) ->
    cancel_timeout(CloseTRef, ?CLOSE_TIMEOUT_MSG),
    MaxIdle = logplex_app:config(http_drain_idle_timeout, timer:minutes(5)),
    Fuzz = random:uniform(logplex_app:config(http_drain_idle_fuzz, 15000)),
    NewTimer = erlang:start_timer(MaxIdle + Fuzz, self(), ?CLOSE_TIMEOUT_MSG),
    State#state{close_tref = NewTimer}.

compare_point(#state{last_good_time=never, connect_time=ConnectTime}) ->
    ConnectTime;
compare_point(#state{last_good_time=LastGood}) ->
    LastGood.

connection_idle(State) ->
    MaxIdle = logplex_app:config(http_drain_idle_timeout, timer:minutes(5)),
    SinceLastGoodMicros = timer:now_diff(os:timestamp(), compare_point(State)),
    SinceLastGoodMicros > (MaxIdle * 1000).

close_if_idle(State = #state{client = Client}) ->
    case connection_idle(State) of
        true ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=idle_timeout",
                  log_info(State, [])),
            logplex_http_client:close(Client),
            {closed, State#state{client=undefined}};
        _ ->
            {not_closed, State}
    end.

connection_too_old(#state{connect_time = ConnectTime}) ->
    MaxTotal = logplex_app:config(http_drain_max_ttl, timer:hours(5)),
    SinceConnectMicros = timer:now_diff(os:timestamp(), ConnectTime),
    SinceConnectMicros > (MaxTotal * 1000).

close_if_old(State = #state{client = Client}) ->
    case connection_too_old(State) of
        true ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=max_ttl",
                  log_info(State, [])),
            logplex_http_client:close(Client),
            {closed, State#state{client=undefined}};
        _ ->
            {not_closed, start_close_timer(State)}
    end.

start_drain_buffer(State = #state{channel_id=ChannelId,
                                  buf = undefined}) ->
    Size = default_buf_size(),
    {ok, Buf} = logplex_drain_buffer:start_link(ChannelId, self(),
                                                notify, Size),
    State#state{buf = Buf}.

maybe_resize(State0=#state{ service=normal }) ->
    State0#state{ last_good_time=os:timestamp() };
maybe_resize(State0=#state{ service=degraded, buf=Buf }) ->
    State = State0#state{last_good_time=os:timestamp(), service=normal},
    Size = default_buf_size(),
    logplex_drain_buffer:resize_msg_buffer(Buf, Size),
    ?INFO("drain_id=~p channel_id=~p dest=~s at=maybe_resize"
          " service=normal buf_size=~p",
          log_info(State, [Size])),
    State.

maybe_shrink(State = #state{last_good_time=never}) ->
    ?INFO("drain_id=~p channel_id=~p dest=~s at=maybe_shrink"
          " service=~p time_since_last_good=~p",
          log_info(State, [degraded, never])),
    State#state{service=degraded};
maybe_shrink(State = #state{buf=Buf, service=Status, last_good_time=LastGood}) ->
    MsecSinceLastGood = trunc(timer:now_diff(os:timestamp(), LastGood) / 1000),
    case {MsecSinceLastGood > ?SHRINK_TIMEOUT, Status} of
        {true, normal} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=maybe_shrink"
                  " service=~p time_since_last_good=~p",
                  log_info(State, [degraded, MsecSinceLastGood])),
            logplex_drain_buffer:resize_msg_buffer(Buf, ?SHRINK_BUF_SIZE),
            State#state{service=degraded};
        {_, Status} ->
            ?INFO("drain_id=~p channel_id=~p dest=~s at=maybe_shrink"
                  " service=~p time_since_last_good=~p",
                  log_info(State, [Status, MsecSinceLastGood])),
            State#state{service=Status}
    end.

default_buf_size() -> logplex_app:config(http_drain_buffer_size, 1024).
