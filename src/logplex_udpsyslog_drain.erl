%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc udpsyslog://Host:Port/
%% @end
%%%-------------------------------------------------------------------
-module(logplex_udpsyslog_drain).

-behaviour(gen_server).

-include("logplex.hrl").
-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

%% API
-export([start_link/5
         ,shutdown/1
        ]).

-export([valid_uri/1
         ,uri/2
         ,start_link/4
        ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                host :: string() | inet:ip_address() | binary(),
                address :: inet:ip_address(),
                port :: inet:port_number(),
                sock = undefined :: 'undefined' | inet:socket(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% UDP failures since last_good_time
                failures = 0 :: non_neg_integer()
               }).

-define(RECONNECT_MSG, reconnect).

%%====================================================================
%% API
%%====================================================================

start_link(ChannelID, DrainID, DrainTok,
           {udpsyslog, _, Host, Port, _, _}) ->
    start_link(ChannelID, DrainID, DrainTok, Host, Port).

start_link(ChannelID, DrainID, DrainTok, Host, Port) ->
    gen_server:start_link(?MODULE,
                          [#state{drain_id=DrainID,
                                  drain_tok=DrainTok,
                                  channel_id=ChannelID,
                                  host=Host,
                                  port=Port}],
                          []).

shutdown(Pid) ->
    gen_server:cast(Pid, shutdown),
    ok.

valid_uri(#ex_uri{scheme="udpsyslog",
                  authority=#ex_uri_authority{host=Host, port=Port}} = Uri)
  when is_list(Host), is_integer(Port),
       0 < Port andalso Port =< 65535 ->
    {valid, udpsyslog, Uri};
valid_uri(#ex_uri{scheme="udpsyslog",
                  authority=A=#ex_uri_authority{host=Host,
                                                port=undefined}} = Uri)
  when is_list(Host) ->
    {valid, udpsyslog,
     Uri#ex_uri{authority=A#ex_uri_authority{port=514}}};
valid_uri(_) ->
    {error, invalid_udpsyslog_uri}.

uri(Host, Port) when is_binary(Host), is_integer(Port) ->
    uri(binary_to_list(Host), Port);
uri(Host, Port) when is_list(Host), is_integer(Port) ->
    #ex_uri{scheme="udpsyslog",
            authority=#ex_uri_authority{host=Host, port=Port}}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([State = #state{drain_id=DrainId, channel_id=ChannelId,
                     host=H, port=P}])
  when H =/= undefined, is_integer(P) ->
    try
        logplex_drain:register(DrainId, ChannelId, udpsyslog, {H,P}),
        ?INFO("drain_id=~p channel_id=~s dest=~s at=spawn",
              log_info(State, [])),
        {ok, State, hibernate}
    catch
        error:badarg -> ignore
    end.

%% @private
handle_call(Call, _From, State) ->
    ?WARN("drain_id=~p channel_id=~s dest=~s err=unexpected_call data=~p",
          log_info(State, [Call])),
    {noreply, State}.

%% @private
handle_cast(shutdown, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
    ?WARN("drain_id=~p channel_id=~s dest=~s err=unexpected_cast data=~p",
          log_info(State, [Msg])),
    {noreply, State}.

%% @private
handle_info({post, Msg}, State = #state{sock = undefined})
  when is_tuple(Msg) ->
    case connect(State) of
        {ok, Addr, Sock} ->
            ?INFO("drain_id=~p channel_id=~s dest=~s at=connect try=~p addr=~s",
                  log_info(State, [State#state.failures + 1,
                                   host_str(Addr)])),
            handle_info({post, Msg},
                        udp_good(State#state{address=Addr, sock=Sock}));
        {error, Reason} ->
            NewState = udp_bad(State#state{sock=undefined}),
            msg_stat(drain_dropped, 1, NewState),
            ?ERR("drain_id=~p channel_id=~s dest=~s at=connect "
                 "err=gen_udp data=~p try=~p last_success=~s",
                 log_info(State, [Reason, NewState#state.failures,
                                  time_failed(NewState)])),
            {noreply, NewState}
    end;

handle_info({post, Msg}, State = #state{}) when is_tuple(Msg) ->
    case post(Msg, State) of
        ok ->
            msg_stat(drain_delivered, 1, State),
            logplex_realtime:incr('drain.delivered'),
            {noreply, udp_good(State)};
        {error, Reason} ->
            NewState = udp_bad(State#state{sock=undefined}),
            msg_stat(drain_dropped, 1, State),
            ?ERR("drain_id=~p channel_id=~s dest=~s at=post "
                 "err=gen_udp data=~p",
                 log_info(NewState, [Reason])),
            {noreply, NewState}
    end;

handle_info({udp, S, _IP, _Port, Data}, State = #state{sock = S}) ->
    ?WARN("drain_id=~p channel_id=~s dest=~s err=unexpected_peer_data data=~p",
          log_info(State, [Data])),
    {noreply, State};

handle_info(Info, State) ->
    ?WARN("drain_id=~p channel_id=~s dest=~s err=unexpected_info data=~p",
          log_info(State, [Info])),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec post(logplex_syslog_utils:syslog_msg(),
           #state{}) -> 'ok' |
                        {'error', term()}.
%% @private
post(Msg, #state{sock=Sock,
                 address=Addr,
                 port=Port,
                 drain_tok=DrainTok}) when is_tuple(Msg) ->
    Packet = logplex_syslog_utils:to_msg(Msg, DrainTok),
    gen_udp:send(Sock, Addr, Port, Packet).

%% @private
-spec connect(#state{sock :: undefined}) -> {ok, inet:ip_address(), port()} |
                                            {'error', term()}.
connect(#state{sock = undefined, host=Host, port=Port})
    when is_integer(Port) ->
    case inet:getaddrs(Host, inet) of
        {ok, [Addr | _]} ->
            case gen_udp:open(0, [binary]) of
                {ok, Sock} ->
                    {ok, Addr, Sock};
                {error, _} = E -> E
            end;
        {error, _} = E -> E
    end.

%% @private
udp_good(State = #state{}) ->
    State#state{last_good_time = os:timestamp(),
                failures = 0}.

%% @private
%% Caller must ensure sock is closed before calling this.
udp_bad(State = #state{failures = F}) ->
    State#state{failures = F + 1}.

-spec time_failed(#state{}) -> iolist().
%% @private
time_failed(State = #state{}) ->
    time_failed(os:timestamp(), State).
time_failed(Now, #state{last_good_time=T0})
  when is_tuple(T0) ->
    io_lib:format("~fs ago", [timer:now_diff(Now, T0) / 1000000]);
time_failed(_, #state{last_good_time=undefined}) ->
    "never".

%% @private
host_str({A,B,C,D}) ->
    Quads = [integer_to_list(Quad) || Quad <- [A,B,C,D]],
    string:join(Quads,".");
host_str(H)
  when is_list(H); is_binary(H) ->
    H.

%% @private
log_info(#state{drain_id=DrainId, channel_id=ChannelId, host=H, port=P}, Rest)
  when is_list(Rest) ->
    [DrainId, ChannelId, io_lib:format("~s:~p", [host_str(H), P]) | Rest].

-spec msg_stat('drain_dropped' | 'drain_buffered' | 'drain_delivered',
               pos_integer(), #state{}) -> any().
msg_stat(Key, N,
         #state{drain_id=DrainId, channel_id=ChannelId}) ->
    logplex_stats:incr(#drain_stat{drain_id=DrainId,
                                   channel_id=ChannelId,
                                   key=Key}, N).
