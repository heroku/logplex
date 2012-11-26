%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc
%% @end
%%%-------------------------------------------------------------------
-module(logplex_http_client).

-include("logplex_logging.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/7
         ,raw_request/3
         ,close/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {drain_id, channel_id, dest,
                client}).

%%====================================================================
%% API
%%====================================================================

start_link(DrainId, ChannelId, Dest, Scheme, Host, Port, Timeout) ->
    gen_server:start_link(?MODULE,
                          [#state{drain_id=DrainId,
                                  channel_id=ChannelId,
                                  dest=Dest},
                           Scheme, Host, Port],
                          [{timeout, Timeout}]).

raw_request(Pid, Req, Timeout) ->
    gen_server:call(Pid, {raw_request, Req}, Timeout).

close(Pid) ->
    gen_server:cast(Pid, close),
    timer:kill_after(timer:seconds(1), Pid), % XXX - necessary?
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([State = #state{},
      Scheme, Host, Port]) ->
    {ok, Client0} = client_init(Scheme),
    ConnectStart = os:timestamp(),
    try cowboy_client:connect(scheme_to_transport(Scheme),
                              Host, Port, Client0) of
        {ok, Client} ->
            ConnectEnd = os:timestamp(),
            ?INFO("drain_id=~p channel_id=~p dest=~s at=try_connect "
                  "attempt=success connect_time=~p",
                  log_info(State, [ltcy(ConnectStart, ConnectEnd)])),
            {ok, State#state{client=Client}};
        {error, Why} ->
            ConnectEnd = os:timestamp(),
            ?WARN("drain_id=~p channel_id=~p dest=~s at=try_connect "
                  "attempt=fail connect_time=~p tcp_err=~1000p",
                  log_info(State, [ltcy(ConnectStart, ConnectEnd), Why])),
            ignore
    catch
        Class:Err ->
            Report = {Class, Err, erlang:get_stacktrace()},
            ConnectEnd = os:timestamp(),
            ?WARN("drain_id=~p channel_id=~p dest=~s at=connect "
                  "attempt=fail err=exception connect_time=~p "
                  "next_state=disconnected "
                  "data=~1000p",
                  log_info(State, [ltcy(ConnectStart, ConnectEnd), Report])),
            ignore
    end.

%% @private
handle_call({raw_request, Req}, _From, State) ->
    ReqStart = os:timestamp(),
    case raw_request(Req, State) of
        {ok, Status, Headers, NewState} ->
            ReqEnd = os:timestamp(),
            ?INFO("drain_id=~p channel_id=~p dest=~s at=response "
                  "result=success status=~p req_time=~p",
                  log_info(State, [Status, ltcy(ReqStart, ReqEnd)])),
            {reply, {ok, Status, Headers}, NewState};
        {error, Why} = Err ->
            ReqEnd = os:timestamp(),
            ?WARN("drain_id=~p channel_id=~p dest=~s at=response"
                  " result=error req_time=~p tcp_err=\"~1000p\"",
                  log_info(State, [ltcy(ReqStart, ReqEnd), Why])),
            {stop, normal, Err, State}
    end;

handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%% @private
handle_cast(close, State = #state{client=Client}) ->
    cowboy_client:close(Client),
    {stop, normal, State};

handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
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

scheme_to_transport("https") -> cowboy_ssl_transport;
scheme_to_transport("http") -> cowboy_tcp_transport.

client_init("http") ->
    cowboy_client:init([]);
client_init("https") ->
    cowboy_client:init([{reuse_sessions, false}
                       ]).

log_info(#state{drain_id=DrainId, channel_id=ChannelId, dest=Dest}, Rest)
  when is_list(Rest) ->
    [DrainId, ChannelId, Dest | Rest].


ltcy(Start, End) ->
    timer:now_diff(End, Start).

raw_request(Request, State = #state{client=Client}) ->
    try
        {ok, Client2} =
            cowboy_client:raw_request(Request, Client),
        case cowboy_client:response(Client2) of
            {ok, Status, Headers, NewClient} ->
                {ok, Status, Headers,
                 State#state{client = NewClient}};
            {error, _Why} = Err ->
                Err
        end
    catch
        Class:Ex ->
            {error, {Class, Ex, erlang:get_stacktrace()}}
    end.
