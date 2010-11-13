-module(logplex_drain).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([create/3, delete/1, lookup/1, route/3]).

-record(state, {socket}).

%% API functions
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(ChannelId, Host, Port) when is_binary(ChannelId), is_binary(Host), is_integer(Port) ->
    case redis:q([<<"INCR">>, <<"drain_index">>]) of
        {ok, DrainId} ->
            redis:q([<<"HMSET">>, iolist_to_binary([<<"drain:">>, integer_to_list(DrainId)]),
                <<"channel_id">>, ChannelId,
                <<"host">>, Host,
                <<"port">>, integer_to_list(Port)]),
            redis:q([<<"SADD">>, iolist_to_binary([<<"channel:">>, ChannelId, <<":drains">>]), integer_to_list(DrainId)]);
        Error ->
            Error
    end.

delete(DrainId) when is_binary(DrainId) ->
    case lookup(DrainId) of
        [{channel_id, ChannelId},_] ->
            redis:q([<<"DEL">>, iolist_to_binary([<<"drain:">>, DrainId])]),
            redis:q([<<"SREM">>, iolist_to_binary([<<"channel:">>, ChannelId, <<":drains">>])]);
        _ ->
            ok
    end.

lookup(DrainId) when is_binary(DrainId) ->
    case redis:q([<<"HGETALL">>, iolist_to_binary([<<"drain:">>, DrainId])]) of
        Fields when is_list(Fields), length(Fields) > 0 ->
            [{channel_id, logplex_utils:field_val(<<"channel_id">>, Fields)},
             {host, logplex_utils:field_val(<<"host">>, Fields)},
             {port, begin
                 case logplex_utils:field_val(<<"port">>, Fields) of
                     <<"">> -> undefined;
                     Val -> list_to_integer(binary_to_list(Val))
                 end
              end}];
        _ ->
            []
    end.

route(Host, Port, Msg) when is_binary(Host), is_integer(Port), is_binary(Msg) ->
    gen_server:cast(?MODULE, {route, Host, Port, Msg}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init([]) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
	{ok, #state{socket=Socket}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({route, Host, Port, Msg}, State) ->
    gen_udp:send(State#state.socket, binary_to_list(Host), Port, Msg),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> 
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
