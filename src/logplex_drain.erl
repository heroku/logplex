%% Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>
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
-module(logplex_drain).
-behaviour(gen_server).

-record(state, {timer_ref}).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-export([create/3, delete/3, clear_all/1, lookup/1, lookup_host/1]).

-include_lib("logplex.hrl").

%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host), (is_integer(Port) orelse Port == undefined) ->
    case lookup(ChannelId, Host, Port) of
        [_] ->
            {error, already_exists};
        Other when Other == []; Other == undefined ->
            case logplex_utils:resolve_host(Host) of
                undefined ->
                    error_logger:error_msg("invalid drain: ~p:~p~n", [Host, Port]),
                    {error, invalid_drain};
                _Ip ->
                    case redis_helper:drain_index() of
                        DrainId when is_integer(DrainId) ->
                            redis_helper:create_drain(DrainId, ChannelId, Host, Port),
                            DrainId;
                        Error ->
                            Error
                    end
            end
    end.

delete(ChannelId, Host, Port) when is_integer(ChannelId), is_binary(Host), is_list(Port) ->
    Port1 = if Port == "" -> undefined; true -> list_to_integer(Port) end,
    case lookup(ChannelId, Host, Port1) of
        [#drain{id=DrainId}|_] ->
            redis_helper:delete_drain(DrainId);
        _ ->
            {error, not_found}
    end.

clear_all(ChannelId) when is_integer(ChannelId) ->
    List = logplex_channel:drains(ChannelId),
    [begin
        redis_helper:delete_drain(DrainId)
    end || #drain{id=DrainId} <- List],
    ok.

lookup(DrainId) when is_integer(DrainId) ->
    redis_helper:lookup_drain(DrainId).

lookup(ChannelId, Host, Port) ->
    case {nsync_helper:tab_channel_drains(), nsync_helper:tab_drains()} of
        {ChannelDrainsTab, DrainsTab} when ChannelDrainsTab == undefined; DrainsTab == undefined ->
            undefined;
        {ChannelDrainsTab, DrainsTab} ->
            lookup(ChannelDrainsTab, DrainsTab, ChannelId, Host, Port)
    end.
lookup(ChannelDrainsTab, DrainsTab, ChannelId, Host, Port) ->
    case ets:lookup(ChannelDrainsTab, iolist_to_binary([<<"ch:">>,integer_to_list(ChannelId),<<":drain">>])) of
        [{_, DrainIdList}] ->
            lookup_drainlist(DrainsTab, DrainIdList, #drain{channel_id = ChannelId, host = Host, port = Port});
        _ -> undefined
    end.

lookup_drainlist(DrainsTab, DrainIdList, #drain{host=Host,port=Port}=Drain) ->
    lists:foldl(
      fun(DrainId, Acc) -> 
              case ets:lookup(DrainsTab, iolist_to_binary([<<"drain:">>,DrainId,<<":data">>])) of
                  [{_, Dict}] ->
                      case dict:fetch(<<"host">>, Dict) == Host of 
                          true when is_integer(Port) -> 
                              BinPort = list_to_binary(integer_to_list(Port)),
                              case dict:find(<<"port">>, Dict) of
                                  {ok, BinPort} ->
                                      Id = list_to_integer(binary_to_list(iolist_to_binary([DrainId]))),
                                      [Drain#drain{id = Id, resolved_host = lookup_host(Id)} | Acc];
                                  _ -> Acc
                              end;
                          true ->
                              Id = list_to_integer(binary_to_list(iolist_to_binary([DrainId]))),
                              [Drain#drain{id = Id, resolved_host = lookup_host(Id)} | Acc];
                          _ -> Acc 
                      end;
                  _ -> Acc
              end
      end, [], DrainIdList).

lookup_host(DrainId) when is_integer(DrainId) ->
    gen_server:call(?MODULE, {lookup_host, DrainId}).

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
    ets:new(logplex_drain_hosts, [named_table, set, protected]),
    TimerRef = erlang:send_after(60 * 1000, self(), refresh_dns),
    {ok, #state{timer_ref = TimerRef}}.

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
handle_call({lookup_host, DrainId}, _From, State) ->
    case ets:lookup(logplex_drain_hosts, DrainId) of
        [] ->
            {reply, undefined, State};
        [{DrainId, ResolvedHost}] ->
            {reply, ResolvedHost, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast({create_drain, DrainId, ResolvedHost}, State) ->
    ets:insert(logplex_drain_hosts, {DrainId, ResolvedHost}),
    {noreply, State};

handle_cast({delete_drain, DrainId}, State) ->
    ets:delete(logplex_drain_hosts, DrainId),
    {noreply, State};

handle_cast(refresh_dns, State) ->
    refresh_dns(),
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
handle_info(refresh_dns, State) ->
    refresh_dns(),
    erlang:send_after(60 * 1000, self(), refresh_dns),
    {noreply, State};
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
terminate(_Reason, #state{timer_ref = TimerRef}) ->
    erlang:cancel_timer(TimerRef),
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
refresh_dns() ->
    case nsync_helper:tab_drains() of
        undefined ->
            ok;
        DrainsTab ->
            DrainHosts = 
                lists:foldl(fun({Drain, Dict}, Acc)-> 
                                    Host = dict:fetch(<<"host">>, Dict),
                                    case logplex_utils:resolve_host(Host) of
                                        undefined -> Acc;
                                        Ip -> 
                                            DrainId = list_to_integer(string:sub_word(binary_to_list(Drain), 2, $:)),
                                            [{DrainId, Ip} | Acc]
                                    end
                            end, [], ets:tab2list(DrainsTab)),
            ets:insert(logplex_drain_hosts, DrainHosts)
    end.
