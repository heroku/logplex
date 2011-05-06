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
-module(logplex_db).
-behaviour(gen_server).

%% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
	     handle_info/2, terminate/2, code_change/3]).

-include_lib("logplex.hrl").

-define(BLANK_SCHEMA, {0,0}).

%% API functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    start(),
    {ok, []}.

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
start() ->
    wait_for_nodes(),
    SchemaVsn = schema_version(),
    case nodes() == [] of
        true ->
            SchemaVsn == undefined andalso create_schema(), 
            mnesia:start(),
            SchemaVsn == undefined andalso create_tables(),
            ok;
        false ->
            mnesia:start(),
            mnesia:change_config(extra_db_nodes, nodes()),
            mnesia:change_table_copy_type(schema, node(), disc_copies),
            sync_tables_to_local(),
            ok
    end.

wait_for_nodes() ->
    Registered = lists:sort(redgrid:registered_nodes()),
    Running = lists:sort([node()|nodes()]),
    case Registered == Running of
        true -> ok;
        false ->
            io:format("waiting for nodes~n"),
            timer:sleep(1000),
            wait_for_nodes()
    end.

schema_version() ->
    case mnesia:system_info(schema_version) of
        ?BLANK_SCHEMA ->
            undefined;
        Vsn ->
            Vsn
    end.

create_schema() ->
    io:format("create schema~n"),
    mnesia:create_schema([node()]),
    ok.

create_tables() ->
    io:format("create tables~n"),
    mnesia:create_table(counters,[{attributes, [key, val]},                   {disc_copies, [node()]}]),
    mnesia:create_table(channel, [{attributes, record_info(fields, channel)}, {disc_copies, [node()]}]),
    mnesia:create_table(token,   [{attributes, record_info(fields, token)},   {disc_copies, [node()]}]),
    mnesia:create_table(drain,   [{attributes, record_info(fields, drain)},   {disc_copies, [node()]}]),
    mnesia:create_table(session, [{attributes, record_info(fields, session)}, {disc_copies, [node()]}]),
    ok.

sync_tables_to_local() ->
    io:format("sync tables to local~n"),
    Tables = [{T, mnesia:table_info(T, where_to_commit)} || T <- mnesia:system_info(tables)],
    Copies =
        lists:foldl(
            fun({T, Locs}, Acc) ->
                lists:foldl(
                    fun({Node, Type}, Acc1) ->
                        case Node == node() of
                            true -> Acc1;
                            false -> [{T, Type}|Acc1]
                        end
                    end, Acc, Locs)
            end, [], Tables),
    [mnesia:add_table_copy(T, node(), Type) || {T, Type} <- Copies],
    mnesia:wait_for_tables(mnesia:system_info(tables), 60000),
    ok.
