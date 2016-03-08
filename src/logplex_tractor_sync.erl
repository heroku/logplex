-module(logplex_tractor_sync).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {name :: {local, atom()} | {global, term()},
                status = disconnected :: disconnected|already_diffing}).

%%% API

start_link(Name) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Name], []).

%%% gen_server callbacks

init([Name]) ->
    {ok, #state{name=Name}, 0}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{ name=Name, status=disconnected }=State) ->
    sdiff_client:ready(Name),
    case sdiff_client:sync_diff(Name) of
        {ok, done} ->
            application:set_env(logplex, tractor_loaded, true),
            {noreply, State#state{ status=done }};
        {ok, Status} ->
            {noreply, State#state{ status=Status }, 5000}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions
