%%% Logplex has the presence of reference-counted binaries (refc binaries) that
%%% are leaking (see
%%% http://www.erlang.org/doc/efficiency_guide/binaryhandling.html#id65722).
%%% The problem is that all drain processes end up doing little more than
%%% routing binaries that may or may not be reference-counted. After enough
%%% work done by a drain -- or a larger mailbox, or anything else -- it may get
%%% more space allocated for its stack and heap. When that space is garbage-
%%% collected or compacted following hibernation (if any), future refc binaries
%%% will feel the process space one by one, each being one single pointer in a
%%% list of references.
%%%
%%% Garbage collection may take far longer to trigger for 100,000 refc binaries
%%% than for far fewer non-counted binaries, or may just as well never happen.
%%% In this case, the memory is never reclaimed and we have a leak.
%%%
%%% There exist decent work-arounds for this -- fiddling with hibernation,
%%% different GC strategies (tracking refc binary space and doing it manually),
%%% doing it on a per-process basis, and so on.
%%%
%%% However, because production nodes might be suffering right now, this server
%%% acts as a quick fix where a max memory threshold may be given, and within 5
%%% minutes, the VM will go through a full GC for the entire VM.  This is not
%%% optimal, and the effect can be bad given potential adverse effect on the
%%% generational garbage collector, but it's better than what we have right now
%%% -- nothing and crash dumps.
%%%
%%% We believe the leak to be real following usage of a function such as
%%% https://gist.github.com/ferd/6028931 that revealed gigabytes of data would
%%% be freed, and hundreds of thousands of refc binary references being behind
%%% the savings.
-module(logplex_leak).
-behaviour(gen_server).
-include("logplex_logging.hrl").

-define(SLEEP, timer:minutes(5)).
-define(THRESHOLD, 10000000000). % arbitrary value!

-record(state, {tref}).

-export([start_link/0, force/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% Public Inerface %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

force() ->
    gen_server:call(?MODULE, force, timer:seconds(10)).

%%%%%%%%%%%%%%%%%%
%%% gen_server %%%
%%%%%%%%%%%%%%%%%%

init([]) ->
    Ref = erlang:start_timer(0, self(), gc),
    {ok, #state{tref=Ref}}.

handle_call(force, _From, S=#state{tref=Ref}) ->
    erlang:cancel_timer(Ref),
    Before = erlang:memory(total),
    [erlang:garbage_collect(Pid) || Pid <- processes()],
    NewRef = erlang:start_timer(?SLEEP, self(), gc),
    After = erlang:memory(total),
    ?INFO("at=gc mem_pre=~p mem_post=~p type=forced", [Before,After]),
    {reply, ok, S#state{tref=NewRef}};
handle_call(_, _From, State=#state{}) ->
    {noreply, State}.

handle_cast(_Unknown, State=#state{}) ->
    {noreply, State}.

handle_info({timeout, Ref, gc}, S=#state{tref=Ref}) ->
    Mem = erlang:memory(total),
    case Mem >= logplex_app:config(force_gc_memory, ?THRESHOLD) of
        true ->
            [erlang:garbage_collect(Pid) || Pid <- processes()],
            NewRef = erlang:start_timer(?SLEEP, self(), gc),
            After = erlang:memory(total),
            ?INFO("at=gc mem_pre=~p mem_post=~p type=timeout", [Mem,After]),
            {noreply, S#state{tref=NewRef}};
        false ->
            NewRef = erlang:start_timer(?SLEEP, self(), gc),
            {noreply, S#state{tref=NewRef}}
    end;
handle_info(_WhoCares, State=#state{}) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok,State}.

terminate(_,_) -> ok.
