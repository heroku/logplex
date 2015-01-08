f(UpgradeNode).
UpgradeNode = fun () ->
    OldVsn = "v78",
    NextVsn = "v79",
    case logplex_app:config(git_branch) of
        OldVsn ->
            io:format(whereis(user), "at=upgrade_start cur_vsn=~p~n", [tl(OldVsn)]);
        NextVsn ->
            io:format(whereis(user),
                      "at=upgrade type=retry cur_vsn=~p old_vsn=~p~n", [tl(OldVsn), tl(NextVsn)]);
        Else ->
            io:format(whereis(user),
                      "at=upgrade_start old_vsn=~p abort=wrong_version", [tl(Else)]),
            erlang:error({wrong_version, Else})
    end,

    % lookup all the HTTP drain pids
    Drains = [ Pid || {Pid, http} <- gproc:lookup_local_properties(drain_type)],


    % suspend new drains from being created
    sys:suspend(logplex_drain_sup),

    % suspend all the http drains in preparation for a code change
    [ ok = sys:suspend(Pid, 60000) || Pid <- Drains, erlang:is_process_alive(Pid) ],

    % load the new version of the module
    l(logplex_http_drain),

    % resume allowing drain creation
    sys:resume(logplex_drain_sup),

    % perform the state change via code_change
    [ ok = sys:change_code(Pid, logplex_http_drain, OldVsn, undefined, 60000) || Pid <- Drains, erlang:is_process_alive(Pid) ],

    % resume operation of all the http drains
    [ ok = sys:resume(Pid, 60000) || Pid <- Drains, erlang:is_process_alive(Pid) ],


    io:format(whereis(user), "at=upgrade_end cur_vsn=~p~n", [NextVsn]),
    ok = application:set_env(logplex, git_branch, NextVsn),
    ok
end.

f(NodeVersions).
NodeVersions = fun () ->
    lists:keysort(3,
                  [{N,
                    element(2, rpc:call(N, application, get_env, [logplex, git_branch])),
                    rpc:call(N, os, getenv, ["INSTANCE_NAME"])}
                   || N <- [node() | nodes()] ])
end.

f(NodesAt).
NodesAt = fun (Vsn) ->
    [ N || {N, V, _} <- NodeVersions(), V =:= Vsn ]
end.

f(NextNodesAt).
NextNodesAt = fun(Vsn, N) -> lists:sublist(NodesAt(Vsn), N) end.

f(RollingUpgrade).
RollingUpgrade = fun (Nodes) ->
  lists:foldl(fun (N, {good, Upgraded}) ->
    case rpc:call(N, erlang, apply, [ UpgradeNode, [] ]) of
      ok ->
        {good, [N | Upgraded]};
      Else ->
        {{bad, N, Else}, Upgraded}
    end;
    (N, {_, _} = Acc) -> Acc
    end,
    {good, []},
    Nodes)
end.
