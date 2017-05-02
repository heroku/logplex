-module(logplex_api_v3_healthcheck).

-export([init/3,
         handle/2,
         terminate/3
        ]).

%% @private
init(_Transport, Req, _Opts) ->
    {ok, Req, no_state}.

%% @private
handle(Req, State) ->
    try
        case logplex_api_v3:is_authorized(Req, State) of
            {true, Req1, State} ->
                RegisteredMods = [logplex_stats, logplex_tail, logplex_shard, tcp_acceptor],
                [case whereis(Mod) of
                     Pid when is_pid(Pid) ->
                         true = is_process_alive(Pid);
                     _Other ->
                         throw(io_lib:format("Process dead: ~p", [Mod]))
                 end || Mod <- RegisteredMods],
                Status = list_to_binary(atom_to_list(logplex_app:config(api_status, normal))),
                Resp = jsx:encode([{<<"status">>, Status}]),
                Req2 = cowboy_req:set_resp_body(Resp, Req1),
                Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
                {ok, Req4} = cowboy_req:reply(200, Req3),
                logplex_stats:healthcheck(),
                {ok, Req4, State};
            {_NotAuthorized, Req1, State} ->
                {ok, Req2} = cowboy_req:reply(401, Req1),
                {ok, Req2, State}
        end
    catch
        _Error:_Reason ->
            {ok, Rq} = cowboy_req:reply(503, Req),
            {ok, Rq, State}
    end.

%% @private
terminate(_Reason, _Req, _State) ->
    ok.

