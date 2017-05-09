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
	{ok, Req1} = case logplex_app:elb_healthcheck() of
					 healthy ->
						 cowboy_req:reply(200, [], <<"OK">>, Req);
					 unhealthy ->
						 cowboy_req:reply(503, [], <<"SYSTEM BOOTING">>, Req)
				 end,
	{ok, Req1, State}.

%% @private
terminate(_Reason, _Req, _State) ->
    ok.

