%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Syslog/HTTP handler for Logplex.
%% @end
-module(logplex_logs_rest).

-export([child_spec/0]).

-export([init/3
         ,rest_init/2
         ,content_types_accepted/2
         ,from_logplex/2
         ,process_post/2
         ,content_types_provided/2
         ,to_response/2
        ]).

child_spec() ->
    cowboy:child_spec(?MODULE, 100,
                      cowboy_tcp_transport,
                      [{port, logplex_app:config(http_log_input_port)}],
                      cowboy_http_protocol,
                      [{dispatch, [{'_', [{[<<"logs">>], ?MODULE, []}]}]}]).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_http_rest}.

rest_init(Req, _Opts) ->
    {ok, Req, undefined}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-logplex-1">>, []}, from_logplex},
      {{<<"application">>, <<"logplex-1">>, []}, from_logplex}],
     Req, State}.

from_logplex(Req, State) ->
    case cowboy_http_req:body(Req) of
        {ok, Body, Req1} ->
            Req2 = parse_logplex_body(Body, Req1),
            {true, Req2, State};
        {error, Why} ->
            io:format(standard_io, "Invalid logplex body: ~p.~n", [Why]),
            {false, Req, State}
    end.

process_post(Req, State) ->
    from_logplex(Req, State).

content_types_provided(Req, State) ->
    {[{{<<"text">>, <<"plain">>, []}, to_response}],
     Req, State}.

to_response(Req, State) ->
    {"OK", Req, State}.

parse_logplex_body(Body, Req) when is_binary(Body) ->
    erlang:error(not_implemented),
    Req.
