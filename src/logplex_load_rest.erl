%% @copyright Heroku
%% @author Andrew Gwozdziewycz <apg@heroku.com>
%% @version {@vsn}, {@date} {@time}
%% @doc Handler for the /load endpoint
%% @end

-module(logplex_load_rest).

-include("logplex_logging.hrl").

-export([init/3
        , allowed_methods/2
        , is_authorized/2
        , content_types_accepted/2
        , handle_load/2]).

init(_Transport, _Req, _) ->
    {upgrade, protocol, cowboy_res}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
    logplex_rest:is_authorized(Req, State).

content_types_accepted(Req, State) ->
    {[{{'*'}, handle_load}],
     Req, State}.

handle_load(Req, State) ->
    Body = cowboy_req:body(Req),
    Modules = mochijson2:decode(Body),
    case is_list(Modules) of
        true ->
            {Code, Json} = lists:foldl(
               fun(Module, {Code, Acc}) ->
                       Module1 = binary_to_atom(Module, latin1),
                       case c:l(Module1) of
                           {module, _} when Code == 200 -> {200, Acc};
                           {module, _} -> {Code, Acc};
                           {error, Reason} -> {400, [{Module, atom_to_binary(Reason, latin1)}|Acc]}
                       end
               end, {200, []}, Modules),
            case Code of
                200 -> {true, Req, State};
                _ ->
                    logplex_rest:reply(Code, iolist_to_binary(mochijson2:encode(Json))),
                    {halt, Req, State}
            end;
        _ ->
            cowboy_req:reply(500, <<"expected list">>),
            {halt, Req, State}
    end.
