%% TLS migration script (temporary)
%%
%% Run from the console as:
%% > l(tls_migration). tls_migration:main(ok).

-module(tls_migration).

-export([main/1]).

-export([should_migrate_drain/1,
         migrate_drain_maybe/1,
         unflag_all_insecure_drains/0,
         test/0]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").

% TODO support --dry-run option
main(_) ->
    dict_from_list(migrate_all_drains()).

%% Returns a list of tuple of the form:
%%    {Action, Drain, Reason}
%% where Action is one of:
%%    - migrate :: drain was migrated
%%    - unhealthy :: drain doesn't even connect in insecure
%%    - unhealthy_if_tls :: connects in insecure, but fails to verify
%%    - healthy :: connects fine, no need to migrate
migrate_all_drains() ->
    ets:foldl(fun(Drain, Results) ->
                      case should_migrate_drain(Drain) of
                          true ->
                              % TODO: sleep before connections to not overload papertrail
                              case migrate_drain_maybe(Drain) of
                                  {migrate, {Drain, _Reason}}=Result ->
                                      mark_drain_as_insecure(Drain),
                                      [Result | Results];
                                  {_State, {_Drain, _Reason}}=Result ->
                                      [Result | Results]
                              end;
                          _ ->
                              Results
                      end
              end, [], drains).

migrate_drain_maybe(Drain) ->
    case attempt_connection({insecure, Drain}) of
        {error, Reason} ->
            % Drain never connects in the first place.
            {unhealthy, {Drain, Reason} };
        ok ->
            % Drains works, so let's try migrating it.
            migrate_drain(Drain)
    end.

migrate_drain(Drain) ->
    case attempt_connection({default, Drain}) of
        {error, {tls_alert, _Alert}=Reason} ->
            % Verification failed. Make this insecure.
            {migrate, {Drain, Reason} };
        {error, Reason} ->
            {unhealthy_if_tls, {Drain, Reason} };
        ok ->
            % Connects fine, so leave as is.
            {healthy, Drain}
    end.

% TODO: have this take a force_insecure option.
attempt_connection({insecure, #drain{uri=URI}=Drain}) ->
    NewURI = logplex_drain:parse_url(
              new_uri_with_insecure(
               logplex_drain:uri_to_binary(URI))),
    attempt_connection({default, Drain#drain{uri=NewURI}});
attempt_connection({default, #drain{id=DrainID, channel_id=ChannelID, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} = URI}}) ->
    io:format("Attempting connection to ~p:~p for drain ~p~n", [Host, Port, DrainID]),
    case logplex_tlssyslog_drain:do_connect(Host, Port, URI, DrainID, ChannelID) of
        {ok, _SslSocket} ->
            io:format("OK"),
            ok;
        {error, {tls_alert, _Alert}=Reason} ->
            io:format("TLS error ~p~n", [Reason]),
            {error, Reason};
        {error, OtherReason} ->
            % TODO: what to do with this? timeouts should be retried.
            %  maybe return 'other' failed drains in the accumulator
            io:format("Non-TLS error ~p~n", [OtherReason]),
            {error, OtherReason}
    end.

should_migrate_drain(#drain{type=DrainType, uri=#ex_uri{authority=#ex_uri_authority{host=Host}}}) ->
    % TODO: filter out #insecure drains.
    (DrainType =:= 'tlssyslog') and (Host =:= "logs.papertrailapp.com").

mark_drain_as_insecure(#drain{uri=URI}=Drain) ->
    update_drain_uri(Drain, new_uri_with_insecure(logplex_drain:uri_to_binary(URI))).

unmark_drain_as_insecure(#drain{uri=URI}=Drain) ->
    update_drain_uri(Drain, new_uri_without_insecure(logplex_drain:uri_to_binary(URI))).

new_uri_with_insecure(URI) ->
    erlang:iolist_to_binary([URI, <<"#insecure">>]).

new_uri_without_insecure(URI) ->
    erlang:iolist_to_binary(
      re:replace(URI,
                 "#insecure",
                 "",
                 [{return,list}])).

update_drain_uri(#drain{id=DrainID, channel_id=ChannelID, token=Token}, NewURI) ->
    io:format("Changing drain ~p URI to ~p~n", [DrainID, NewURI]),
    case redis_helper:create_url_drain(DrainID, ChannelID, Token, NewURI) of
        ok ->
            ok;
        Err ->
            io:format("REDIS ERROR updating drain ~p: ~p~n", [DrainID, Err])
    end.

dict_from_list(Items) when is_list(Items) ->
    lists:foldl(fun({Key, Value}, Dict) ->
                        dict:append_list(Key, Value, Dict)
                end,
                dict:new(),
                Items).

% ~~ Code that follows below are for testing only ~~

% To undo the effects of running this script.
unflag_all_insecure_drains() ->
    ets:foldl(fun(Drain, _Acc) ->
                      unmark_drain_as_insecure(Drain)
              end, [], drains).

test() ->
    unflag_all_insecure_drains(),
    main(unused).
