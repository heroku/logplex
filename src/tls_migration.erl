%% TLS migration script (temporary)
%%
%% Run from the console as:
%% > l(tls_migration). tls_migration:main(ok).

-module(tls_migration).

-export([main/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").

% TODO support --dry-run option
main(_) ->
    ets:foldl(fun(Drain, Results) ->
                      case should_migrate_drain(Drain) of
                          true ->
                              % TODO: sleep before connections to not overload papertrail
                              case migrate_drain_maybe(Drain) of
                                  skip ->
                                      Results;
                                  {Drain, Reason} ->
                                      [{Drain, Reason} | Results]
                              end;
                          _ ->
                              Results
                      end
              end, [], drains).

migrate_drain_maybe(Drain) ->
    case attempt_connection(Drain) of
        {error, Reason} ->
            handle_failed_drain(Drain, Reason);
        _ ->
            skip
    end.

handle_failed_drain(Drain, Reason) ->
    toggle_insecure_and_save(Drain),
    {Drain, Reason}.

attempt_connection(#drain{id=DrainID, channel_id=ChannelID, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} = URI}) ->
    io:format("Attempting connection to ~p:~p for drain ~p~n", [Host, Port, DrainID]),
    case logplex_tlssyslog_drain:do_connect(Host, Port, URI, DrainID, ChannelID) of
        {ok, _SslSocket} ->
            io:format("OK"),
            ok;
        {error, {tls_alert, _Alert}=Reason} ->
            io:format("TLS error ~p~n", [Reason]),
            {error, Reason};
        {error, OtherReason} ->
            % TODO: what do do with this? timeouts should be retried.
            %  maybe return 'other' failed drains in the accumulator
            io:format("Non-TLS error ~p~n", [OtherReason]),
            ok
    end.

should_migrate_drain(#drain{id=_DrainId, type=DrainType, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} }) ->
    % TODO: filter out #insecure drains.
    (DrainType =:= 'tlssyslog') and (Host =:= "logs.papertrailapp.com").

toggle_insecure_and_save(#drain{id=DrainID, channel_id=ChannelID, token=Token, uri=URI}) ->
    NewURI = erlang:iolist_to_binary([logplex_drain:uri_to_binary(URI), <<"#insecure">>]),
    io:format("Changing drain ~p URI to ~p~n", [DrainID, NewURI]),
    case redis_helper:create_url_drain(DrainID, ChannelID, Token, NewURI) of
        ok ->
            ok;
        Err ->
            io:format("REDIS ERROR updating drain ~p: ~p~n", [DrainID, Err])
    end.
