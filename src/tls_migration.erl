%% TLS migration script (temporary)
%%
%% Run from the console as:
%% > l(tls_migration). tls_migration:main(ok).

-module(tls_migration).

-export([main/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").

main(_) ->
    ets:foldl(fun(Drain, _Acc) ->
                      case should_migrate_drain(Drain) of
                          true ->
                              migrate_drain_maybe(Drain);
                          _ ->
                              ok
                      end
              end, notused, drains),
    ok.

migrate_drain_maybe(Drain) ->
    case attempt_connection(Drain) of
        {error, Reason} ->
            handle_failed_drain(Drain, Reason);
        _ ->
            ok
    end.

handle_failed_drain(Drain, Reason) ->
    write_csv_row(Drain, Reason),
    % TODO: only do this on ssl errors (not all kinds of errors)
    toggle_insecure_and_save(Drain).

attempt_connection(#drain{id=DrainID, channel_id=ChannelID, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} = URI}) ->
    io:format("Attempting connection to ~p:~p for drain ~p~n", [Host, Port, DrainID]),
    case logplex_tlssyslog_drain:do_connect(Host, Port, URI, DrainID, ChannelID) of
        {ok, _SslSocket} ->
            ok;
        {error, {tls_alert, Alert}=Reason} ->
            {error, Reason};
        {error, OtherReason} ->
            io:format("Non-TLS error ~p~n", [OtherReason]),
            ok
    end.

toggle_insecure_and_save(Drain) ->
    % TODO: toggle insecure and save to redis
    skip.

should_migrate_drain(#drain{id=_DrainId, type=DrainType, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} }) ->
    (DrainType =:= 'tlssyslog') and (Host =:= "logs.papertrailapp.com").

write_csv_row(#drain{id=DrainID, channel_id=ChannelID, uri=DrainURI}=Drain, FailureReason) ->
    % We just dump CSV rows interleaved with regular logging line, and as such
    % hackily distinguish them using the CSV prefix (we expect the runner to
    % filter using `| grep '^CSV'` for processing the CSV data)
    io:format("CSV ~p, ~p, ~p, ~p~n", [ChannelID, DrainID, DrainURI, FailureReason]).
