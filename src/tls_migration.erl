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
    toggle_insecure_and_save(Drain).

attempt_connection(#drain{id=DrainID, channel_id=ChannelID, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} = URI}) ->
    io:format("Attempting connection to ~p:~p for drain ~p~n", [Host, Port, DrainID]),
    case logplex_tlssyslog_drain:do_connect(Host, Port, URI, DrainID, ChannelID) of
        {ok, _SslSocket} ->
            ok;
        {error, {tls_alert, _Alert}=Reason} ->
            {error, Reason};
        {error, OtherReason} ->
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
 
write_csv_row(#drain{id=DrainID, channel_id=ChannelID, uri=DrainURI}=Drain, FailureReason) ->
    % We just dump CSV rows interleaved with regular logging line, and as such
    % hackily distinguish them using the CSV prefix (we expect the runner to
    % filter using `| grep '^CSV'` for processing the CSV data)
    io:format("CSV ~p, ~p, ~p, ~p~n", [ChannelID, DrainID, DrainURI, FailureReason]).
