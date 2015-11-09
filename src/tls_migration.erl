%% TLS migration script (temporary)
%%
%% Run from the console as:
%% > l(tls_migration).
%% > tls_migration:main(ok).

-module(tls_migration).

-export([main/1]).

-include_lib("ex_uri/include/ex_uri.hrl").
-include("logplex_drain.hrl").
-include("logplex_logging.hrl").

main(_) ->
    ets:foldl(fun(Drain, Acc) ->
                      case should_migrate_drain(Drain) of
                          true ->
                              migrate_drain(Drain);
                          _ ->
                              ok
                      end
              end, notused, drains),
    ok.

migrate_drain(Drain) ->
    case attempt_connection(Drain) of
        {error, Reason} ->
            io:format("Drain ~p failed; ~p~n", [Drain, Reason]),
            toggle_insecure_and_save(Drain);
        _ ->
            ok
    end.

attempt_connection(#drain{id=DrainID, channel_id=ChannelID, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} = URI}) ->
    io:format("Attempting connection to ~p:~p for drain ~p~n", [Host, Port, DrainID]),
    case logplex_tlssyslog_drain:do_connect(Host, Port, URI, DrainID, ChannelID) of
        {ok, _SslSocket} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

toggle_insecure_and_save(Drain) ->
    % TODO: toggle insecure and save to redis
    skip.

should_migrate_drain(#drain{id=_DrainId, type=DrainType, uri=#ex_uri{authority=#ex_uri_authority{host=Host, port=Port}} }) ->
    (DrainType =:= 'tlssyslog') and (Host =:= "logs.papertrailapp.com").
