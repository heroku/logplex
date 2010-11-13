-module(logplex_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    boot_redis(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    Opts = [
        {ip, "0.0.0.0"},
        {port, 80},
        {backlog, 1024},
        {loop, {logplex_api, loop}},
        {name, logplex_api}
    ],
    _SslOpts = [
        {ip, "0.0.0.0"},
        {port, 443},
        {backlog, 1024},
        {loop, {logplex_api, loop}},
        {name, logplex_ssl_api},
        {ssl, true},
        {ssl_opts, [
            {certfile, "priv/server_cert.pem"},
            {keyfile, "priv/server_key.pem"}
        ]}
    ],
    {ok, {{one_for_one, 5, 10}, [
        {logplex, {logplex, start_link, []}, permanent, 2000, worker, [logplex]},
        {logplex_grid, {logplex_grid, start_link, []}, permanent, 2000, worker, [logplex_grid]},
        {syslog_server, {syslog_server, start_link, []}, permanent, 2000, worker, [syslog_server]},
        {logplex_api, {logplex_api, start_link, [Opts]}, permanent, 2000, worker, [logplex_api]},
        %{logplex_ssl_api, {logplex_api, start_link, [SslOpts]}, permanent, 2000, worker, [logplex_api]},
        {logplex_tail, {logplex_tail, start_link, []}, permanent, 2000, worker, [logplex_tail]},
        {logplex_drain, {logplex_drain, start_link, []}, permanent, 2000, worker, [logplex_drain]}
    ]}}.

boot_redis() ->
    case application:start(redis, temporary) of
        ok ->
            case os:getenv("REDIS_URL") of
                false -> ok;
                Url ->
                    Opts = case redis_uri:parse(Url) of
                        {redis, UserInfo, Host, Port, _Path, _Query} ->
                            Pass = 
                                case UserInfo of
                                    "" -> undefined;
                                    Val -> list_to_binary(Val)
                                end,
                            [{ip, Host}, {port, Port}, {pass, Pass}];
                        _ ->
                            []
                    end,
                    redis_pool:cycle_pool(Opts)
            end,
            redis_pool:expand_pool(1);
        Err ->
            exit(Err)
    end.
