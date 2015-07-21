-module(tls_spike).

%% Using a free papertrail/logentries account obtain a Host:Port url
%%
%% To run this code:
%%
%%    rebar get-deps -C public.rebar.config % if not already done
%%    rebar compile
%%    wget http://curl.haxx.se/ca/cacert.pem
%%    ./bin/devel_logplex
%%    l(tls_spike).
%%    SocketPairs = tls_spike:run_all(Host, Port).
%%    tls_spike:send_test_logs(SocketPairs).
%%
%% The out in terminal and logged to papertrail will contain the TLS options used to get a working connection. In
%% the case of verify_peer mode, it will indicate the required depth to obtain a ROOT ca.

-export([run_all/2, send_test_logs/1, run/4, gen_msg/1]).

run_all(Host, Port) when is_integer(Port) ->
    start_all_apps(),
    [run(simple, Host, Port, verify_none_opts()),
     run(next_depth, Host, Port, next_verify_peer_opts([]))].

run(simple, Host, Port, Opts) ->
    start_all_apps(),
    handle_connect(ssl:connect(Host, Port, Opts), Opts);
run(next_depth, Host, Port, Opts) ->
    case run(simple, Host, Port, Opts) of
        {{error, {tls_alert, "handshake failure"}}, Opts} ->
            run(next_depth, Host, Port, next_verify_peer_opts(Opts));
        {_Socket, _Opts}=Result ->
            Result
    end.

handle_connect({ok, Socket}, Opts) ->
    {Socket, Opts};
handle_connect({error, _}=Error, Opts) ->
    {Error, Opts}.

verify_none_opts() ->
    [{verify, verify_none} | base_opts()].

next_verify_peer_opts(Opts) ->
    Depth = proplists:get_value(depth, Opts, -1),
    [{verify, verify_peer}, {depth, Depth+1} | base_opts()].

base_opts() ->
    [{reuse_sessions, false}, {cacertfile, filename:absname("cacert.pem")}].

start_all_apps() ->
    logplex_app:a_start(ssl, transient),
    ssl:start().

send_test_logs(SocketPairs) ->
    [ ssl:send(Socket, gen_msg(Opts)) || {Socket, Opts} <- SocketPairs ].

gen_msg(Opts) ->
    {ok, Source} = inet:gethostname(),
    Msg = logplex_syslog_utils:fmt(user,
                                   info,
                                   now,
                                   Source,
                                   <<"tls_spike">>,
                                   logfmt(Opts),
                                   []),
    SyslogMsg = logplex_syslog_utils:rfc5424(Msg),
    logplex_syslog_utils:frame(SyslogMsg).

% horribly inefficient but spike so meh
logfmt(Opts) ->
    Keys = proplists:get_keys(Opts),
    Values = [ proplists:get_value(Key, Opts) || Key <- Keys ],
    Pairs = lists:zipwith(fun (Key, Value) ->
                                  binary_to_list(iolist_to_binary(io_lib:format("~s=~p", [Key, Value])))
                  end, Keys, Values),
    string:join(Pairs, " ").
