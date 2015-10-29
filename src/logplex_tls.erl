%%%-------------------------------------------------------------------
%% @copyright Heroku 
%% @doc TLS Client Helper Module
%% @end
%%%-------------------------------------------------------------------

-module(logplex_tls).

                                                % Public API
-export([connect_opts/1,
         max_depth/0,
         max_depth/1,
         cacertfile/0,
         cacertfile/1,
         approved_ciphers/0]).

                                                % Internal API
-export([is_128_aes/1,
         verify/3]).

connect_opts(Host, Insecure) ->
    [{verify_fun, verify_opt(Host, Insecure)},
     {depth, max_depth()},
     {cacertfile, cacertfile()},
     {ciphers, approved_ciphers()}].

verify_opt(_Host, Insecure) when Insecure =:= true ->
    {fun verify_none/3, []};
verify_opt(Host, _Secure) ->
    {fun verify_host/3, [{check_hostname, Host}]}.

verify_none(_, {bad_cert, _}, UserState) ->
    {valid, UserState};
verify_none(_, {extension, _}, UserState) ->
    {unknown, UserState};
verify_none(_, valid, UserState) ->
    {valid, UserState};
verify_none(_, valid_peer, UserState) ->
    {valid, UserState}.

verify_host(_Cert,{bad_cert, _} = Reason, _) ->
    {fail, Reason};
verify_host(_Cert,{extension, _}, UserState) ->
    {unknown, UserState};
verify_host(_Cert, valid, UserState) ->
    {valid, UserState};
verify_host(Cert, valid_peer, UserState) ->
    Hostname = proplists:get_value(check_hostname, UserState),
    verify_cert_hostname(Cert, Hostname, UserState).

verify_cert_hostname(Cert, undefined, UserState) ->
    {valid, UserState};
verify_cert_hostname(Cert, Hostname, UserState) ->
    ssl_verify_hostname:verify_cert_hostname(Cert, Hostname).

max_depth() ->
    %% OpenSSL defaults to a max depth of 100, it used to use 9.
    logplex_app:config(tls_max_depth).

max_depth(Depth) ->
    application:set_env(logplex, tls_max_depth, Depth).

cacertfile() ->
    filename:absname(logplex_app:config(tls_cacertfile), logplex_app:priv_dir()).

cacertfile(Path) ->
    application:set_env(logplex, tls_cacertfile, Path).

approved_ciphers() ->
                                                % TODO use a static list of ciphers
    lists:filter(fun is_128_aes/1, ssl:cipher_suites()).

is_128_aes({_, aes_128_cbc, _}) -> true;
is_128_aes({_, aes_128_gcm, _}) -> true;
is_128_aes(_) -> false.
