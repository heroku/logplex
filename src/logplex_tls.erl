%%%-------------------------------------------------------------------
%% @copyright Heroku 
%% @doc TLS Client Helper Module
%% @end
%%%-------------------------------------------------------------------

-module(logplex_tls).

-include("logplex_logging.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Public API Exports
-export([connect_opts/3,
         max_depth/0,
         max_depth/1,
         cacertfile/0,
         cacertfile/1,
         pinned_certs/0,
         pinned_certs/1,
         pinned_certfile/0,
         pinned_certfile/1,
         approved_ciphers/0,
         approved_ciphers/1,
         cache_env/0]).

%% Internal API Exports
-export([is_128_aes/1,
         verify_host/3,
         verify_none/3]).

-define(set_env(Key, Value), (application:set_env(logplex, Key, Value))).
-define(get_env(Key), (logplex_app:config(Key))).

-record(user_state, {channel_id :: logplex_channel:id(),
                     drain_id :: logplex_drain:id(),
                     dest :: #ex_uri{},
                     host :: ssl:host(),
                     mode :: secure | insecure,
                     depth = -1 :: pos_integer()}).

%% Public API

%% @doc Returns TLS connection options.
connect_opts(ChannelID, DrainID, Dest) ->
    UserState = user_state(ChannelID, DrainID, Dest),
    [{verify_fun, verify_fun_and_data(UserState)},
     {depth, max_depth()},
     {partial_chain, fun partial_chain/1},
     {reuse_sessions, false},
     {cacertfile, cacertfile()},
     {ciphers, approved_ciphers()}].

%% @doc Returns the maximum number of non Root CA certificates.
%% @see ssl:ssloption()
-spec max_depth() -> pos_integer().

max_depth() ->
    %% OpenSSL defaults to a max depth of 100, it used to use 9.
    ?get_env(tls_max_depth).

%% @doc Sets the maximum number of non Root CA certificates.
%% @see ssl:ssloption()
-spec max_depth(pos_integer()) -> ok.

max_depth(Depth) when is_integer(Depth) ->
    ?set_env(tls_max_depth, Depth).

%% @doc Returns the PEM formatted cacertfile bundle location.
-spec cacertfile() -> ssl:path().

cacertfile() ->
    ?get_env(tls_cacertfile).

%% @doc Sets the PEM formatted cacertfile bundle location.
-spec cacertfile(ssl:path()) -> ok.

cacertfile(Path) ->
    ?set_env(tls_cacertfile, Path).

%% @doc Returns the PEM formatted pinned certificates.
%% Takes precedence over pinned_certfile().
-spec pinned_certs() -> [public_key:der_encoded()] | [].

pinned_certs() ->
    ?get_env(tls_pinned_certs).

%% @doc Set the DER formatted pinned certificates.
%% Takes precedence over pinned_certfile().
-spec pinned_certs([public_key:der_encoded()]) -> ok.

pinned_certs(DerCerts) ->
    ?set_env(tls_pinned_certs, DerCerts).

%% @doc Get the PEM formatted pinned certificates file bundle.
-spec pinned_certfile() -> ssl:path().

pinned_certfile() ->
    absname(?get_env(tls_pinned_certfile)).

%% @doc Set the PEM formatted pinned certificates file bundle.
-spec pinned_certfile(ssl:path()) -> ok.

pinned_certfile(Path) ->
    ?set_env(tls_pinned_certfile, Path).

%% @doc Get the approved cipher suites list.
-spec approved_ciphers() -> ssl:ciphers().

approved_ciphers() ->
    ?get_env(tls_ciphers).

%% @doc Set the approved cipher suites list.
-spec approved_ciphers(ssl:ciphers()) -> ok.

approved_ciphers(Ciphers) when is_list(Ciphers) ->
    ?set_env(tls_ciphers, Ciphers).

%% @doc Formats and caches various default settings.
%% The following settings are cached.
%% <ul>
%% <li>The cacertfile is converted to an absolute path.</li>
%% <li>The ssl cipher suites are filtered down to an approved list of 128 bit AES ciphers.</li> 
%% <li>The pinned certificates are possibly read a bundle file and converted from PEM format to DER format.</li> 
%% </ul>
-spec cache_env() -> ok.

cache_env() ->
    cacertfile(absname(?get_env(tls_cacertfile))),

    approved_ciphers(lists:filter(fun is_128_aes/1, ssl:cipher_suites())),

    pinned_certs(case pinned_certs() of
                     [] -> pinned_certs_from_file();
                     Certs -> pem_to_der(Certs)
                 end),
    ok.

%% Internal API

is_128_aes({_, aes_128_cbc, _}) -> true;
is_128_aes({_, aes_128_gcm, _}) -> true;
is_128_aes(_) -> false.

partial_chain(Chain) ->
    partial_chain(pinned_certs(), Chain).

partial_chain([], _Chain) ->
    unknown_ca;
partial_chain(Pinned0, Chain0) ->
    Certs = [{Cert, public_key:pkix_decode_cert(Cert, otp)} || Cert <- Chain0],
    Pinned = [public_key:pkix_decode_cert(Cert, otp) || Cert <- Pinned0],
    case find(fun({_, Cert}) ->
                      check_cert(Pinned, Cert)
              end, Certs) of
        {ok, Trusted} -> {trusted_ca, element(1, Trusted)};
        _ -> unknown_ca
    end.

verify_none(_, {bad_cert, _}, UserState) ->
    {valid, UserState};
verify_none(_, {extension, _}, UserState) ->
    {unknown, UserState};
verify_none(_, valid, UserState) ->
    {valid, UserState};
verify_none(_, valid_peer, UserState) ->
    {valid, UserState}.

verify_host(_Cert, {bad_cert, Reason}, UserState) ->
    ?ERR("channel_id=~p drain_id=~p dest=~s depth=~b at=verify_host "
         "err=bad_cert reason=~p",
         log_args(UserState, [Reason])),
    {fail, Reason};
verify_host(_Cert, {extension, _}, UserState0) ->
    {unknown, UserState0};
verify_host(_Cert, valid, UserState0) ->
    {valid, incr_depth(UserState0)};
verify_host(Cert, valid_peer, #user_state{ host=Host }=UserState) ->
    try ssl_verify_hostname:verify_cert_hostname(Cert, Host) of
        {valid, Host} -> {valid, incr_depth(UserState)};
        {fail, Reason}=Error ->
            ?ERR("channel_id=~p drain_id=~p dest=~s depth=~b at=verify_host failure=Reason",
                 log_args(UserState, [Reason])),
            Error
    catch
        Error:Reason ->
            ?ERR("channel_id=~p drain_id=~p dest=~s depth=~b at=verify_host err=~p reason=~p trace=~p",
                 log_args(UserState, [Error, Reason, erlang:get_stacktrace()])),
            {fail, unexpected_error}
    end.

%% Private Functions

verify_fun_and_data(#user_state{ mode=insecure }) ->
    {fun verify_none/3, []};
verify_fun_and_data(#user_state{}=InitialUserState) ->
    {fun verify_host/3, InitialUserState}.

user_state(ChannelID, DrainID, Dest) ->
    {Host, _Port, Mode} = logplex_drain:unpack_uri(Dest),
    #user_state{channel_id=ChannelID,
                drain_id=DrainID,
                dest=Dest,
                host=Host,
                mode=Mode}.

absname(Path) ->
    filename:absname(Path, logplex_app:priv_dir()).

pem_to_der([]) ->
    [];
pem_to_der(Certs) when is_binary(Certs) ->
    Pems = public_key:pem_decode(Certs),
    [Der || {'Certificate', Der, _} <- Pems].

pinned_certs_from_file() ->
    case file:read_file(pinned_certfile()) of
        {ok, Certs} -> pem_to_der(Certs);
        {error, enoent} ->
            []
    end.

log_args(#user_state{ channel_id=ChannelID, drain_id=DrainID, dest=Dest, depth=Depth }, Rest) ->
    [ChannelID, DrainID, logplex_drain:uri_to_binary(Dest), Depth | Rest].

incr_depth(#user_state{ depth=D }=UserState0) ->
    UserState0#user_state{ depth=D+1 }.

extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

check_cert(Pinned, Cert) ->
    CertPubKey = extract_public_key_info(Cert),
    lists:any(fun(CACert) ->
                      CACertPubKey = extract_public_key_info(CACert),
                      CertPubKey == CACertPubKey
              end, Pinned).

find(Fun, [Head|Tail]) when is_function(Fun) ->
    case Fun(Head) of
        true ->
            {ok, Head};
        false ->
            find(Fun, Tail)
    end;
find(_Fun, []) ->
    error.
