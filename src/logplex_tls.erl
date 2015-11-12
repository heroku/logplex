%%%-------------------------------------------------------------------
%% @copyright Heroku 
%% @doc TLS Client Helper Module
%% @end
%%%-------------------------------------------------------------------

-module(logplex_tls).

-include("logplex_logging.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Public API
-export([connect_opts/3,
         max_depth/0,
         max_depth/1,
         cacertfile/0,
         cacertfile/1,
         approved_ciphers/0]).

%% Internal API
-export([is_128_aes/1,
         verify_host/3,
         verify_none/3]).

-record(user_state, {channel_id :: logplex_channel:id(),
                     drain_id :: logplex_drain:id(),
                     dest :: #ex_uri{},
                     host :: ssl:host(),
                     mode :: secure | insecure,
                     depth = -1 :: pos_integer()}).

connect_opts(ChannelID, DrainID, Dest) ->
    UserState = user_state(ChannelID, DrainID, Dest),
    [{verify_fun, verify_fun_and_data(UserState)},
     {depth, max_depth()},
     {partial_chain, fun partial_chain/1},
     {reuse_sessions, false},
     {cacertfile, cacertfile()},
     {ciphers, approved_ciphers()}].

user_state(ChannelID, DrainID, Dest) ->
    {Host, _Port, Mode} = logplex_drain:unpack_uri(Dest),
    #user_state{channel_id=ChannelID,
                drain_id=DrainID,
                dest=Dest,
                host=Host,
                mode=Mode}.

verify_fun_and_data(#user_state{ mode=insecure }) ->
    {fun verify_none/3, []};
verify_fun_and_data(#user_state{}=InitialUserState) ->
    {fun verify_host/3, InitialUserState}.

max_depth() ->
    %% OpenSSL defaults to a max depth of 100, it used to use 9.
    logplex_app:config(tls_max_depth).

max_depth(Depth) ->
    application:set_env(logplex, tls_max_depth, Depth).

cacertfile() ->
    filename:absname(logplex_app:config(tls_cacertfile), logplex_app:priv_dir()).

pinned_certfile() ->
    filename:absname(logplex_app:config(tls_pinned_certfile), logplex_app:priv_dir()).

cacertfile(Path) ->
    application:set_env(logplex, tls_cacertfile, Path).

approved_ciphers() ->
    %% TODO use a static list of ciphers
    lists:filter(fun is_128_aes/1, ssl:cipher_suites()).

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

extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

pinned_certs() ->
    case file:read_file(pinned_certfile()) of
        {ok, Certs} ->
            Pems = public_key:pem_decode(Certs),
            [Der || {'Certificate', Der, _} <- Pems];
        {error, enoent} ->
            []
    end.

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

log_args(#user_state{ channel_id=ChannelID, drain_id=DrainID, dest=Dest, depth=Depth }, Rest) ->
    [ChannelID, DrainID, logplex_drain:uri_to_binary(Dest), Depth | Rest].

incr_depth(#user_state{ depth=D }=UserState0) ->
    UserState0#user_state{ depth=D+1 }.
