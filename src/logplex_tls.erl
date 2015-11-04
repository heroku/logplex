%%%-------------------------------------------------------------------
%% @copyright Heroku 
%% @doc TLS Client Helper Module
%% @end
%%%-------------------------------------------------------------------

-module(logplex_tls).

-include("logplex_logging.hrl").
-include_lib("ex_uri/include/ex_uri.hrl").

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

-export([log_error/2]).

-record(user_state, {channel_id :: logplex_channel:id(),
                     drain_id :: logplex_drain:id(),
                     dest :: #ex_uri{},
                     host :: ssl:host(),
                     mode :: secure | insecure}).

connect_opts(ChannelID, DrainID, Dest) ->
    UserState = user_state(ChannelID, DrainID, Dest),
    [{verify_fun, verify_fun_and_data(UserState)},
     {depth, max_depth()},
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

cacertfile(Path) ->
    application:set_env(logplex, tls_cacertfile, Path).

approved_ciphers() ->
    %% TODO use a static list of ciphers
    lists:filter(fun is_128_aes/1, ssl:cipher_suites()).

%% Internal API

is_128_aes({_, aes_128_cbc, _}) -> true;
is_128_aes({_, aes_128_gcm, _}) -> true;
is_128_aes(_) -> false.

verify_none(_, {bad_cert, _}, UserState) ->
    {valid, UserState};
verify_none(_, {extension, _}, UserState) ->
    {unknown, UserState};
verify_none(_, valid, UserState) ->
    {valid, UserState};
verify_none(_, valid_peer, UserState) ->
    {valid, UserState}.

verify_host(_Cert, {bad_cert, _}=Reason, UserState) ->
    log_error(Reason, UserState),
    {fail, Reason};
verify_host(_Cert, {extension, _}=Reason, UserState) ->
    {unknown, UserState};
verify_host(_Cert, valid, UserState) ->
    {valid, UserState};
verify_host(Cert, valid_peer, #user_state{ host=Host }=UserState) ->
    Reply = ssl_verify_hostname:verify_cert_hostname(Cert, Host),
    handle_reply(Reply, UserState).
 
%% Private Functions

handle_reply({valid, Host}, #user_state{ host=Host }=UserState) ->
    {valid, UserState};
handle_reply({fail, Reason}=Error, UserState) ->
    log_error(Reason, UserState),
    Error.

log_error(Reason, #user_state{ channel_id=ChannelID, drain_id=DrainID, dest=Dest }) ->
    ?ERR("channel_id=~p drain_id=~p dest=~s at=log_error err=~p",
         [ChannelID, DrainID, logplex_drain:uri_to_binary(Dest), Reason]).
