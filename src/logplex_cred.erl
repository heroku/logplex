%%%-------------------------------------------------------------------
%% @copyright Heroku, 2012
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Logplex credentials
%% @end
%%%-------------------------------------------------------------------
-module(logplex_cred).

-include("logplex_logging.hrl").

%% API
-export([from_dict/2
         ,cache/1
         ,store/1
         ,destroy/1
         ,lookup/1
         ,delete/1
         ,create_ets_table/0
         ,new/0
         ,new/1
         ,id/0
         ,id/1
         ,id_to_binary/1
         ,binary_to_id/1
        ]).

-type perm() :: 'any_channel' | 'core' | 'ion' |
                {'channel', logplex_channel:id()}.

-record(cred, {id :: binary(),
               pass :: binary(),
               perms = ordsets:new() :: ordsets:ordset(perm())
              }).

-define(CRED_TAB, creds).

%%====================================================================
%% API
%%====================================================================

create_ets_table() ->
    ets:new(?CRED_TAB, [named_table, public, set, {keypos, 2}]).

new(Id) when is_binary(Id) -> #cred{id = Id}.

new() ->
    #cred{id = id(),
          pass = pass()}.

id(#cred{id = Id}) -> Id.

id() ->
    bytes_to_iolist(crypto:rand_bytes(16)).

pass() ->
    bytes_to_iolist(crypto:rand_bytes(32)).

bytes_to_iolist(Bytes) ->
    << <<(hd(integer_to_list(Nib, 16))):8>> || <<Nib:4>> <= Bytes >>.

from_dict(Id, Dict) ->
    dict:fold(fun cred_from_dict/3, new(Id), Dict).

cache(#cred{} = Cred) ->
    ets:insert(?CRED_TAB, Cred).

delete(Id) ->
    ets:delete(?CRED_TAB, Id).

lookup(Id) ->
    case ets:lookup(?CRED_TAB, Id) of
        [Cred = #cred{}] -> Cred;
        _ -> no_such_cred
    end.

store(#cred{id = Id,
            pass = Pass,
            perms = Perms}) ->
    redis_helper:store_cred(Id, Pass, perms_to_dict(Perms)).

destroy(#cred{id = Id}) ->
    redis_helper:delete_cred(Id).

id_to_binary(Bin) when is_binary(Bin) -> Bin.
binary_to_id(Bin) -> Bin.

%%====================================================================
%% Internal functions
%%====================================================================

cred_from_dict(<<"pass">>, Pass, Cred = #cred{}) ->
    Cred#cred{pass = Pass};

cred_from_dict(<<"full_api">>, _, Cred = #cred{perms = Perms}) ->
    Cred#cred{perms = ordsets:add_element(full_api, Perms)};
cred_from_dict(<<"channel">>, <<"any">>, Cred = #cred{perms = Perms}) ->
    Cred#cred{perms = ordsets:add_element(any_channel, Perms)};
cred_from_dict(<<"channel">>, ChannelIdB, Cred = #cred{perms = Perms}) ->
    ChannelId = logplex_channel:binary_to_id(ChannelIdB),
    Cred#cred{perms = ordsets:add_element({chan, ChannelId}, Perms)};

cred_from_dict(Key, Value, Cred) ->
    ?WARN("at=unknown_key key=~p value=~p cred_id=~p",
          [Key, Value, Cred#cred.id]),
    Cred.

perms_to_dict(Perms) ->
    [ case Perm of
          core -> {<<"full_api">>, <<"1">>};
          any_channel -> {<<"channel">>, <<"any">>};
          {channel, Id} -> {<<"channel">>, logplex_channel:id_to_binary(Id)}
      end
      || Perm <- ordsets:to_list(Perms) ].
