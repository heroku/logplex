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
         ,new/2
         ,id/0
         ,id/1
         ,pass/1
         ,perms/1
         ,id_to_binary/1
         ,binary_to_id/1
         ,grant/2
        ]).

-export([verify_basic/1
         ,auth/2
         ,has_perm/2
        ]).

-type id() :: binary().
-type pass() :: binary().
-type reportable_perm() :: 'any_channel' | 'full_api'.
-type perm() :: reportable_perm() |
                {'channel', logplex_channel:id()}.

-record(cred, {id :: id(),
               pass :: pass(),
               perms = ordsets:new() :: ordsets:ordset(perm())
              }).

-define(CRED_TAB, creds).

%%====================================================================
%% API
%%====================================================================

create_ets_table() ->
    ets:new(?CRED_TAB, [named_table, public, set, {keypos, 2}]).

new(Id) when is_binary(Id) -> #cred{id = Id,
                                    pass = pass()}.
new(Id, Pass) when is_binary(Id), is_binary(Pass) ->
    #cred{id = Id,
          pass = Pass}.

new() ->
    #cred{id = id(),
          pass = pass()}.

id(#cred{id = Id}) -> Id.
perms(#cred{perms = Perms}) -> Perms.
pass(#cred{pass = Pass}) -> Pass.

id() ->
    bytes_to_iolist(crypto:rand_bytes(16)).

pass() ->
    bytes_to_iolist(crypto:rand_bytes(32)).

bytes_to_iolist(Bytes) ->
    << <<(hd(integer_to_list(Nib, 16))):8>> || <<Nib:4>> <= Bytes >>.

-spec from_dict(id(), dict()) -> #cred{}.
from_dict(Id, Dict) ->
    dict:fold(fun cred_from_dict/3, new(Id), Dict).

cache(#cred{} = Cred) ->
    ets:insert(?CRED_TAB, Cred).

delete(Id) ->
    ets:delete(?CRED_TAB, Id).

-spec lookup(id()) -> #cred{} | 'no_such_cred'.
lookup(Id) ->
    case ets:lookup(?CRED_TAB, Id) of
        [Cred = #cred{}] -> Cred;
        _ -> no_such_cred
    end.

store(#cred{id = Id,
            pass = Pass,
            perms = Perms}) ->
    maybe_report_operation(Id, Perms, store),
    redis_helper:store_cred(Id, Pass, perms_to_dict(Perms)).

destroy(#cred{id = Id}) ->
    redis_helper:delete_cred(Id).

id_to_binary(Bin) when is_binary(Bin) -> Bin.
binary_to_id(Bin) -> Bin.

-spec grant(perm(), #cred{}) -> #cred{}.
grant(Perm, Cred = #cred{perms = Perms}) ->
    valid = valid_perm(Perm),
    NewPerms = ordsets:add_element(Perm, Perms),
    Cred#cred{perms = NewPerms}.

-spec valid_perm(any()) -> 'valid' | 'invalid'.
valid_perm(full_api) -> valid;
valid_perm(any_channel) -> valid;
valid_perm({channel, Id}) when is_integer(Id) -> valid;
valid_perm(_) -> invalid.

-spec has_perm(perm(), #cred{}) -> 'permitted' | 'not_permitted'.
has_perm(Perm, Cred = #cred{}) ->
    case ordsets:is_element(Perm, perms(Cred)) of
        true -> permitted;
        false -> not_permitted
    end.


-spec verify_basic(string() | binary()) ->
                          {'authorized', #cred{}} |
                          {'error', term()}.
verify_basic(BasicAuthStr) ->
    try binary:split(base64:decode(BasicAuthStr), <<":">>) of
        [Id, Pass] ->
            auth(Id, Pass);
        [_] ->
            {error, not_basic_auth}
    catch
        _Class:_Ex ->
            {error, invalid_encoding}
    end.

-spec auth(id(), pass()) ->
                  {'authorized', #cred{}} |
                  {'error', term()}.
auth(Id, Pass) when is_binary(Id), is_binary(Pass) ->
    case lookup(Id) of
        no_such_cred ->
            {error, invalid_credentials};
        Cred ->
            case pass(Cred) of
                Pass ->
                    {authorized, Cred};
                _WrongPass ->
                    {error, {incorrect_pass, Pass}}
            end
    end.



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
          full_api -> {<<"full_api">>, <<"1">>};
          any_channel -> {<<"channel">>, <<"any">>};
          {channel, Id} -> {<<"channel">>, logplex_channel:id_to_binary(Id)}
      end
      || Perm <- ordsets:to_list(Perms) ].

-spec reportable_perms() -> ordsets:ordset(reportable_perm()).
reportable_perms() ->
    ordsets:from_list([any_channel, full_api]).

maybe_report_operation(Id, Perms, Op) ->
    case ordsets:is_disjoint(reportable_perms(), Perms) of
        true -> ok;
        false ->
            ?WARN("at=reportable_perms op=~p cred_id=~p perms=~p",
                  [Op, Id, ordsets:to_list(Perms)])
    end.
