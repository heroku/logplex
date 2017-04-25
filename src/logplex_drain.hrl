-include_lib("ex_uri/include/ex_uri.hrl").

-record(drain, {id :: logplex_drain:id() | '_',
                channel_id :: logplex_channel:id() | '_' | '$1',
                token :: logplex_drain:token() | '_',
                type = tcpsyslog :: logplex_drain:type() | '_',
                uri :: #ex_uri{} | '_' | '$2'}).
