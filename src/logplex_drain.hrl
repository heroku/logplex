%%-record(drain, {id, channel_id, token, resolved_host, host, port, tcp=true}).
-record(drain, {id :: logplex_drain:id() | '_',
                channel_id :: logplex_channel:id() | '_' | '$1',
                token :: logplex_drain:token() | '_',
                type = tcpsyslog :: logplex_drain:type() | '_',
                uri :: #ex_uri{} | '_'}).
