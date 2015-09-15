default_buf_size() -> logplex_app:config(tcp_drain_buffer_size, 1024).

-record(state, {drain_id :: logplex_drain:id(),
                drain_tok :: logplex_drain:token(),
                channel_id :: logplex_channel:id(),
                host :: string() | inet:ip_address() | binary(),
                port :: inet:port_number(),
                sock = undefined :: 'undefined' | ssl:sslsocket(),
                %% Buffer for messages while disconnected
                buf = logplex_msg_buffer:new(default_buf_size()) :: logplex_msg_buffer:buf(),
                %% Last time we connected or successfully sent data
                last_good_time :: 'undefined' | erlang:timestamp(),
                %% TCP failures since last_good_time
                failures = 0 :: non_neg_integer(),
                %% Reconnect timer reference
                reconnect_tref = undefined :: 'undefined' | reference(),
                %% Send timer reference
                send_tref = undefined :: 'undefined' | reference(),
                %% SSL Send connection monitor reference
                send_mref = undefined :: 'undefined' | reference(),
                %% Close timer reference
                close_tref :: reference() | 'undefined',
                %% Time of last successful connection
                connect_time :: 'undefined' | erlang:timestamp()
               }).

