-include_lib("eunit/include/eunit.hrl").

-record(msg, {time, source, ps, content}).
-record(token, {id :: logplex_token:id() | '_',
                channel_id :: logplex_channel:id() | '$1',
                name :: logplex_token:name() | '_'
               }).
-record(channel_stat, {channel_id :: logplex_channel:id(),
                       key :: atom()}).
-record(drain_stat, {drain_id :: logplex_drain:id(),
                     drain_type :: logplex_drain:type(),
                     channel_id :: logplex_channel:id(),
                     key :: atom()}).
-record(logplex_stat, {module :: atom(),
                       key :: any()}).
-record(queue_stat, {redis_url :: iolist(),
                     key :: list()}).
-define(LOG_HISTORY, <<"1500">>).
-define(MAX_SPOOL_POOL_SIZE, 1000).
