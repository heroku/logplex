-include_lib("eunit/include/eunit.hrl").

-record(msg, {time, source, ps, content}).
-record(token, {id :: logplex_token:id() | '_',
                channel_id :: logplex_channel:id() | '$1',
                name :: logplex_token:name() | '_'
               }).
-record(channel_stat, {channel_id :: logplex_channel:id(),
                       key :: atom()}).
-record(drain_stat, {drain_id :: logplex_drain:id(),
                     channel_id :: logplex_channel:id(),
                     key :: atom()}).
-record(logplex_stat, {module :: atom(),
                       key :: atom()}).

-define(HTTP_PORT, 8001).
-define(UDP_PORT, 9999).

-define(LOG_HISTORY, <<"1500">>).
-define(MAX_SPOOL_POOL_SIZE, 1000).

-define(DEFAULT_LOGPLEX_QUEUE_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_DRAIN_BUFFER_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_REDIS_BUFFER_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_READ_QUEUE_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_WORKERS, 10).
-define(DEFAULT_LOGPLEX_DRAIN_WRITERS, 10).
-define(DEFAULT_LOGPLEX_REDIS_WRITERS, 10).
-define(DEFAULT_LOGPLEX_READERS, 10).
