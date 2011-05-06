-include_lib("eunit/include/eunit.hrl").

-record(msg, {time, source, ps, content}).
-record(channel, {id, name, app_id, addon}).
-record(token, {id, channel_id, name, app_id, addon}).
-record(drain, {id, channel_id, resolved_host, host, port}).
-record(session, {id, body}).

-define(HTTP_PORT, 8001).
-define(TCP_PORT, 9998).
-define(UDP_PORT, 9999).

-define(DEFAULT_LOG_HISTORY, <<"500">>).
-define(ADVANCED_LOG_HISTORY, <<"1500">>).

-define(BASIC_THROUGHPUT, 500).
-define(EXPANDED_THROUGHPUT, 10000).

-define(MAX_SPOOL_POOL_SIZE, 1000).

-define(DEFAULT_LOGPLEX_QUEUE_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_DRAIN_BUFFER_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_REDIS_BUFFER_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_READ_QUEUE_LENGTH, 2000).
-define(DEFAULT_LOGPLEX_WORKERS, 10).
-define(DEFAULT_LOGPLEX_DRAIN_WRITERS, 10).
-define(DEFAULT_LOGPLEX_REDIS_WRITERS, 10).
-define(DEFAULT_LOGPLEX_READERS, 10).
