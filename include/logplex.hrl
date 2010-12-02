-record(msg, {time, source, ps, content}).
-record(channel, {id, name, app_id, addon}).
-record(token, {id, channel_id, name, addon}).
-record(drain, {id, channel_id, host, port}).

-define(UDP_PORT, 9999).

-define(DEFAULT_LOG_HISTORY, 500).
-define(ADVANCED_LOG_HISTORY, 1500).

-define(BASIC_THROUGHPUT, 500).
-define(EXPANDED_THROUGHPUT, 10000).

-define(MAX_SPOOL_POOL_SIZE, 1000).