-include_lib("eunit/include/eunit.hrl").

-record(msg, {time, source, ps, content}).
-record(channel, {id}).
-record(token, {id, channel_id, name, drains=[]}).
-record(drain, {id, channel_id, token, resolved_host, host, port, tcp=true}).
-record(session, {id, body}).
-record(channel_stat, {channel_id :: logplex_channel:id(),
                       key :: atom()}).
-record(drain_stat, {drain_id :: logplex_drain:id(),
                     channel_id :: logplex_channel:id(),
                     key :: atom()}).

-define(HTTP_PORT, 8001).
-define(TCP_PORT, 6001).
-define(UDP_PORT, 9999).

-define(MAX_DRAINS, 5).
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
