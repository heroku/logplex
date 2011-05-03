%% Common definitions for nsync data manupulator modules:
%%   - logplex_token
%%   - logplex_channel
%%   - logplex_drain
%%   - logplex_session

-define(RAW_TID, raw_data).
-define(DEFAULT_BINARY, <<"undefined">>).
-define(DEFAULT_INTEGER, 0).
-define(CHANNEL_PREFIX, "ch:").
-define(DATA_SUFFIX, ":data").
-define(TOKEN_PREFIX, "tok:").
-define(DRAIN_PREFIX, "drain:").
