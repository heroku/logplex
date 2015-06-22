FROM voidlock/erlang:R16B03-1-onbuild

ENV ERL_CRASH_DUMP=/dev/null \
    LOGPLEX_CONFIG_REDIS_URL="redis://db:6379/" \
    LOGPLEX_SHARD_URLS="redis://db:6379/#frag1" \
    LOGPLEX_REDGRID_REDIS_URL="redis://db:6379/" \
    LOGPLEX_STATS_REDIS_URL="redis://db:6379/" \
    LOGPLEX_COOKIE="123" \
    LOGPLEX_AUTH_KEY="secret"

EXPOSE 8001 8601 6001 4369 49000

VOLUME /usr/src/app/deps
VOLUME /usr/src/app/ebin

CMD ["./bin/logplex"]
