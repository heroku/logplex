FROM voidlock/erlang:18.1.3

ENV ERL_CRASH_DUMP=/dev/null \
    LOGPLEX_CONFIG_REDIS_URL="redis://db:6379/" \
    LOGPLEX_SHARD_URLS="redis://db:6379/#frag1" \
    LOGPLEX_REDGRID_REDIS_URL="redis://db:6379/" \
    LOGPLEX_STATS_REDIS_URL="redis://db:6379/" \
    LOGPLEX_COOKIE="123" \
    LOGPLEX_AUTH_KEY="secret"

EXPOSE 8001 8601 6001 4369 49000

VOLUME /root/.cache

RUN curl --silent -L --fail --max-time 10 -o /usr/local/bin/rebar3 https://github.com/erlang/rebar3/releases/download/3.5.0/rebar3 && chmod +x /usr/local/bin/rebar3

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

CMD ["./bin/logplex"]
