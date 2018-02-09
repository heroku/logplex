FROM voidlock/erlang:18.1.3

ENV ERL_CRASH_DUMP=/dev/null \
    RELX_REPLACE_OS_VARS=true \
    LOGPLEX_CONFIG_REDIS_URL="redis://db:6379/" \
    LOGPLEX_SHARD_URLS="redis://db:6379/#frag1" \
    LOGPLEX_REDGRID_REDIS_URL="redis://db:6379/" \
    LOGPLEX_STATS_REDIS_URL="redis://db:6379/" \
    LOGPLEX_COOKIE="123" \
    LOGPLEX_AUTH_KEY="secret"

EXPOSE 8001 8601 6001 4369 49000

RUN useradd -d /app logplex
WORKDIR /app

COPY . /app
RUN chown -R logplex:logplex /app

USER logplex

RUN curl --silent -L --fail --max-time 10 -o rebar3 https://github.com/erlang/rebar3/releases/download/3.5.0/rebar3 && chmod +x rebar3
RUN make compile

CMD ["bin/run", "./_build/public/rel/logplex/bin/logplex", "foreground"]
