FROM voidlock/elixir:1.1

ENV ERL_CRASH_DUMP=/dev/null \
    LOGPLEX_CONFIG_REDIS_URL="redis://db:6379/" \
    LOGPLEX_SHARD_URLS="redis://db:6379/#frag1" \
    LOGPLEX_REDGRID_REDIS_URL="redis://db:6379/" \
    LOGPLEX_STATS_REDIS_URL="redis://db:6379/" \
    LOGPLEX_COOKIE="123" \
    LOGPLEX_AUTH_KEY="secret"

EXPOSE 8001 8601 6001 4369 49000

VOLUME /root/.cache

# Explicitly install these here as `mix deps.get` doesn't have a --force option
# to auto-confirm.
RUN mix local.hex --force
RUN mix local.rebar --force

WORKDIR /usr/src/app
ADD mix.exs /usr/src/app/mix.exs
ADD mix.lock /usr/src/app/mix.lock
RUN mix deps.get
RUN mix compile

ADD . /usr/src/app
RUN ln -s /usr/src/app/test /usr/src/app/ctest  # https://github.com/alco/mix-erlang-tasks/issues/2
RUN mix release
