# Build

    $ ./rebar --config public.rebar.config get-deps compile

# Run

    $ source keys.sh
    $ bin/logplex

# Testing

Given an empty local redis (v2.6ish):

    $ INSTANCE_NAME=`hostname` LOGPLEX_CONFIG_REDIS_URL="redis://localhost:6379" LOCAL_IP="127.0.0.1" LOGPLEX_COOKIE=123 rebar skip_deps=true ct

Runs the common test suite for logplex.
