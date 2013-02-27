# Build

    $ ./rebar --config public.rebar.config get-deps compile

# Test

Given an empty local redis (v2.6ish):

    $ INSTANCE_NAME=`hostname` \
      LOGPLEX_CONFIG_REDIS_URL="redis://localhost:6379" \
      LOCAL_IP="127.0.0.1" \
      LOGPLEX_COOKIE=123 \
      rebar skip_deps=true ct

Runs the common test suite for logplex.

# Develop

run

    $ INSTANCE_NAME=`hostname` \
      LOGPLEX_CONFIG_REDIS_URL="redis://localhost:6379" \
      LOCAL_IP="127.0.0.1" \
      LOGPLEX_COOKIE=123 \
      LOGPLEX_AUTH_KEY=123 \
      erl -name logplex@`hostname` -pa ebin -env ERL_LIBS deps -s logplex_app -setcookie ${LOGPLEX_COOKIE}

create creds    

    1> logplex_cred:store(logplex_cred:grant('full_api', logplex_cred:grant('any_channel', logplex_cred:rename(<<"Local-Test">>, logplex_cred:new(<<"local">>, <<"password">>))))).
    ok

hit healthcheck

    $ curl http://local:password@localhost:8001/healthcheck
    OK

create a channel

    $ curl -d '{"tokens": ["app"]}' http://local:password@localhost:8001/channels
    {"channel_id":1,"tokens":{"app":"t.feff49f1-4d55-4c9e-aee1-2d2b10e69b42"}}

post a log msg

    $ curl -v \
    -H "Content-Type: application/logplex-1" \
    -H "Content-Length: 119" \
    -d "115 <134>1 2012-12-10T03:00:48Z+00:00 erlang t.feff49f1-4d55-4c9e-aee1-2d2b10e69b42 console.1 - Logsplat test message 1" \
    http://local:password@localhost:8601/logs

create a log session

    $ curl -d '{"channel_id": "1"}' http://local:password@localhost:8001/v2/sessions
    {"url":"/sessions/9d53bf70-7964-4429-a589-aaa4df86fead"}

fetch logs for session

    $ curl http://local:password@localhost:8001/sessions/9d53bf70-7964-4429-a589-aaa4df86fead?srv=1
    2012-12-10T03:00:48Z+00:00 app[console.1]: test message 1

