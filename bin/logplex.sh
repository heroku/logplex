#!/bin/sh

erl +K true +A30 -name logplex_console@`hostname --fqdn` -pa ebin -pa deps/*/ebin -noshell -detached -boot release/logplex-1.0