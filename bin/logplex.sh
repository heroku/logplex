#!/bin/sh

export USER=logplex
export DIR=/opt/logplex
export SERVER_UID=`id -u $USER`
export SERVER_GID=`id -g $USER`

cd $DIR
erl +K true +A30 -name logplex_console@`hostname --fqdn` -pa ebin -pa deps/*/ebin -noshell -boot release/logplex-1.0 > /var/log/$USER.log &2>1