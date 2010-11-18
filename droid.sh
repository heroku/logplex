#!/bin/bash

export USER=logplex
export DIR=/opt/logplex
export SERVER_UID=`id -u $USER`
export SERVER_GID=`id -g $USER`

cd $DIR
ruby ./droid.rb > /var/log/droid.log 2>&1
