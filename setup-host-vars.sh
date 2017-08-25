#!/bin/bash
FQHN=$(hostname -f)
DN=${FQHN#*.*}

export LOGPLEX_HOSTNAME=logplex
echo "The logplex hostname        $LOGPLEX_HOSTNAME"

export LOGPLEX_DOMAIN_NAME=$LOGPLEX_HOSTNAME.$DN
echo "The logplex FQ domain name  $LOGPLEX_DOMAIN_NAME"

export LOGPLEX_INSTANCE_NAME=$LOGPLEX_HOSTNAME@$LOGPLEX_DOMAIN_NAME
echo "The logplex instance name   $LOGPLEX_INSTANCE_NAME"
