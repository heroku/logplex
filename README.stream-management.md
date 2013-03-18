# Stream management #

This is a high-level view of how logs are routed through logplex.
 A general production set up might look as follows:

![general architecture](./logplex.png)

All production applications can log to any of the logplex nodes,
although it might be preferable to use proxies in between to handle
things more efficiently.

Logplex on a group of redis database. A Master database for
configuration: How many shards there are, what drains exist (to be
explained further in the text) and so on. The redis shards are
holding references to latest log messages received, in order to build a
short backlog when necessary.

## Where requests come from ##

The typical application using logplex will pipe syslog message from a
local syslog over TCP, or from an applicaiton such as [log
shuttle](https://github.com/ryandotsmith/log-shuttle), which will send
them over HTTP. It is recommended to go with HTTP if possible, as it
allows a better control authentication (over HTTP), SSL, and so on,
whereas the TCP syslog messages will have to be seen as any other raw
TCP stream.

The request will then be routed to a logplex endpoint which, like all
other endpoints, can listen to either kind of streams:

![logplex message](./logplex-trans.png)

Based on an application token, the logplex router is able to figure out
which request belongs to what application, and regroup them accordingly.

In the image above, the message is a local syslog one sent over HTTP.
It's routed through the TCP API, which redistribute it to a drain and/or
a tail, on top of sending it to a given shard, which depends on the
application (and token) it contains.

The drains and tails will then forward the logs to clients.

## Where requests go ##

There are two ways to consume requests: drains and tails.

### Tails ###

Tails are analogous to running `tail -f` on a log on a local machine,
except they're able to fetch logs globally all at once.

A user can start a drain by contacting the logplex HTTP API with the
correct credentials. It doesn't matter which instance is used. The
logplex router will automatically spawn *collectors* on each of the
logplex nodes for that tail and specific application.

Each of these collectors will report to a *tail process* on the logplex
node that was contacted. That node uses the backlog accumulated in redis
and the data forwarded by all collectors, and sends it back to the
client:

![logplex tail](./logplex-tail.png)

When the tail gets closed, the syslog logs stop being streamed.

More details on the drains can be found in the
[logplex API](./README.logplex_api.md)

### Drains ###

A drain works differently from a tail. While a tail is dynamic and only
lasts as long as its connection lasts, a drain is a more permanent
thing.

The user can register a drain as an endpoint where they want logplex to
forwards log to. This can be a web server that stores accumulated logs
inside a given database, on disk files, or does anything with it,
really.

Once the drain is registered through the HTTP API, the configuration is
stored in the redis configuration database and *drain processes* are
created on all nodes.

The main difference with a tail is then that instead of being merged in
a single stream to be sent over a connection, drains will call the
endpoint registered and push data there directly:

![logplex drain](./logplex-drain.png)

There is also no backlog required to be read from redis for the logplex
drain to work.

More details on the drains can be found in the
[logplex API](./README.logplex_api.md) and the
[http drains doc](https://github.com/heroku/logplex/blob/master/doc/README.http_drains.md)


