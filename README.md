# Setup

    $ git submodule update --init
    $ make

# Erlang version

Tested on Erlang R13B04

# EUnit tests
prereq: start a local redis server on the default port 6379

    $ bin/test

# Run

## Development

    $ bin/console

## Production
*note: assumes Ubuntu/Upstart for start/stop commands

    $ [start|stop|restart] logplex
    $ bin/connect ('ctrl-g q' to exit)

# REST API

## Healthcheck

    GET /healthcheck

## Channels

    POST /channels

JSON Params:

* name: channel name (required)
* app\_id: Heroku app\_id (required)
* addon: Heroku addon \[basic|expanded|advanced\] (required)

Resp:

* status: 201
* body: channel\_id

<br/>

    POST /channels/<channel_id>/token

JSON Params:

* name: token name that will appear as the log msg source (required)

Resp:

* status: 201
* body token\_id

<br/>

    POST /channels/<channel_id>/addon

JSON Params:

* addon: Heroku addon to upgrade/downgrade to [basic|expanded|advanced] (required)

Resp:

* status: 200
* body: OK

<br/>

    GET /channels/<channel_id>/info

Resp:

* status: 200
* body: json info object

    DELETE /channels/<channel_id>

Resp:

* status: 200
* body: OK

## Sessions

    POST /sessions

JSON Params:

* channel\_id (required)
* tail (optional)
* num (optional)
* source (optional)
* ps (optional)

Resp:

* status: 201
* body: session\_id

<br/>

    GET /sessions/<session_id>

Resp:

* status: 200
* body: log data

## Drains

    POST /channels/<channel_id>/drains

JSON Params:

* host (required)
* port (optional)

Resp:

* status: 201

<br/>

    GET /channels/<channel_id>/drains

Resp:

* status: 200
* body: drain list

<br/>

    DELETE /channels/<channel_id>/drains

Querystring Params:

* host (required)
* port (required)

Resp:

* status: 200

## Environment Variables

* LOGPLEX\_COOKIE - Erlang cookie, important for multiple node grid (not required)
* LOGPLEX\_CONFIG\_REDIS\_URL - config data redis url (default: redis://127.0.0.1:6379)
* HTTP_PORT - http port (default: 80)
* LOGPLEX\_AUTH\_KEY - api http header 'Authorization' == LOGPLEX\_AUTH\_KEY (required)
* LOCAL\_IP - ip to register with other logplex nodes (default: 127.0.0.1)
* LOGPLEX\_WORKERS - number of workers pulling from queue (default: 10)
* LOGPLEX\_DRAIN\_WRITERS - number of writers pulling from buffer and writing to drains (default: 100)
* LOGPLEX\_REDIS\_WRITERS - number of writers pulling from buffer and writing to redis (default: 100)
* LOGPLEX\_READERS - number of readers pulling from read queue (default: 100)
* LOGPLEX\_QUEUE\_LENGTH - max length of read queue (default: 2000)
* LOGPLEX\_DRAIN\_BUFFER\_LENGTH - max length of drain write buffer (default: 2000)
* LOGPLEX\_REDIS\_BUFFER\_LENGTH - max length of redis write buffer (default: 2000)
* LOGPLEX\_READ\_QUEUE\_LENGTH - max length of read queue (default: 2000)

## Sharding

Add new log redis url to config redis shard key:

    sadd redis:shard:urls redis://password@hostname:6379/

## Redis Keys

    ch:336:data (hash)
    drain:57:data (hash)
    tok:t.abcdefghijklmnopqrstuvwxyz:data (hash)
    ch:17:spool (list)
    /sessions/09743165504ecc6c4db21cbb0083c030 (string)
    drain_index (counter)
    channel_index (counter)
    healthcheck (counter)
    node:heroku.com:10.100.0.1 (string)
    redis:shard:urls (set)

# License

Copyright (c) 2010 Jacob Vorreuter <jacob.vorreuter@gmail.com>  

Permission is hereby granted, free of charge, to any person  
obtaining a copy of this software and associated documentation  
files (the "Software"), to deal in the Software without  
restriction, including without limitation the rights to use,  
copy, modify, merge, publish, distribute, sublicense, and/or sell  
copies of the Software, and to permit persons to whom the  
Software is furnished to do so, subject to the following  
conditions:  

The above copyright notice and this permission notice shall be  
included in all copies or substantial portions of the Software.  

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,  
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES  
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND  
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT  
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,  
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING  
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR  
OTHER DEALINGS IN THE SOFTWARE.  
