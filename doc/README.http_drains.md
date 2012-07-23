# Logplex HTTP Drains

Drains are the machine-oriented ouput abstraction for Logplex. They receive copies of every log message passing through a channel and are responsible for delivering them, as a client, to a user defined URL.

Logplex HTTP drains collect log message into batches and then POST them to the `http://` or `https://` URL configured during drain setup. Every part of the URL is supported, including basic authentication, host and port, path, query string and fragment. The POST itself will be a simple file-upload with a content type of "application/logplex-<FormatVersion>".

## Example Post

This is an example HTTP request that logplex might make if it had ten log messages to deliver:

    POST /logs HTTP/1.1
    Host: example.com
    Content-Type: application/logplex-1
    Logplex-Msg-Count: 10
    Logplex-Frame-Id: 09C557EAFCFB6CF2740EE62F62971098
    Logplex-Drain-Token: d.fc6b856b-3332-4546-93de-7d0ee272c3bd
    User-Agent: Logplex/v49
    Content-Length: 771
    
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 1 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 2 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 3 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 4 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 5 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 6 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 7 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 8 Hi from erlang
    74 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 9 Hi from erlang
    75 <174>1 2012-07-22T00:06:26-00:00 somehost erlang console 10 Hi from erlang
        
    
### HTTP Headers

* Logplex-Msg-Count: The number of messages encoded in the body of this request. 10 in the example above. You can use this field as a sanity check to detect if you have not parsed the body correctly.
* Logplex-Frame-Id: The unique identifier for this request. If this request is retried for some reason (non-2xx response code, network connection failure, etc.) this identifier will allow you to spot duplicate requests.
* Logplex-Drain-Token: This is the unique identifier for the logplex drain. It will be the same identifier you see if you run `heroku drains -x` for your app.
* User-Agent: This describes the version of logplex, and is changed from time to time. 
* Content-Type: This field describes the encoding of the body for the request. The only current version of this is "application/logplex-1" which is described below.

## Logplex Frame Format

The body of a request from a Logplex HTTP drain is a series of [RFC5452](https://tools.ietf.org/html/rfc5424) messages. The framing format used is *byte counted*.

So a request body for `application/logplex-1` should be as follows:

    <NumberOfBytes/ASCII encoded integer><Space character><RFC5452 message:NumberOfBytes long>

repeated `logplex-msg-count` times.
