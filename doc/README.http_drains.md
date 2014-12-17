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
    Content-Length: 730
    
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
    70 <174>1 2012-07-22T00:06:26+00:00 host erlang console - Hi from erlang
        
    
### HTTP Headers

* Logplex-Msg-Count: The number of messages encoded in the body of this request. 10 in the example above. You can use this field as a sanity check to detect if you have not parsed the body correctly.
* Logplex-Frame-Id: The unique identifier for this request. If this request is retried for some reason (non-2xx response code, network connection failure, etc.) this identifier will allow you to spot duplicate requests.
* Logplex-Drain-Token: This is the unique identifier for the logplex drain. It will be the same identifier you see if you run `heroku drains -x` for your app.
* User-Agent: This describes the version of logplex, and is changed from time to time. 
* Content-Type: This field describes the encoding of the body for the request. The only current version of this is "application/logplex-1" which is described below.

## Logplex Frame Format

The body of a request from a Logplex HTTP drain is a series of [RFC5424](https://tools.ietf.org/html/rfc5424) messages. The framing format used is *byte counted*.

So a request body for `application/logplex-1` should be as follows:

    <NumberOfBytes/ASCII encoded integer><Space character><RFC5424 message:NumberOfBytes long>

repeated `logplex-msg-count` times. However, `application/logplex-` does not follow RFC5424 exactly. It does not include `STRUCTUREDDATA` and does not put a `NILVALUE` in it's place.
