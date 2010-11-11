#!/usr/bin/env escript
%% -*- erlang -*-

main([Token]) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    gen_udp:send(Socket, "localhost", 9999, iolist_to_binary([<<"<40>1 2010-11-10T17:16:33-08:00 domU-12-31-39-13-74-02 ">>, Token, <<" web.1 - - Unidling">>])),
    gen_udp:send(Socket, "localhost", 9999, iolist_to_binary([<<"<13>1 2010-11-10T17:16:36-08:00 railgun.41925@jake.herokudev.com ">>, Token, <<" web.1 - - >> Thin web server (v1.2.6 codename Crazy Delicious)">>])),
    gen_udp:send(Socket, "localhost", 9999, iolist_to_binary([<<"<173>1 2010-11-10T17:16:37-08:00 ip-10-202-195-70 ">>, Token, <<" nginx - - GET / HTTP/1.1 | 208.66.27.62 | 245 | http | 200">>])),
    ok.

% curl -d name=app123 http://localhost:8008/channels
% curl -d name=heroku http://localhost:8008/channels/666/token
% escript bin/syslog.escript XXX
% curl -d channel_id=666&tail=true http://localhost:8008/sessions
% curl http://localhost:8008/sessions/XXX

% <40>1 2010-11-10T17:16:33-08:00 domU-12-31-39-13-74-02 t.297260f6e3557760da23bc7c9a913555 web.1 - - Unidling
% 
% <40>1 2010-11-10T17:16:33-08:00 domU-12-31-39-13-74-02 t.297260f6e3557760da23bc7c9a913555 web.1 - - State changed from created to starting
% 
% <13>1 2010-11-10T17:16:36-08:00 railgun.41925@jake.herokudev.com t.c1024ede4773008e5798c77e503f0d6e web.1 - - >> Thin web server (v1.2.6 codename Crazy Delicious)
% 
% <13>1 2010-11-10T17:16:36-08:00 railgun.41925@jake.herokudev.com t.c1024ede4773008e5798c77e503f0d6e web.1 - - >> Maximum connections set to 1024
% 
% <13>1 2010-11-10T17:16:36-08:00 railgun.41925@jake.herokudev.com t.c1024ede4773008e5798c77e503f0d6e web.1 - - >> Listening on 0.0.0.0:23384, CTRL+C to stop
% 
% <40>1 2010-11-10T17:16:37-08:00 domU-12-31-39-13-74-02 t.297260f6e3557760da23bc7c9a913555 web.1 - - State changed from starting to up
% 
% <13>1 2010-11-10T17:16:37-08:00 railgun.41925@jake.herokudev.com t.c1024ede4773008e5798c77e503f0d6e web.1 - - beginning request 0
% 
% <13>1 2010-11-10T17:16:37-08:00 railgun.41925@jake.herokudev.com t.c1024ede4773008e5798c77e503f0d6e web.1 - - finished request 0
% 
% <173>1 2010-11-10T17:16:37-08:00 ip-10-202-195-70 t.297260f6e3557760da23bc7c9a913555 nginx - - GET / HTTP/1.1 | 208.66.27.62 | 245 | http | 200
% 
% <158>1 2010-11-10T17:16:37-08:00 ip-10-244-155-95 t.297260f6e3557760da23bc7c9a913555 router - - GET pure-leaf-84.jake.herokudev.com/ dyno=web.1 queue=0 wait=3766ms service=14ms bytes=215
% 
% <13>1 2010-11-10T17:16:37-08:00 railgun.41925@jake.herokudev.com t.c1024ede4773008e5798c77e503f0d6e web.1 - - beginning request 1
% 
% <13>1 2010-11-10T17:16:37-08:00 railgun.41925@jake.herokudev.com t.c1024ede4773008e5798c77e503f0d6e web.1 - - finished request 1
% 
% <158>1 2010-11-10T17:16:37-08:00 ip-10-244-155-95 t.297260f6e3557760da23bc7c9a913555 router - - GET pure-leaf-84.jake.herokudev.com/favicon.ico dyno=web.1 queue=0 wait=0ms service=1ms bytes=215
% 
% <173>1 2010-11-10T17:16:37-08:00 ip-10-202-195-70 t.297260f6e3557760da23bc7c9a913555 nginx - - GET /favicon.ico HTTP/1.1 | 208.66.27.62 | 245 | http | 200