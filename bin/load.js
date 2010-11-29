var dgram = require('dgram');
var message = new Buffer("<40>1 2010-11-10T17:16:33-08:00 domU-12-31-39-13-74-02 t.8f5f231c-d915-4c46-8b03-c838533b3c25 web.1 - - State changed from created to starting");
//var message = new Buffer("Nov 29 12:51:31 domU-12-31-39-00-66-52 t.641de56d-098c-441e-b8dc-a3e1789675d6[router]: GET vivid-fire-26.jake.herokudev.com/ dyno=web.1 queue=0 wait=0ms service=1ms bytes=215")
var client = dgram.createSocket("udp4");
while(true) {
	client.send(message, 0, message.length, 9999, "127.0.0.1");
}
client.close();