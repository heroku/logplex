var dgram = require('dgram');
var sys = require('sys');

var token;
var host;
var port;
var rate = 100;

for (i = 2; i < process.argv.length; i++) {
  switch (process.argv[i]) {
    case "--token":
      token = process.argv[++i];
      break;
    case "--host":
      host = process.argv[++i];
      break;
    case "--port":
      port = parseInt(process.argv[++i]);
      break;
    case "--rate":
      rate = parseInt(process.argv[++i]);
      break;
  }
}

var message = new Buffer("<40>1 2010-11-10T17:16:33-08:00 domU-12-31-39-13-74-02 " + token + " web.1 - - State changed from created to starting");

var client = dgram.createSocket("udp4");

var tick = function(counter) {
  client.send(message, 0, message.length, port, host);
  if (counter == rate) setTimeout(tick, 1, 0);
  else tick(counter+1);
};

setTimeout(tick, 1, 0);

