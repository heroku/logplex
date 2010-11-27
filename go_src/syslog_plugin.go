package main

import (
	"os";
	"bufio";
	"net";
	"bytes";
	"fmt";
)

func main() {
  c, err := net.Dial("unix", "", "/tmp/redis.sock");
  if err != nil { os.Exit(1) }
  reader := bufio.NewReader(os.Stdin);
  for true {
    msg, err := reader.ReadBytes(byte('\n'));
    if err != nil { os.Exit(1) }
    l := len(msg)
    cmdbuf := bytes.NewBufferString(fmt.Sprintf("*3\r\n$5\r\nRPUSH\r\n$4\r\nlogs\r\n$%d\r\n%s\r\n", l-1, msg[:l-1]))
    _, err = c.Write(cmdbuf.Bytes());
    if err != nil { os.Stdout.WriteString(err.String()) }
  }
}