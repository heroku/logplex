package main

import (
  "os";
  "bufio";
  "net";
  "bytes";
  "fmt";
  "strconv";
  "time";
)

func main() {
  /* this throttle determines when logs should be thrown
     out instead of inserted into redis */
  throttle := time.Seconds()

  /* open unixsocket */
  c, err := net.Dial("unix", "", "/tmp/redis.sock");
  if err != nil {
    os.Stdout.WriteString(err.String())
    os.Exit(1)
  }
  reader := bufio.NewReader(os.Stdin);

  /* loop */
  for ;; {

    /* read from stdin */
    msg, err := reader.ReadBytes(byte('\n'));
    if err != nil {
      os.Stdout.WriteString(err.String())
      os.Exit(1)
    }

    if (time.Seconds() >= throttle) {

      /* format redis command */
      l := len(msg)
      cmdbuf := bytes.NewBufferString(fmt.Sprintf("*3\r\n$5\r\nRPUSH\r\n$4\r\nlogs\r\n$%d\r\n%s\r\n", l-1, msg[:l-1]))

      /* write to socket */
      _, err = c.Write(cmdbuf.Bytes())
      if err != nil {
        os.Stdout.WriteString(err.String())
        os.Exit(1)
      }

      /* read from socket */
      buf := make([]byte, 512)
      _, err = c.Read(buf)
      if err != nil {
        os.Stdout.WriteString(err.String())
        os.Exit(1)
      }
      if (buf[:1][0] == byte(':')) {
        l := len(buf)
        s := ""
        for i := 1; i < l && buf[i] > 47 && buf[i] < 58; i++ {
          s += string(buf[i]);
        }
        code, _ := strconv.Atoi(s)
        if (code > 100) {
          throttle = time.Seconds() + 3
          os.Stdout.WriteString(fmt.Sprintf("over capacity: %d\n", code))
        }
      }
    }
  }
}