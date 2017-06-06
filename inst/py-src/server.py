from __future__ import print_function
import socket
import sys
import os
import contextlib
import traceback
import json
if sys.version_info[0] < 3:
  import StringIO as io
else:
  import io


script, PORT, HOST = sys.argv


@contextlib.contextmanager
def stdoutIO(stdout = None):
    old = sys.stdout
    if stdout is None:
        stdout = io.StringIO()
    sys.stdout = stdout
    yield stdout
    sys.stdout = old

HOST = socket.gethostbyname(str(HOST))
PORT = int(PORT)

with contextlib.closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as SERVER:
  # initialize server
  SERVER.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  SERVER.bind((HOST, PORT))
  SERVER.listen(1)
  print("server is listening")
  while True:
    # accept a connection
    try:
      CONN, _ = SERVER.accept()
      print("new connection")
      # read incoming command
      data = CONN.recv(1024).decode(encoding = 'UTF-8')
      # quit if requested
      if "quit" in data:
        result = "QUIT"
        CONN.sendall(result.encode(encoding = 'UTF-8'))
        break
      print ("from connected  user: " + str(data))
      # execute command
      with stdoutIO() as execout:
        try:
          exec(compile(str(data), "<from R>", "exec"))
        except:
          print("pysockr-error\n" + traceback.format_exc())
      # return results
      result = execout.getvalue()
      print ("sending: " + str(result))
      CONN.sendall(str(result).encode(encoding = 'UTF-8'))
    finally:
      # close the conenction
      print("closing connection")
      CONN.shutdown(2)
      CONN.close()

print("shutting down server")
