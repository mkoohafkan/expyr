from __future__ import print_function
import socket as SOCKET
import sys as SYS
import os as OS
import contextlib as CONTEXTLIB
import traceback as TRACEBACK
import json as JSON
if float('.'.join(str(i) for i in SYS.version_info[0:2])) < 3.4:
  import singledispatch as FUNCTOOLS
else:
  import functools as FUNCTOOLS
if float('.'.join(str(i) for i in SYS.version_info[0:2])) < 3.6:
  import StringIO as IO
else:
  import io as IO

@CONTEXTLIB.contextmanager
def stdoutIO(stdout = None):
  old = SYS.stdout
  if stdout is None:
    stdout = IO.StringIO()
  SYS.stdout = stdout
  yield stdout
  SYS.stdout = old

@FUNCTOOLS.singledispatch
def PYSOCKR_JSON_DUMPS(obj):
  return(JSON.dumps(obj))

@FUNCTOOLS.singledispatch
def PYSOCKR_JSON_LOADS(obj):
  return(JSON.loads(obj))

def pysockr_server(HOST, PORT):
  """Run the pysockr server"""
  HOST = SOCKET.gethostbyname(str(HOST))
  PORT = int(PORT)
  
  with CONTEXTLIB.closing(SOCKET.socket(SOCKET.AF_INET, SOCKET.SOCK_STREAM)) as SERVER:
    # initialize server
    SERVER.setsockopt(SOCKET.SOL_SOCKET, SOCKET.SO_REUSEADDR, 1)
    SERVER.bind((HOST, PORT))
    SERVER.listen(1)
    print("server is listening")
    while True:
      # accept a connection
      try:
        CONN, ADDR = SERVER.accept()
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
            print("pysockr-error\n" + TRACEBACK.format_exc())
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

if __name__ == "__main__":
  script, port, host = SYS.argv
  pysockr_server(host, port)
