import socket
import sys
import contextlib
import traceback
import json
try:
  import io
except ImportError:
  import StringIO as io

script, port = sys.argv


@contextlib.contextmanager
def stdoutIO(stdout = None):
    old = sys.stdout
    if stdout is None:
        stdout = io.StringIO()
    sys.stdout = stdout
    yield stdout
    sys.stdout = old

host = 'localhost'
port = int(port)

with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
  # initialize server
  s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  s.bind((host,port))
  s.listen(1)
  print("server is listening")
  while True:
    # accept a connection
    try:
      conn, _ = s.accept()
      print("new connection")
      # read incoming command
      data = conn.recv(1024).decode(encoding = 'UTF-8')
      # quit if requested
      if "quit" in data:
        result = "QUIT"
        conn.sendall(result.encode(encoding = 'UTF-8'))
        break
      print ("from connected  user: " + str(data))
      # execute command
      with stdoutIO() as execout:
        try:
          exec(str(data))
        except:
          print(traceback.format_exc())
      # return results
      result = execout.getvalue()
      print ("sending: " + str(result))
      conn.sendall(str(result).encode(encoding = 'UTF-8'))
    finally:
      # close the conenction
      print("closing connection")
      conn.shutdown(2)
      conn.close()

print("shutting down server")
