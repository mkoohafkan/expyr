import sys
import pysockr

script, port, host = sys.argv
pysockr.server(host, port)
