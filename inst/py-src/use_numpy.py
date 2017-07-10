@PYSOCKR_JSON_DUMPS.register(numpy.ndarray)
def NUMPY_RJSON(obj):
  return(PYSOCKR_JSON_DUMPS(obj.tolist()))
