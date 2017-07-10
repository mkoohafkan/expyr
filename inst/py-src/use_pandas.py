@PYSOCKR_JSON_DUMPS.register(pandas.core.frame.DataFrame)
def PANDAS_RJSON(obj):
  return(obj.to_json())

@PYSOCKR_JSON_DUMPS.register(pandas.core.series.Series)
def PANDAS_RJSON(obj):
  return(obj.to_json())
