# use a decorator to overload the PYSOCKR_JSON_DUMPS function
@PYSOCKR_JSON_DUMPS.register( <your class> )
def <your function> (obj):
  # ... code ... #
  return( <JSON string> )
