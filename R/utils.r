#' Python Methods for JSON Serialization
#' 
#' Add methods for JSON serialization of Python classes.
#' 
#' @param pyenv The \code{PythonEnv} object.
#' @param method.def A file containing a Python function definition.
#'   See 'details' for more information.
#' @return (Invisibly) the \code{PythonEnv} object.
#'
#' @details Support for JSON serialization of custom classes is achieved
#'   by defining new functions in Python with a special decorator to 
#'   register the function with \code{pysockr}. The function must take a 
#'   single argument and return a JSON string. A template for supporting 
#'   new classes is provided.  \code{pysockr} also provides support for 
#'   \code{pandas.core.frame.DataFrame}, \code{pandas.core.series.Series},
#'   and \code{numpy.ndarray objects} which can be added via 
#'   \code{use_pandas} and \code{use_numpy}.
#'
#' @examples
#' \dontrun{
#' # View the template
#' file.show(system.file("py-src/_use_template.py", package = "pysockr"))
#' 
#' # view the numpy methods
#' file.show(system.file("py-src/use_numpy.py", package = "pysockr"))
#' # add numpy support to a PythonEnv object
#' use_numpy(pyenv)
#' 
#' # view the pandas methods
#' file.show(system.file("py-src/use_pandas.py", package = "pysockr"))
#' # add support to a PythonEnv object
#' use_pandas(pyenv)
#' }
#' 
#' @export
use_class = function(pyenv, method.def) {
  pyenv$exec(file = method.def)
  invisible(pyenv)
}

#' @describeIn use_class Add method for pandas DataFrames and Series.
#' @export
use_pandas = function(pyenv) {
  pyenv$exec("import pandas")
  pandas.def = system.file("py-src/use_pandas.py", package = "pysockr")
  use_class(pyenv, pandas.def)
}

#' @describeIn use_class Add method for numpy ndarrays.
#' @export
use_numpy = function(pyenv) {
  pyenv$exec("import numpy")
  numpy.def = system.file("py-src/use_numpy.py", package = "pysockr")
  use_class(pyenv, numpy.def)
}
