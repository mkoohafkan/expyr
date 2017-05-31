#' Start Python
#' 
#' Start the Python Server.
#' 
#' @param python.port The port to use for communication.
#' @param python.path The path to the Python executable.
#' 
#' @examples
#' \dontrun{ 
#' py_start(6011, "C:/python36/python.exe")
#' py_start(6011, "C:/python27/python.exe")
#' }
#' 
#' @export
py_start = function(python.port, python.path) {
  if (missing(python.path))
    python.path = getOption("pysockr.python.path")
  else
    options(pysockr.python.path = python.path)
  if (is.null(python.path))
    stop('Python path not specified')
  
  if (missing(python.port))
    python.port = getOption("pysockr.python.port")
  else
    options(pysockr.python.port = as.integer(python.port))
  if (is.null(python.port))
    stop('Python port not specified')
  if(python.port < 1024L)
    warning("Using port numbers below 1024 is not recommended")
  
  fpath = system.file("py-src/server.py", package = "pysockr") 
  cmd = sprintf("%s %s %s", shQuote(python.path), 
    shQuote(fpath), as.integer(python.port))
  system(cmd, wait = FALSE)
  # check if it's working
  s = py_socket()
  on.exit(close(s))
  writeLines('print("RUNNING")', s)
  res = readLines(s)
  if (length(res) < 1L)
    stop("Python connection could not be established")
  assign("running", TRUE, envir = pythonenv)
  invisible(TRUE)
}


#' Python Socket
#' 
#' Create a socket connection to the Python server.
#' 
py_socket = function(){
  socketConnection(port = getOption("pysockr.python.port"), 
    open = 'r+', blocking = TRUE, server = FALSE, 
    encoding = "UTF-8")
}

#' Check Python
#' 
#' Check that the Python server is running.
#' 
py_check = function() {
}


#' Execute Python Code
#' 
#' Execute Python code and return the result.
#' 
#' @param ... Python code to execute.
#' @return A string containing the result, if any.
#' 
#' @examples
#' \dontrun{ 
#' py_exec('a = 5')
#' py_exec('print(a)')
#' py_exec('print(a + 5)')
#' }
#' 
#' @export
py_exec = function(...) {
  if (!get("running", envir = pythonenv))
    stop("The Python server is not running")
  code = paste(list(...), collapse = ";")
  s = py_socket()
  on.exit(close(s))
  writeLines(code, s)
  readLines(s, warn = FALSE)
}


#' Get Python Variable
#' 
#' Retrieve the value of a Python variable.
#' 
#' @param x The Python variable name.
#' @return The value of the Python variable.
#'
#' @examples
#' \dontrun{ 
#' py_exec('a = 5')
#' py_get('a')
#' }
#' 
#' @importFrom jsonlite fromJSON
#' @export
py_get = function(x) {
  if (!get("running", envir = pythonenv))
    stop("The Python server is not running")
  s = py_socket()
  on.exit(close(s))
  writeLines(sprintf("print(json.dumps(%s))", x), s)
  fromJSON(readLines(s, warn = FALSE))
}


#' Set Python Variable
#' 
#' Set a Python variable.
#' 
#' @param ... A sequence of variables to set. Can be either
#'   existing variables in the R environment or arbitrary 
#'   named variables to create.
#'
#' @examples
#' \dontrun{ 
#' py_set(a = 5)
#' py_set(b = 7, c = "foo")
#' py_set(d = 1:5)
#' py_set(d = list(1, 2, 3, 4, 5))
#' }
#'  
#' @importFrom jsonlite toJSON
#' @export
py_set = function(...) {
  if (!get("running", envir = pythonenv))
    stop("The Python server is not running")
  dots = list(...)
  jdots = toJSON(dots)
  s = py_socket()
  on.exit(close(s))
  writeLines(sprintf("locals().update(%s)", jdots), s)
}


#' Quit Python
#' 
#' Shut down the Python server.
#' 
#' @examples
#' \dontrun{ 
#' py_quit()
#' }
#' 
#' @export
py_quit = function() {
  if (!get("running", envir = pythonenv))
    stop("The Python server is not running")
  s = py_socket()
  on.exit(close(s))
  writeLines("quit", s)
  res = readLines(s, warn = FALSE)
  if (res != "QUIT")
    stop("The Python server could not be shut down")
  assign("running", FALSE, envir = pythonenv)
  invisible(TRUE)
}
