#' Socket-based Interface for Python Processes
#'
#' A simple socket-based interface to Python. Provides a basic 
#' Python server script and R6 class for interacting with a
#' Python process.

#' @name pysockr-package
#' @aliases pysockr
#' @docType package
NULL

#' Python Environment
#' 
#' The Python Environment Class. Provides an interface to a Python process.
#' 
#' 
#' @section Usage:
#' \preformatted{py = PythonEnv$new(port, path)
#'
#' py$start
#' py$running
#' py$exec(..., file = NULL)
#' py$set(...)
#' py$get(varname)
#' py$stop
#' py$kill
#' 
#' print(py)
#' }
#'
#' @section Arguments:
#' \code{port} The port to use for communication with Python.
#' 
#' \code{path} The path to the Python executable.
#' 
#' \code{varname} The name of a Python variable to bring into R.
#' 
#' \code{...} Commands to run or named variables to set in the Python process.
#'
#' \code{file} File containing Python code to execute.
#' @section Methods:
#' \code{$new} Initialize a Python interface. The Python process is not 
#'   started automatically.
#'   
#' \code{$start} Start the Python process. The Python process runs 
#'   asynchronously.
#'
#' \code{$running} Check if the Python process is running.
#' 
#' \code{$port} Set or get the port of the Python interface. The port 
#'   cannot be changed when the Python process is running.
#'
#' \code{$pid} Get the Process ID of the Python interface. Useful for
#'   diagnosing problems.
#'
#' \code{$stop} Stop the Python process by sending a request to the 
#'   Python process.
#' 
#' \code{$kill} Forcibly terminate the Python process. Useful when
#'   \code{$stop} fails or hangs.
#'
#' \code{$set(...)} Set variables in the Python process. R variables
#'   are encoded into JSON format, sent to the Python process as text,
#'   and decoded into variables on the Python side.
#'
#' \code{$get(varname)} Get a variable from the Python process. Python
#'   variables are encoded into JSON format, sent to R as text,
#'   and decoded into variables on the R side.
#'   
#' \code{$exec(...)} Execute the specified Python 
#'   commands and invisibly return printed Python output (if any).
#'   Alternatively, a \code{file} containing Python code can be supplied.
#' 
#' \code{print(py)} Show some information about the
#' Python process on the screen, whether it is running and its process id, 
#' etc.
#'
#' @importFrom R6 R6Class
#' @name PythonEnv
#' @examples
#' pypath = Sys.which('python') 
#' if(nchar(pypath) > 0) {
#'   py = PythonEnv$new(path = pypath, port = 6011, host = "127.0.0.1")
#'   py$start
#'   py$running
#'   py$set(a = 5)
#'   py$get('a')
#'   py$stop
#' } else {
#'   message("No Python environment available")
#' }
NULL

#' @export
PythonEnv = R6::R6Class("PythonEnv", cloneable = FALSE,
  private = list(
    currentpid = NULL,
    currentport = NULL,
    currentpath = NULL,
    currenthost = NULL,
    version = NULL,
    isrunning = NULL,
    socket = NULL
  ),
  public = list(
    print = function(...) {
      cat("Python ", private$version, "\n", sep = "")
      cat("(", self$path, ")\n", sep = "")
      if (self$running)
        cat("Listening on ", self$host, ":", self$port, 
          " (Process ID: ", self$pid,  ")\n", sep = "")
      else
        cat("Process not running")
    },
    
    initialize = function(path, port, host = "localhost") {
      if (is.na(as.integer(port)))
        stop("Invalid port specified", call. = FALSE)
      if (!file.exists(path))
        stop("Invalid path specified", call. = FALSE)
      private$currentpath = path
      private$currenthost = host
      private$currentport = as.integer(port)
      private$version = "(version unknown)"
      if (port < 1024L)
        warning("Using port numbers below 1024 is not recommended")
      private$isrunning = FALSE
      # socket helper
      private$socket = function() {
        socketConnection(host = self$host, port = self$port, 
          open = 'r+', blocking = TRUE, server = FALSE, 
          encoding = "UTF-8")
      }
      invisible(self)
    },
    
    finalize = function() {
      if (self$running)
        self$stop
    },
    
    exec = function(..., file = NULL) {
      if (!self$running)
        stop("The Python process is not running", call. = FALSE)
      if(!is.null(file))
        code = normalizePath(file, mustWork = TRUE)
      else
        code = paste(list(...), collapse = "\n")
      s = private$socket()
      on.exit(close(s))
      writeLines(code, s)
      res = readLines(s, warn = FALSE)
      if(length(res) > 0)
        if(res[1] == "pysockr-error")
          stop("Python returned an error\n",
            paste(tail(res, -1), collapse = "\n"), call. = FALSE)
      invisible(res)
    },
    
    get = function(varname) {
      if (!self$running)
        stop("The Python process is not running", call. = FALSE)
      msg = sprintf("print(json.dumps(%s))", varname)
      jsonlite::fromJSON(self$exec(msg))
    },
    
    set = function(...) {
      if (!self$running)
        stop("The Python process is not running", call. = FALSE)
      dots = list(...)
      jdots = jsonlite::toJSON(dots)
      s = private$socket()
      on.exit(close(s))
      writeLines(sprintf("locals().update(%s)", jdots), s)
    }
  ),
  
  active = list(
    
    running = function() {
      private$isrunning
    },
    
    port = function(value) {
      if (missing(value))
        return(private$currentport)
      if (self$running)
        stop("Cannot update the port while the Python process is running",
          call. = FALSE)
      private$currentport = as.integer(value)
    },

    host = function(value) {
      if (missing(value))
        return(private$currenthost)
      if (self$running)
        stop("Cannot update the host while the Python process is running",
             call. = FALSE)
      private$currenthost = value
    },
    
    path = function(value) {
      if (missing(value))
        return(private$currentpath)
      if (self$running)
        stop("Cannot update the path while the Python process is running",
             call. = FALSE)
      private$currentpath = value
    },

    pid = function() {
      private$currentpid
    },
    
    start = function() {
      if (self$running) {
        message("The Python process is already running")
      } else {
        fpath = system.file("py-src/server.py", package = "pysockr") 
        system2(private$currentpath, wait = FALSE, 
                args = c(shQuote(fpath), self$port, self$host))
        # check if it's running
        s = private$socket()
        on.exit(close(s))
        writeLines('print("RUNNING")', s)
        res = readLines(s, warn = FALSE)
        if (length(res) < 1L)
          stop("Connection to Python could not be established", call. = FALSE)
        else
          private$isrunning = TRUE
        # get pid
        private$currentpid = as.integer(self$exec('print(os.getpid())'))
        # get version
        private$version = self$exec('print(sys.version)')
      }
      self
    },
    
    stop = function() {
      if (!self$running) {
        message("The Python process is not running")
      } else {
        s = private$socket()
        on.exit(close(s))
        writeLines("quit", s)
        res = readLines(s, warn = FALSE)
        if (res != "QUIT")
          stop("The Python process could not be shut down normally")
        private$isrunning = FALSE
        message("Python process ", self$pid, " was terminated", sep = "")
      }
      self
    },
    kill = function() {
      tools::pskill(self$pid)
      message("Python process ", self$pid, " was terminated", sep = "")
      private$isrunning = FALSE
      self
    }
  )
)
