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
#' py$start()
#' 
#' py$running
#' py$pid
#' py$path (py$path <-)
#' py$host (py$host <-)
#' py$port (py$port <-)
#' py$timeout (py$timeout <-)
#' 
#' py$exec(..., file = NULL)
#' py$set(...)
#' py$get(varname)
#' 
#' py$stop(force = FALSE)
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
#' 
#' \code{force} If \code{TRUE}, force the Python process to terminate
#'   using a sytem call.
#' 
#' @section Methods:
#' \code{$new()} Initialize a Python interface. The Python process is not 
#'   started automatically.
#'   
#' \code{$path} Set or get the path to the Python executable. The path 
#'   cannot be changed while the Python process is running.
#'   
#' \code{$host} Set or get the host address of the Python connection.
#'   Default is \code{'localhost'}. The host cannot be changed while the 
#'   Python process is running.
#'   
#' \code{$port} Set or get the port of the Python interface. The port 
#'   cannot be changed while the Python process is running.
#'   
#' \code{$timeout} Set or get the timeout for receiving messages from
#'   Python. Default is 60 seconds.
#'
#' \code{$start()} Start the Python process. The Python process runs 
#'   asynchronously.
#'
#' \code{$running} Check if the Python process is running.
#' 
#' \code{$pid} Get the Process ID of the Python interface. Useful for
#'   diagnosing problems.
#'
#' \code{$set()} Set variables in the Python process. R variables
#'   are encoded into JSON format, sent to the Python process as text,
#'   and decoded into variables on the Python side.
#'
#' \code{$get()} Get a variable from the Python process. Python
#'   variables are encoded into JSON format, sent to R as text,
#'   and decoded into variables on the R side.
#'   
#' \code{$exec()} Execute the specified Python 
#'   commands and invisibly return printed Python output (if any).
#'   Alternatively, the \code{file} argument can be used to specify
#'   a file containing Python code. Note that there will be no return 
#'   value unless an explicit Python \code{print} statement is executed.
#' 
#' \code{$stop()} Stop the Python process by sending a request to the 
#'   Python process. If \code{force = TRUE}, the process will be 
#'   terminated using a system call instead.
#' 
#' \code{print(py)} Show some information about the
#' Python process on the screen, whether it is running and its process id, 
#' etc.
#'
#' @name PythonEnv
#' @examples
#' pypath = Sys.which('python')
#' cat(pypath, "\n")
#' if(nchar(pypath) > 0) { 
#'   py = PythonEnv$new(path = pypath, port = 6011)
#'   py$start()
#'   py$running
#'   py$set(a = 5)
#'   py$get('a')
#'   py$stop(force = TRUE)
#' } else 
#' message("No Python distribution found!")
NULL

#' @export
PythonEnv = R6::R6Class("PythonEnv", cloneable = FALSE,
  private = list(
    currentpid = NULL,
    currentport = NULL,
    currentpath = NULL,
    currenthost = NULL,
    currenttimeout = NULL,
    version = NULL,
    isrunning = NULL,
    socket = function() {
      socketConnection(host = self$host, port = self$port, 
        open = 'r+', blocking = TRUE, server = FALSE, 
        timeout = self$timeout, encoding = "UTF-8")
    }
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
      private$currentpath = as.character(path)
      private$currenthost = as.character(host)
      private$currenttimeout = 60
      private$currentport = as.integer(port)
      private$version = "(version unknown)"
      private$isrunning = FALSE
      invisible(self)
    },
    
    finalize = function() {
      if (self$running)
        self$stop(force = TRUE)
    },
    
    start = function() {
      if (self$running) {
        message("The Python process is already running")
        return(invisible(self))
      }
      if (is.na(self$port) || length(self$port) == 0L) {
        stop("Invalid port specified", call. = FALSE)
      } else if (self$port < 1024L) {
        warning("Using port numbers below 1024 is not recommended")
      }
      if (!file.exists(self$path)) {
        stop("Invalid path specified", call. = FALSE)
      }
      fpath = system.file("py-src/run_server.py", package = "pysockr") 
      system2(self$path, wait = FALSE, 
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
      private$currentpid = as.integer(self$exec('print(OS.getpid())'))
      # get version
      private$version = self$exec('print(SYS.version)')
      self
    },
    
    stop = function(force = FALSE) {
      if (!self$running) {
        message("The Python process is not running")
      } else if (force) {
        tools::pskill(self$pid)
        message("Python process ", self$pid, " was terminated", sep = "")
        private$isrunning = FALSE
      } else {
        res = self$exec("quit")
        if (res != "QUIT")
          stop("The Python process could not be shut down normally")
        private$isrunning = FALSE
        message("Python process ", self$pid, " was terminated", sep = "")
      }
      invisible(self)
    },

    exec = function(..., file = NULL) {
      if (!self$running)
        stop("The Python process is not running", call. = FALSE)
      if(!is.null(file))
        code = paste(readLines(normalizePath(file, mustWork = TRUE)),
          collapse = "\n")
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
      msg = sprintf("print(PYSOCKR_JSON_DUMPS(%s))", varname)
      rjson::fromJSON(self$exec(msg))
    },
    
    set = function(...) {
      if (!self$running)
        stop("The Python process is not running", call. = FALSE)
      dots = list(...)
      if (!all(grepl("^[_a-zA-Z][0-9a-zA-Z]*$", names(dots))))
        stop("Invalid Python variable name")
      jdots = rjson::toJSON(dots)
      self$exec(sprintf("locals().update(%s)", jdots))
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
    
    timeout = function(value) {
      if (missing(value))
        return(private$currenttimeout)
      private$currenttimeout = value
    }
  )
)
