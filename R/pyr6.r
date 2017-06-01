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
#' py$exec(...)
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
#' \code{$exec(...)} Execute the specified Python commands and invisibly 
#'   return printed Python output (if any). 
#' 
#' \code{print(py)} or \code{py$print()} shows some information about the
#' Python process on the screen, whether it is running and its process id, 
#' etc.
#'
#' @importFrom R6 R6Class
#' @name PythonEnv
#' @examples
#' \dontrun{
#' py <- PythonEnv$new(port = 6011, path = sys.which("python"))
#' p$is_alive()
#' p
#' p$kill()
#' p$is_alive()
#'
#' p$restart()
#' p$is_alive()
#' Sys.sleep(3)
#' p$is_alive()
#' }
NULL

#' @export
PythonEnv = R6::R6Class("PythonEnv", cloneable = FALSE,
  private = list(
    currentpid = NULL,
    currentport = NULL,
    path = NULL,
    version = NULL,
    isrunning = NULL,
    socket = NULL
  ),
  public = list(
    print = function(...) {
      cat("Python ", private$version, "\n", sep = "")
      cat("(", private$path, ")\n", sep = "")
      if (self$running)
        cat("Listening on Port ", self$port, " (Process ID: ", self$pid,  ")\n",
          sep = "")
      else
        cat("Process not running")
    },
    
    initialize = function(port, path) {
      if (is.na(as.integer(port)))
        stop("Invalid port specified", call. = FALSE)
      if (!file.exists(path))
        stop("Invalid path specified", call. = FALSE)
      private$path = path
      private$currentport = as.integer(port)
      private$version = "version unknown"
      if (port < 1024L)
        warning("Using port numbers below 1024 is not recommended")
      private$isrunning = FALSE
      # socket helper
      private$socket = function() {
        socketConnection(port = self$port, 
                         open = 'r+', blocking = TRUE, server = FALSE, 
                         encoding = "UTF-8")
      }
      invisible(self)
    },
    
    finalize = function() {
      if (self$running)
        self$stop
    },
    
    exec = function(...) {
      if (!self$running)
        stop("The Python process is not running", call. = FALSE)
      code = paste(list(...), collapse = ";")
      s = private$socket()
      on.exit(close(s))
      writeLines(code, s)
      res = readLines(s, warn = FALSE)
      invisible(res)
    },
    
    get = function(varname) {
      if (!self$running)
        stop("The Python process is not running", call. = FALSE)
      s = private$socket()
      on.exit(close(s))
      writeLines(sprintf("print(json.dumps(%s))", varname), s)
      jsonlite::fromJSON(readLines(s, warn = FALSE))
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
    
    pid = function() {
      private$currentpid
    },
    
    start = function() {
      if (self$running) {
        message("The Python process is already running")
      } else {
        fpath = system.file("py-src/server.py", package = "pysockr") 
        system2(private$path, args = c(shQuote(fpath), self$port), wait = FALSE)
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

