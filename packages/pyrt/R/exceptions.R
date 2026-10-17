# Python exception hierarchy and `try / except / else / finally` helpers.
#
# Every Python exception is an R condition whose class vector begins with its
# Python exception MRO (e.g. `c("KeyError", "LookupError", "Exception",
# "BaseException", "pyrt_exception", "error", "condition")`), so standard
# `tryCatch()` handlers or [py_try()] can match any ancestor class in the
# Python exception hierarchy.

py_exc_parents <- list(
  BaseException = character(),
  GeneratorExit = "BaseException",
  KeyboardInterrupt = "BaseException",
  SystemExit = "BaseException",
  Exception = "BaseException",
  StopIteration = "Exception",
  StopAsyncIteration = "Exception",
  ArithmeticError = "Exception",
  FloatingPointError = "ArithmeticError",
  OverflowError = "ArithmeticError",
  ZeroDivisionError = "ArithmeticError",
  AssertionError = "Exception",
  AttributeError = "Exception",
  BufferError = "Exception",
  EOFError = "Exception",
  ImportError = "Exception",
  ModuleNotFoundError = "ImportError",
  LookupError = "Exception",
  IndexError = "LookupError",
  KeyError = "LookupError",
  MemoryError = "Exception",
  NameError = "Exception",
  UnboundLocalError = "NameError",
  OSError = "Exception",
  FileExistsError = "OSError",
  FileNotFoundError = "OSError",
  IsADirectoryError = "OSError",
  NotADirectoryError = "OSError",
  PermissionError = "OSError",
  TimeoutError = "OSError",
  ReferenceError = "Exception",
  RuntimeError = "Exception",
  NotImplementedError = "RuntimeError",
  RecursionError = "RuntimeError",
  SyntaxError = "Exception",
  IndentationError = "SyntaxError",
  TabError = "IndentationError",
  SystemError = "Exception",
  TypeError = "Exception",
  ValueError = "Exception",
  UnicodeError = "ValueError",
  UnicodeDecodeError = "UnicodeError",
  UnicodeEncodeError = "UnicodeError",
  UnicodeTranslateError = "UnicodeError",
  Warning = "Exception",
  DeprecationWarning = "Warning",
  FutureWarning = "Warning",
  PendingDeprecationWarning = "Warning",
  RuntimeWarning = "Warning",
  SyntaxWarning = "Warning",
  UserWarning = "Warning"
)

exc_mro <- function(type, extra_mro = NULL) {
  if (!is.null(extra_mro)) {
    return(unique(c(type, extra_mro, "Exception", "BaseException")))
  }
  out <- character()
  cur <- type
  while (length(cur) == 1L && nzchar(cur)) {
    out <- c(out, cur)
    cur <- py_exc_parents[[cur]]
    if (is.null(cur) && !(out[length(out)] %in% c("BaseException", "Exception"))) {
      out <- c(out, "Exception", "BaseException")
      break
    }
  }
  unique(out)
}

#' Construct, raise, and catch Python exceptions
#'
#' `py_exception()` creates an R condition object whose S3 classes follow
#' Python's built-in exception hierarchy (`KeyError` inherits from
#' `LookupError`, `Exception`, `BaseException`, etc.). `py_raise()` signals the
#' exception (optionally chaining `__cause__` via `from =`). `py_isinstance_exc()`
#' tests whether an exception matches a class name (or list/tuple of class
#' names). `py_try()` implements Python's full `try / except / else / finally`
#' semantics (where `else` runs only when `expr` finishes without raising, and
#' `finally` always runs).
#'
#' @param type Exception class name (such as `"ValueError"`, `"KeyError"`,
#'   `"ZeroDivisionError"`, `"StopIteration"`) or a `pyrt_class` exception
#'   subclass.
#' @param ... Positional arguments stored in `exc$args` and formatted into the
#'   condition message.
#' @param value Optional single payload ( notably `StopIteration(value)`).
#' @param exc An exception condition or class name passed to `py_raise()`, or a
#'   condition tested by `py_isinstance_exc()`. If `NULL`, `py_raise()` re-raises
#'   the currently active Python exception.
#' @param cause Optional chained cause (`raise exc from cause`).
#' @param classes Character vector, list, or `py_tuple` of exception class names
#'   or `pyrt_class` objects.
#' @param expr Expression evaluated inside the `try` block.
#' @param handlers Named list of handler functions (`function(e) ...`). A
#'   handler named `"*"` acts as a bare `except:`. Multiple exception types can
#'   be separated with `"|"` (e.g. `"KeyError|IndexError"`).
#' @param orelse Optional function `function() ...` executed if and only if
#'   `expr` did not raise an exception.
#' @param finalbody Optional function `function() ...` always executed on exit.
#' @returns `py_exception()` returns a condition of class `"pyrt_exception"`;
#'   `py_isinstance_exc()` returns a single logical; `py_try()` returns the
#'   value of `expr`, the matched handler, or `orelse()`.
#' @export
#' @examples
#' e <- py_exception("KeyError", "missing")
#' py_isinstance_exc(e, "LookupError")
#' py_try(
#'   py_raise("IndexError", "out of range"),
#'   handlers = list(LookupError = function(err) err$type)
#' )
py_exception <- function(type = "Exception", ..., value = NULL) {
  extra <- NULL
  if (inherits(type, "pyrt_class")) {
    extra <- type$mro_names
    type <- type$name
  }
  args <- list(...)
  if (length(args) == 0L && !is.null(value)) {
    args <- list(value)
  }
  msg_body <- if (length(args) == 0L) {
    ""
  } else if (length(args) == 1L && is.character(args[[1L]])) {
    args[[1L]]
  } else {
    paste(vapply(args, py_str, character(1)), collapse = ", ")
  }
  full_msg <- if (nzchar(msg_body)) paste0(type, ": ", msg_body) else type
  mro <- exc_mro(type, extra)
  val <- if (!is.null(value)) value else if (length(args) >= 1L) args[[1L]] else NULL
  structure(
    list(
      message = full_msg,
      call = NULL,
      type = type,
      args = args,
      value = val,
      `__cause__` = NULL
    ),
    class = unique(c(mro, "pyrt_exception", "error", "condition"))
  )
}

active_exc <- new.env(parent = emptyenv())

#' @rdname py_exception
#' @export
py_raise <- function(exc = NULL, ..., cause = NULL) {
  if (is.null(exc)) {
    if (is.null(active_exc$current)) {
      exc <- py_exception("RuntimeError", "No active exception to reraise")
    } else {
      exc <- active_exc$current
    }
  } else if (is.character(exc) && length(exc) == 1L) {
    exc <- py_exception(exc, ...)
  } else if (inherits(exc, "pyrt_class")) {
    exc <- py_exception(exc, ...)
  } else if (!inherits(exc, "condition")) {
    exc <- py_exception("TypeError", "exceptions must derive from BaseException")
  }
  if (!is.null(cause)) {
    if (is.character(cause) && length(cause) == 1L) {
      cause <- py_exception(cause)
    }
    exc$`__cause__` <- cause
  }
  stop(exc)
}

#' @rdname py_exception
#' @export
py_isinstance_exc <- function(exc, classes) {
  if (!inherits(exc, "condition")) return(FALSE)
  if (inherits(classes, "pyrt_tuple")) {
    classes <- classes$items
  }
  if (is.list(classes)) {
    return(any(vapply(classes, function(cl) py_isinstance_exc(exc, cl), logical(1))))
  }
  if (inherits(classes, "pyrt_class")) {
    classes <- classes$name
  }
  if (is.character(classes)) {
    parts <- unlist(strsplit(classes, "|", fixed = TRUE))
    return(any(parts %in% class(exc)) || ("*" %in% parts))
  }
  FALSE
}

#' @rdname py_exception
#' @export
py_try <- function(expr, handlers = list(), orelse = NULL, finalbody = NULL) {
  if (!is.null(finalbody)) {
    on.exit(finalbody(), add = TRUE)
  }
  raised <- NULL
  val <- tryCatch(
    expr,
    error = function(e) {
      raised <<- e
      NULL
    }
  )
  if (!is.null(raised)) {
    prev <- active_exc$current
    active_exc$current <- raised
    on.exit(active_exc$current <- prev, add = TRUE)
    nms <- names(handlers)
    for (i in seq_along(handlers)) {
      pat <- if (is.null(nms) || !nzchar(nms[[i]])) "*" else nms[[i]]
      if (py_isinstance_exc(raised, pat)) {
        h <- handlers[[i]]
        return(if (length(formals(h)) == 0L) h() else h(raised))
      }
    }
    stop(raised)
  }
  if (!is.null(orelse) && !is_py_return(val) && !is_py_break(val) && !is_py_continue(val)) {
    return(orelse())
  }
  val
}
