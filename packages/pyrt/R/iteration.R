# Python iteration protocol (`iter()`, `next()`, `StopIteration`), closure
# generators (`yield` / `yield from`), and `range` / `enumerate` / `zip` /
# `reversed` / `map` / `filter`.

#' Python iteration protocol (`iter`, `next`), generators, and `range`
#'
#' `py_iter()` returns a stateful `pyrt_iterator` over any Python container,
#' string, `py_range()`, generator, R vector/list, or custom class implementing
#' `__iter__` (or `__getitem__`). `py_next()` advances an iterator by one step,
#' returning `default` if supplied or raising `StopIteration` when exhausted.
#' `py_generator()` wraps a step function (`function(yield_val) ...`) into a
#' stateful generator supporting `next()`, `$send()`, and `$close()`.
#' `py_for()` executes a loop body for each item of an iterable and runs
#' `orelse()` if the loop finished without `break`.
#'
#' @param x An iterable or iterator.
#' @param default Optional fallback returned by `py_next()` when `x` is
#'   exhausted instead of raising `StopIteration`.
#' @param step_fn A zero-argument function that returns `py_yield(value)` to
#'   yield an item or `py_stop_iteration(ret)` when the generator returns.
#' @param value Value yielded by `py_yield()` or returned via `py_stop_iteration()`.
#' @param start,stop,step Integer range bounds for `py_range()` (when only one
#'   argument is given, `py_range(stop)` starts at `0L`).
#' @param body Function `function(item)` called on each element in `py_for()`.
#'   Returning `py_break_signal()` exits the loop early without running `orelse`.
#' @param orelse Optional zero-argument function run when `py_for()` completes
#'   normally without breaking.
#' @param ... Iterables passed to `py_zip()`.
#' @returns `py_iter()` and `py_generator()` return `pyrt_iterator` objects;
#'   `py_next()` returns the next element; `py_collect()` returns an R `list` of
#'   all remaining elements.
#' @export
#' @examples
#' it <- py_iter(py_range(1L, 6L, 2L))
#' py_next(it)
#' py_collect(it)
py_iter <- function(x) {
  if (inherits(x, "pyrt_iterator")) return(x)
  if (inherits(x, "pyrt_instance")) {
    fn <- py_getattr_default(x, "__iter__", NULL)
    if (!is.null(fn)) {
      res <- fn()
      if (inherits(res, "pyrt_iterator") || inherits(res, "pyrt_instance")) return(res)
    }
    gfn <- py_getattr_default(x, "__getitem__", NULL)
    if (!is.null(gfn)) {
      idx <- 0L
      return(make_iterator(function() {
        py_try(
          {
            val <- gfn(idx)
            idx <<- idx + 1L
            list(stopped = FALSE, value = val)
          },
          handlers = list(IndexError = function(e) list(stopped = TRUE, value = NULL))
        )
      }))
    }
    py_raise("TypeError", paste0("'", x$`__class__`$name, "' object is not iterable"))
  }

  # Snapshot keys for dict; read live items for list so in-place list mutations
  # behave like CPython's list_iterator.
  if (inherits(x, "pyrt_list")) {
    i <- 0L
    return(make_iterator(function() {
      i <<- i + 1L
      if (i > length(x$.items)) list(stopped = TRUE, value = NULL)
      else list(stopped = FALSE, value = x$.items[[i]])
    }))
  }

  items <- if (inherits(x, "pyrt_tuple") || inherits(x, "pyrt_range")) {
    x$items
  } else if (inherits(x, "pyrt_ndarray")) {
    n <- x$.shape[[1L]]
    if (n == 0L) list() else lapply(seq(0L, n - 1L), function(i) ndarray_getitem(x, i))
  } else if (inherits(x, "pyrt_Series")) {
    as.list(x$.vals)
  } else if (inherits(x, "pyrt_set")) {
    x$.items
  } else if (inherits(x, "pyrt_dict")) {
    x$.keys
  } else if (is.character(x) && length(x) == 1L) {
    if (!nzchar(x)) list() else as.list(strsplit(x, "", fixed = TRUE)[[1L]])
  } else if (is.vector(x) || is.list(x)) {
    as.list(x)
  } else {
    py_raise("TypeError", "object is not iterable")
  }

  i <- 0L
  n <- length(items)
  make_iterator(function() {
    i <<- i + 1L
    if (i > n) list(stopped = TRUE, value = NULL)
    else list(stopped = FALSE, value = items[[i]])
  })
}

make_iterator <- function(pull_fn) {
  env <- new.env(parent = emptyenv())
  env$.exhausted <- FALSE
  env$.pull <- function() {
    if (env$.exhausted) return(list(stopped = TRUE, value = NULL))
    res <- pull_fn()
    if (isTRUE(res$stopped)) env$.exhausted <- TRUE
    res
  }
  class(env) <- "pyrt_iterator"
  env
}

# Internal non-raising step used by `py_in()`, `py_collect()`, `py_for()`, etc.
py_next_cell <- function(it) {
  if (inherits(it, "pyrt_iterator")) {
    return(it$.pull())
  }
  if (inherits(it, "pyrt_instance")) {
    nfn <- py_getattr_default(it, "__next__", NULL)
    if (is.null(nfn)) py_raise("TypeError", paste0("'", it$`__class__`$name, "' object is not an iterator"))
    return(py_try(
      list(stopped = FALSE, value = nfn()),
      handlers = list(StopIteration = function(e) list(stopped = TRUE, value = e$value))
    ))
  }
  py_raise("TypeError", "object is not an iterator")
}

#' @rdname py_iter
#' @export
py_next <- function(x, default = missing_sentinel) {
  cell <- py_next_cell(x)
  if (!cell$stopped) return(cell$value)
  if (!inherits(default, "pyrt_missing")) return(default)
  py_raise("StopIteration", value = cell$value)
}

#' @rdname py_iter
#' @export
py_collect <- function(x) {
  it <- py_iter(x)
  out <- list()
  repeat {
    cell <- py_next_cell(it)
    if (cell$stopped) break
    out[length(out) + 1L] <- list(cell$value)
  }
  out
}

#' @param cont Optional continuation function `function(sent = NULL)` for
#'   CPS-lowered generators.
#' @param thunk Zero-argument continuation thunk for `py_bounce()`.
#' @rdname py_iter
#' @export
py_iter_step <- function(x) {
  py_next_cell(x)
}

#' @rdname py_iter
#' @export
py_bounce <- function(thunk) {
  structure(list(thunk = thunk), class = "pyrt_bounce")
}

#' @rdname py_iter
#' @export
py_yield <- function(value = NULL, cont = NULL) {
  list(stopped = FALSE, value = value, cont = cont)
}

#' @rdname py_iter
#' @export
py_stop_iteration <- function(value = NULL) {
  list(stopped = TRUE, value = value)
}

#' @rdname py_iter
#' @export
py_generator <- function(step_fn) {
  env <- new.env(parent = emptyenv())
  env$.exhausted <- FALSE
  env$.sent <- NULL
  env$.cont <- step_fn
  env$.pull <- function() {
    if (env$.exhausted) return(list(stopped = TRUE, value = NULL))
    res <- py_try(
      {
        cur <- env$.cont
        r <- if (length(formals(cur)) == 0L) cur() else cur(env$.sent)
        while (inherits(r, "pyrt_bounce")) {
          r <- r$thunk()
        }
        r
      },
      handlers = list(StopIteration = function(e) list(stopped = TRUE, value = e$value))
    )
    env$.sent <- NULL
    if (!is.null(res$cont)) {
      env$.cont <- res$cont
    }
    if (isTRUE(res$stopped)) env$.exhausted <- TRUE
    res
  }
  env$send <- function(value = NULL) {
    env$.sent <- value
    cell <- env$.pull()
    if (cell$stopped) py_raise("StopIteration", value = cell$value)
    cell$value
  }
  env$close <- function() {
    env$.exhausted <- TRUE
    invisible(NULL)
  }
  class(env) <- c("pyrt_generator", "pyrt_iterator")
  env
}

break_tok <- structure(list(), class = "pyrt_break")

#' @rdname py_iter
#' @export
py_break_signal <- function() break_tok

#' @rdname py_iter
#' @export
py_for <- function(x, body, orelse = NULL) {
  it <- py_iter(x)
  broke <- FALSE
  repeat {
    cell <- py_next_cell(it)
    if (cell$stopped) break
    res <- body(cell$value)
    if (inherits(res, "pyrt_break")) {
      broke <- TRUE
      break
    }
  }
  if (!broke && !is.null(orelse)) {
    return(orelse())
  }
  invisible(NULL)
}

#' @rdname py_iter
#' @export
py_range_seq <- function(start, stop = NULL, step = 1L) {
  if (is.null(stop)) {
    stop <- as.integer(start)
    start <- 0L
  } else {
    start <- as.integer(start)
    stop <- as.integer(stop)
  }
  step <- as.integer(step)
  if (step == 0L) py_raise("ValueError", "range() arg 3 must not be zero")
  if ((step > 0L && start >= stop) || (step < 0L && start <= stop)) {
    integer()
  } else if (step > 0L) {
    seq.int(start, stop - 1L, by = step)
  } else {
    seq.int(start, stop + 1L, by = step)
  }
}

#' @rdname py_iter
#' @export
py_range <- function(start, stop = NULL, step = 1L) {
  ints <- py_range_seq(start, stop, step)
  if (is.null(stop)) {
    stop <- as.integer(start)
    start <- 0L
  } else {
    start <- as.integer(start)
    stop <- as.integer(stop)
  }
  step <- as.integer(step)
  r <- list(
    start = start,
    stop = stop,
    step = step,
    items = as.list(ints)
  )
  class(r) <- "pyrt_range"
  r
}

#' @export
format.pyrt_range <- function(x, ...) {
  if (x$step == 1L) sprintf("range(%d, %d)", x$start, x$stop)
  else sprintf("range(%d, %d, %d)", x$start, x$stop, x$step)
}

#' @export
print.pyrt_range <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

#' @rdname py_iter
#' @export
py_enumerate <- function(x, start = 0L) {
  it <- py_iter(x)
  idx <- as.integer(start) - 1L
  make_iterator(function() {
    cell <- py_next_cell(it)
    if (cell$stopped) return(cell)
    idx <<- idx + 1L
    list(stopped = FALSE, value = py_tuple(list(idx, cell$value)))
  })
}

#' @rdname py_iter
#' @export
py_zip <- function(...) {
  iters <- lapply(list(...), py_iter)
  if (length(iters) == 0L) {
    return(make_iterator(function() list(stopped = TRUE, value = NULL)))
  }
  make_iterator(function() {
    row <- vector("list", length(iters))
    for (i in seq_along(iters)) {
      cell <- py_next_cell(iters[[i]])
      if (cell$stopped) return(list(stopped = TRUE, value = NULL))
      row[i] <- list(cell$value)
    }
    list(stopped = FALSE, value = py_tuple(row))
  })
}

#' @rdname py_iter
#' @export
py_reversed <- function(x) {
  if (inherits(x, "pyrt_instance")) {
    fn <- py_getattr_default(x, "__reversed__", NULL)
    if (!is.null(fn)) return(fn())
  }
  py_iter(rev(coerce_items(x)))
}
