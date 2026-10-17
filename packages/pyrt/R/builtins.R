# Python built-in functions (`builtins` module) exposed through `py_builtin()`
# so transpiled code can reference built-ins both in call position (`len(x)`,
# `sorted(xs, key=...)`) and as first-class values (`map(str, xs)`).

py_builtin_float <- function(x = 0.0) {
  if (inherits(x, "pyrt_instance")) {
    m <- py_getattr_default(x, "__float__", NULL)
    if (!is.null(m)) return(as.double(m()))
  }
  if (is.logical(x) && length(x) == 1L) return(if (isTRUE(x)) 1.0 else 0.0)
  if (is.numeric(x) && length(x) == 1L) return(as.double(x))
  if (gmp::is.bigz(x)) return(as.double(x))
  if (is.character(x) && length(x) == 1L) {
    s <- trimws(x)
    low <- tolower(s)
    if (low %in% c("inf", "+inf", "infinity", "+infinity")) return(Inf)
    if (low %in% c("-inf", "-infinity")) return(-Inf)
    if (low %in% c("nan", "+nan", "-nan")) return(NaN)
    s_clean <- gsub("_", "", s, fixed = TRUE)
    val <- suppressWarnings(as.double(s_clean))
    if (is.na(val)) {
      py_raise("ValueError", sprintf("could not convert string to float: '%s'", x))
    }
    return(val)
  }
  py_raise("TypeError", "float() argument must be a string or a real number")
}

py_builtin_list <- function(x = NULL) {
  if (is.null(x)) return(py_list())
  py_list(py_collect(x))
}

py_builtin_tuple <- function(x = NULL) {
  if (is.null(x)) return(py_tuple())
  py_tuple(py_collect(x))
}

py_builtin_set <- function(x = NULL) {
  if (is.null(x)) return(py_set())
  py_set(py_collect(x))
}

py_builtin_dict <- function(x = NULL, ...) {
  d <- py_dict()
  if (!is.null(x)) {
    if (inherits(x, "pyrt_dict")) {
      d$update(x)
    } else {
      for (pair in py_collect(x)) {
        items <- coerce_items(pair)
        if (length(items) != 2L) {
          py_raise("ValueError", "dictionary update sequence element has length != 2")
        }
        d$set(items[[1L]], items[[2L]])
      }
    }
  }
  kw <- list(...)
  if (length(kw) > 0L) {
    nms <- names(kw)
    for (i in seq_along(kw)) {
      d$set(nms[[i]], kw[[i]])
    }
  }
  d
}

py_builtin_sum <- function(iterable, start = 0L) {
  acc <- start
  for (item in py_collect(iterable)) {
    acc <- py_add(acc, item)
  }
  acc
}

py_builtin_all <- function(iterable) {
  for (item in py_collect(iterable)) {
    if (!py_bool(item)) return(FALSE)
  }
  TRUE
}

py_builtin_any <- function(iterable) {
  for (item in py_collect(iterable)) {
    if (py_bool(item)) return(TRUE)
  }
  FALSE
}

py_builtin_min <- function(..., key = NULL, default = NULL) {
  args <- list(...)
  items <- if (length(args) == 1L) py_collect(args[[1L]]) else args
  if (length(items) == 0L) {
    if (!is.null(default)) return(default)
    py_raise("ValueError", "min() arg is an empty sequence")
  }
  best <- items[[1L]]
  best_k <- if (is.null(key)) best else py_call(key, list(best))
  if (length(items) > 1L) {
    for (i in 2:length(items)) {
      cur <- items[[i]]
      cur_k <- if (is.null(key)) cur else py_call(key, list(cur))
      if (py_lt(cur_k, best_k)) {
        best <- cur
        best_k <- cur_k
      }
    }
  }
  best
}

py_builtin_max <- function(..., key = NULL, default = NULL) {
  args <- list(...)
  items <- if (length(args) == 1L) py_collect(args[[1L]]) else args
  if (length(items) == 0L) {
    if (!is.null(default)) return(default)
    py_raise("ValueError", "max() arg is an empty sequence")
  }
  best <- items[[1L]]
  best_k <- if (is.null(key)) best else py_call(key, list(best))
  if (length(items) > 1L) {
    for (i in 2:length(items)) {
      cur <- items[[i]]
      cur_k <- if (is.null(key)) cur else py_call(key, list(cur))
      if (py_gt(cur_k, best_k)) {
        best <- cur
        best_k <- cur_k
      }
    }
  }
  best
}

py_builtin_sorted <- function(iterable, key = NULL, reverse = FALSE) {
  items <- py_collect(iterable)
  out <- py_list(items)
  out$sort(key = key, reverse = reverse)
  out
}

py_builtin_map <- function(fn, ...) {
  iters <- lapply(list(...), py_iter)
  py_generator(function() {
    args <- vector("list", length(iters))
    for (i in seq_along(iters)) {
      nxt <- tryCatch(
        list(ok = TRUE, val = py_next(iters[[i]])),
        StopIteration = function(e) list(ok = FALSE)
      )
      if (!nxt$ok) return(py_stop_iteration())
      args[i] <- list(nxt$val)
    }
    py_yield(py_call(fn, args))
  })
}

py_builtin_filter <- function(fn, iterable) {
  it <- py_iter(iterable)
  py_generator(function() {
    repeat {
      nxt <- tryCatch(
        list(ok = TRUE, val = py_next(it)),
        StopIteration = function(e) list(ok = FALSE)
      )
      if (!nxt$ok) return(py_stop_iteration())
      keep <- if (is_py_none(fn) || is.null(fn)) {
        py_bool(nxt$val)
      } else {
        py_bool(py_call(fn, list(nxt$val)))
      }
      if (keep) return(py_yield(nxt$val))
    }
  })
}

py_builtin_ord <- function(c) {
  if (!is.character(c) || length(c) != 1L || nchar(c) != 1L) {
    py_raise("TypeError", "ord() expected a character of length 1")
  }
  utf8ToInt(c)[[1L]]
}

py_builtin_chr <- function(i) {
  intToUtf8(as.integer(i))
}

py_builtin_hex <- function(x) {
  z <- as_bigz_exact(x)
  if (z < 0L) paste0("-0x", as.character(-z, b = 16L)) else paste0("0x", as.character(z, b = 16L))
}

py_builtin_oct <- function(x) {
  z <- as_bigz_exact(x)
  if (z < 0L) paste0("-0o", as.character(-z, b = 8L)) else paste0("0o", as.character(z, b = 8L))
}

py_builtin_bin <- function(x) {
  z <- as_bigz_exact(x)
  if (z < 0L) paste0("-0b", as.character(-z, b = 2L)) else paste0("0b", as.character(z, b = 2L))
}

py_builtin_round <- function(number, ndigits = NULL) {
  if (is.null(ndigits) || is_py_none(ndigits)) {
    return(normalize_py_int(gmp::as.bigz(round(as.double(number)))))
  }
  round(as.double(number), digits = as.integer(ndigits))
}

py_builtin_print <- function(..., sep = " ", end = "\n") {
  args <- list(...)
  strs <- vapply(args, py_str, character(1))
  cat(paste(strs, collapse = sep), end, sep = "")
  invisible(py_none())
}

py_builtin_callable <- function(x) {
  is.function(x) || inherits(x, "pyrt_class") ||
    (inherits(x, "pyrt_instance") && !is.null(py_getattr_default(x, "__call__", NULL)))
}

#' Retrieve a Python built-in function or exception constructor by name
#'
#' Looks up a Python built-in function (`len`, `range`, `enumerate`, `zip`,
#' `reversed`, `iter`, `next`, `int`, `float`, `bool`, `str`, `repr`, `ascii`,
#' `abs`, `divmod`, `pow`, `min`, `max`, `sum`, `all`, `any`, `sorted`, `list`,
#' `tuple`, `dict`, `set`, `map`, `filter`, `isinstance`, `issubclass`,
#' `getattr`, `setattr`, `hasattr`, `delattr`, `super`, `property`,
#' `staticmethod`, `classmethod`, `ord`, `chr`, `hex`, `oct`, `bin`, `round`,
#' `print`, `callable`) or built-in exception constructor (`ValueError`,
#' `TypeError`, `KeyError`, `IndexError`, `StopIteration`, `RuntimeError`,
#' `ZeroDivisionError`, `AttributeError`, `LookupError`, `AssertionError`,
#' `NotImplementedError`, `OSError`, `Exception`, `BaseException`).
#'
#' @param name Character string naming the Python built-in.
#' @returns A callable R function or `pyrt_class` representing the built-in.
#' @export
#' @examples
#' py_builtin("len")(py_list(list(1L, 2L, 3L)))
#' py_builtin("sum")(py_range(5L))
py_builtin <- function(name) {
  switch(name,
    len = py_len,
    range = py_range,
    enumerate = py_enumerate,
    zip = py_zip,
    reversed = py_reversed,
    iter = py_iter,
    `next` = py_next,
    int = py_int,
    float = py_builtin_float,
    bool = py_bool,
    str = py_str,
    repr = py_repr,
    ascii = py_ascii,
    abs = py_abs,
    divmod = py_divmod,
    pow = py_pow,
    min = py_builtin_min,
    max = py_builtin_max,
    sum = py_builtin_sum,
    all = py_builtin_all,
    any = py_builtin_any,
    sorted = py_builtin_sorted,
    list = py_builtin_list,
    tuple = py_builtin_tuple,
    dict = py_builtin_dict,
    set = py_builtin_set,
    map = py_builtin_map,
    filter = py_builtin_filter,
    isinstance = py_isinstance,
    issubclass = py_issubclass,
    getattr = py_getattr,
    setattr = py_setattr,
    hasattr = py_hasattr,
    delattr = py_delattr,
    super = py_super,
    property = py_property,
    staticmethod = py_staticmethod,
    classmethod = py_classmethod,
    ord = py_builtin_ord,
    chr = py_builtin_chr,
    hex = py_builtin_hex,
    oct = py_builtin_oct,
    bin = py_builtin_bin,
    round = py_builtin_round,
    print = py_builtin_print,
    callable = py_builtin_callable,
    {
      if (name %in% c("BaseException", "Exception", names(py_exc_parents))) {
        return(make_builtin_exc_class(name))
      }
      py_raise("NameError", sprintf("name '%s' is not defined", name))
    }
  )
}

#' @rdname py_builtin
#' @export
py_builtin_names <- function() {
  c(
    "len", "range", "enumerate", "zip", "reversed", "iter", "next",
    "int", "float", "bool", "str", "repr", "ascii", "abs", "divmod", "pow",
    "min", "max", "sum", "all", "any", "sorted", "list", "tuple", "dict", "set",
    "map", "filter", "isinstance", "issubclass", "getattr", "setattr",
    "hasattr", "delattr", "super", "property", "staticmethod", "classmethod",
    "ord", "chr", "hex", "oct", "bin", "round", "print", "callable",
    "BaseException", "Exception", names(py_exc_parents)
  )
}

#' Runtime helpers for transpiled `*args`/`**kwargs`, `assert`, and signals
#'
#' `py_split_varargs()` partitions an R `list(...)` into a `py_tuple` of extra
#' positional arguments (`$args`) and a `py_dict` of extra keyword arguments
#' (`$kwargs`), raising `TypeError` when unexpected positional or keyword
#' arguments are passed. `py_return_signal()`, `py_continue_signal()`,
#' `is_py_return()`, `is_py_continue()`, and `is_py_break()` propagate
#' `return`/`break`/`continue` out of `py_try()` and `py_with()` closures.
#' `py_assert()` raises `AssertionError` when `test` is falsy under `py_bool()`.
#'
#' @param dots R `list(...)` collected from a function's `...` parameter.
#' @param vararg Logical; whether the Python function accepts `*args`.
#' @param kwarg Logical; whether the Python function accepts `**kwargs`.
#' @param value Return value wrapped in `py_return_signal()`.
#' @param x Object tested by `is_py_return()`, `is_py_continue()`, or
#'   `is_py_break()`.
#' @param test Condition tested by `py_assert()`.
#' @param msg Optional message evaluated and passed to `AssertionError` if
#'   `test` is falsy.
#' @returns `py_split_varargs()` returns `list(args = py_tuple, kwargs = py_dict)`.
#' @export
py_split_varargs <- function(dots, vararg = TRUE, kwarg = TRUE) {
  nms <- names(dots)
  if (is.null(nms)) {
    is_kw <- rep(FALSE, length(dots))
  } else {
    is_kw <- nzchar(nms)
  }
  pos_items <- unname(dots[!is_kw])
  kw_items <- dots[is_kw]
  if (!vararg && length(pos_items) > 0L) {
    py_raise("TypeError", "function received unexpected positional arguments")
  }
  if (!kwarg && length(kw_items) > 0L) {
    bad <- names(kw_items)[[1L]]
    py_raise("TypeError", sprintf("got an unexpected keyword argument '%s'", bad))
  }
  kw_dict <- py_dict()
  if (length(kw_items) > 0L) {
    kw_names <- names(kw_items)
    for (i in seq_along(kw_items)) {
      kw_dict$set(kw_names[[i]], kw_items[[i]])
    }
  }
  list(
    args = py_tuple(pos_items),
    kwargs = kw_dict
  )
}

#' @rdname py_split_varargs
#' @export
py_return_signal <- function(value = py_none()) {
  structure(list(value = value), class = "pyrt_return")
}

#' @rdname py_split_varargs
#' @export
is_py_return <- function(x) {
  inherits(x, "pyrt_return")
}

cont_tok <- structure(list(), class = "pyrt_continue")

#' @rdname py_split_varargs
#' @export
py_continue_signal <- function() cont_tok

#' @rdname py_split_varargs
#' @export
is_py_continue <- function(x) {
  inherits(x, "pyrt_continue")
}

#' @rdname py_split_varargs
#' @export
is_py_break <- function(x) {
  inherits(x, "pyrt_break")
}

#' @rdname py_split_varargs
#' @export
py_assert <- function(test, msg = NULL) {
  if (!py_bool(test)) {
    py_raise("AssertionError", if (is.null(msg)) "" else py_str(msg))
  }
  invisible(py_none())
}

