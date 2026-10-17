# 0-based indexing, negative indices, `slice(start, stop, step)`, subscript
# reading/writing/deletion (`py_getitem`, `py_setitem`, `py_delitem`), and
# sequence unpacking (`a, *b, c = rhs`).

norm_index <- function(i, n, err_msg = "index out of range") {
  idx <- if (i < 0L) n + i else i
  if (idx < 0L || idx >= n) py_raise("IndexError", err_msg)
  idx + 1L
}

clamp_slice_bound <- function(i, n) {
  idx <- if (i < 0L) n + i else i
  max(0L, min(n, idx))
}

#' Python `slice()` and 0-based subscript helpers
#'
#' `py_slice(start, stop, step)` constructs a Python slice object (including its
#' `$indices(length)` method). `py_getitem()`, `py_setitem()`, and `py_delitem()`
#' implement 0-based indexing, negative indices, slices with arbitrary positive
#' or negative steps (including extended slice assignment and deletion on
#' `py_list`), dictionary key lookup, string slicing, and `__getitem__` /
#' `__setitem__` / `__delitem__` dunder dispatch on custom classes.
#' `py_unpack()` implements Python's sequence and starred unpacking (`a, *b, c =
#' seq`), raising `ValueError` when the item count does not match.
#'
#' @param start,stop,step Slice bounds (`NULL` for omitted bounds; `step` cannot
#'   be `0L`).
#' @param x Target object (container, string, vector, or `pyrt_instance`).
#' @param key 0-based integer index, `pyrt_slice`, dictionary key, or `py_tuple`
#'   of indices.
#' @param value Replacement value for `py_setitem()`.
#' @param n_before Number of mandatory targets before a starred target (or the
#'   exact target count when `has_star = FALSE`).
#' @param n_after Number of mandatory targets after the starred target.
#' @param has_star Logical; `TRUE` when the target list contains `*rest`.
#' @returns `py_slice()` returns a `pyrt_slice` object; `py_getitem()` returns
#'   the indexed element or sliced sub-container; `py_setitem()` and
#'   `py_delitem()` mutate reference containers in place and return the modified
#'   object invisibly; `py_unpack()` returns a flat R `list` of unpacked values.
#' @export
#' @examples
#' xs <- py_list(list("a", "b", "c", "d", "e"))
#' py_getitem(xs, -1L)
#' py_getitem(xs, py_slice(1L, 4L, 2L))
#' py_setitem(xs, py_slice(1L, 3L), list("B", "C", "CC"))
#' xs
py_slice <- function(start = NULL, stop = NULL, step = NULL) {
  if (!is.null(step) && as.integer(step) == 0L) {
    py_raise("ValueError", "slice step cannot be zero")
  }
  s <- list(
    start = if (is.null(start)) NULL else as.integer(start),
    stop = if (is.null(stop)) NULL else as.integer(stop),
    step = if (is.null(step)) NULL else as.integer(step),
    indices = function(length) {
      ind <- slice_indices_tuple(start, stop, step, as.integer(length))
      py_tuple(list(ind$start, ind$stop, ind$step))
    }
  )
  class(s) <- "pyrt_slice"
  s
}

# Reproduces Python's `slice(start, stop, step).indices(n)` -> (start, stop, step).
slice_indices_tuple <- function(start, stop, step, n) {
  st <- if (is.null(step)) 1L else as.integer(step)
  if (st == 0L) py_raise("ValueError", "slice step cannot be zero")
  if (st > 0L) {
    s <- if (is.null(start)) 0L else as.integer(start)
    if (s < 0L) s <- max(0L, n + s) else s <- min(n, s)
    e <- if (is.null(stop)) n else as.integer(stop)
    if (e < 0L) e <- max(0L, n + e) else e <- min(n, e)
  } else {
    s <- if (is.null(start)) n - 1L else as.integer(start)
    if (s < 0L) s <- max(-1L, n + s) else s <- min(n - 1L, s)
    e <- if (is.null(stop)) -1L else as.integer(stop)
    if (e < 0L) e <- max(-1L, n + e) else e <- min(n - 1L, e)
  }
  list(start = s, stop = e, step = st)
}

# Compute 1-based R positions selected by a `pyrt_slice` on a sequence of length `n`.
slice_r_positions <- function(sl, n) {
  ind <- slice_indices_tuple(sl$start, sl$stop, sl$step, n)
  s <- ind$start
  e <- ind$stop
  st <- ind$step
  if ((st > 0L && s >= e) || (st < 0L && s <= e)) {
    return(integer())
  }
  if (st > 0L) {
    seq.int(s, e - 1L, by = st) + 1L
  } else {
    seq.int(s, e + 1L, by = st) + 1L
  }
}

#' @rdname py_slice
#' @export
py_getitem <- function(x, key) {
  if (inherits(x, "pyrt_instance") || inherits(x, "pyrt_ndarray") ||
      inherits(x, "pyrt_Series") || inherits(x, "pyrt_DataFrame")) {
    fn <- py_getattr_default(x, "__getitem__", NULL)
    if (!is.null(fn)) return(fn(key))
    py_raise("TypeError", paste0("'", x$`__class__`$name, "' object is not subscriptable"))
  }
  if (inherits(x, "pyrt_dict")) {
    return(x$getitem(key))
  }
  if (inherits(x, "pyrt_list")) {
    items <- x$.items
    if (inherits(key, "pyrt_slice")) {
      pos <- slice_r_positions(key, length(items))
      return(py_list(items[pos]))
    }
    r_idx <- norm_index(as.integer(key), length(items), "list index out of range")
    return(items[[r_idx]])
  }
  if (inherits(x, "pyrt_tuple") || inherits(x, "pyrt_range")) {
    items <- x$items
    if (inherits(key, "pyrt_slice")) {
      pos <- slice_r_positions(key, length(items))
      return(py_tuple(items[pos]))
    }
    r_idx <- norm_index(as.integer(key), length(items), "tuple index out of range")
    return(items[[r_idx]])
  }
  if (is.character(x) && length(x) == 1L) {
    chars <- if (nzchar(x)) strsplit(x, "", fixed = TRUE)[[1L]] else character()
    if (inherits(key, "pyrt_slice")) {
      pos <- slice_r_positions(key, length(chars))
      return(paste0(chars[pos], collapse = ""))
    }
    r_idx <- norm_index(as.integer(key), length(chars), "string index out of range")
    return(chars[[r_idx]])
  }
  if (is.vector(x) || is.list(x)) {
    if (inherits(key, "pyrt_slice")) {
      pos <- slice_r_positions(key, length(x))
      return(x[pos])
    }
    r_idx <- norm_index(as.integer(key), length(x), "index out of range")
    return(x[[r_idx]])
  }
  py_raise("TypeError", "object is not subscriptable")
}

#' @rdname py_slice
#' @export
py_setitem <- function(x, key, value) {
  if (inherits(x, "pyrt_instance") || inherits(x, "pyrt_ndarray") || inherits(x, "pyrt_DataFrame")) {
    fn <- py_getattr_default(x, "__setitem__", NULL)
    if (!is.null(fn)) {
      fn(key, value)
      return(invisible(x))
    }
    py_raise("TypeError", paste0("'", x$`__class__`$name, "' object does not support item assignment"))
  }
  if (inherits(x, "pyrt_dict")) {
    x$set(key, value)
    return(invisible(x))
  }
  if (inherits(x, "pyrt_list")) {
    n <- length(x$.items)
    if (inherits(key, "pyrt_slice")) {
      repl <- unname(coerce_items(value))
      st <- if (is.null(key$step)) 1L else key$step
      if (st == 1L) {
        ind <- slice_indices_tuple(key$start, key$stop, 1L, n)
        s <- ind$start
        e <- max(s, ind$stop)
        before <- if (s > 0L) x$.items[seq_len(s)] else list()
        after <- if (e < n) x$.items[seq.int(e + 1L, n)] else list()
        x$.items <- c(before, repl, after)
      } else {
        pos <- slice_r_positions(key, n)
        if (length(pos) != length(repl)) {
          py_raise(
            "ValueError",
            sprintf("attempt to assign sequence of size %d to extended slice of size %d",
                    length(repl), length(pos))
          )
        }
        for (i in seq_along(pos)) {
          x$.items[pos[[i]]] <- list(repl[[i]])
        }
      }
      return(invisible(x))
    }
    r_idx <- norm_index(as.integer(key), n, "list assignment index out of range")
    x$.items[r_idx] <- list(value)
    return(invisible(x))
  }
  if (inherits(x, "pyrt_tuple") || is.character(x)) {
    py_raise("TypeError", "object does not support item assignment")
  }
  py_raise("TypeError", "object does not support item assignment")
}

#' @rdname py_slice
#' @export
py_delitem <- function(x, key) {
  if (inherits(x, "pyrt_instance")) {
    fn <- py_getattr_default(x, "__delitem__", NULL)
    if (!is.null(fn)) {
      fn(key)
      return(invisible(x))
    }
    py_raise("TypeError", paste0("'", x$`__class__`$name, "' object does not support item deletion"))
  }
  if (inherits(x, "pyrt_dict")) {
    x$delitem(key)
    return(invisible(x))
  }
  if (inherits(x, "pyrt_list")) {
    n <- length(x$.items)
    if (inherits(key, "pyrt_slice")) {
      pos <- slice_r_positions(key, n)
      if (length(pos) > 0L) {
        x$.items <- x$.items[-pos]
      }
      return(invisible(x))
    }
    r_idx <- norm_index(as.integer(key), n, "list assignment index out of range")
    x$.items <- x$.items[-r_idx]
    return(invisible(x))
  }
  py_raise("TypeError", "object doesn't support item deletion")
}

#' @rdname py_slice
#' @export
py_unpack <- function(x, n_before, n_after = 0L, has_star = FALSE) {
  items <- coerce_items(x)
  n <- length(items)
  n_before <- as.integer(n_before)
  n_after <- as.integer(n_after)

  if (!isTRUE(has_star)) {
    if (n < n_before) {
      py_raise("ValueError", sprintf("not enough values to unpack (expected %d, got %d)", n_before, n))
    }
    if (n > n_before) {
      py_raise("ValueError", sprintf("too many values to unpack (expected %d)", n_before))
    }
    return(items)
  }

  min_req <- n_before + n_after
  if (n < min_req) {
    py_raise("ValueError", sprintf("not enough values to unpack (expected at least %d, got %d)", min_req, n))
  }
  head_part <- if (n_before > 0L) items[seq_len(n_before)] else list()
  tail_part <- if (n_after > 0L) items[seq.int(n - n_after + 1L, n)] else list()
  mid_len <- n - min_req
  mid_items <- if (mid_len > 0L) items[seq.int(n_before + 1L, n_before + mid_len)] else list()
  c(head_part, list(py_list(mid_items)), tail_part)
}
