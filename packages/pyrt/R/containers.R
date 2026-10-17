# Reference-semantics containers (`py_list`, `py_dict`, `py_set`, `py_tuple`)
# and `py_len()` / `py_hash()`.
#
# R lists copy on modify, which breaks Python code that mutates a list/dict/set
# through an alias or passes it into a function that calls `.append()`. Each
# `pyrt` container wraps an environment (or R6 object) so mutations are visible
# through all references without copying.

# Canonical hash-bucket key so `1L`, `1.0`, `TRUE`, and `as.bigz(1)` map to the
# same bucket in `py_dict` and `py_set` (just as `hash(1) == hash(1.0) ==
# hash(True)` in Python), while unhashable mutable containers raise `TypeError`.
hash_key <- function(k) {
  if (inherits(k, "pyrt_list") || inherits(k, "pyrt_dict") || inherits(k, "pyrt_set")) {
    py_raise("TypeError", paste0("unhashable type: '", class(k)[[1L]], "'"))
  }
  if (inherits(k, "pyrt_instance")) {
    hfn <- py_getattr_default(k, "__hash__", NULL)
    if (is.null(hfn)) py_raise("TypeError", paste0("unhashable type: '", k$`__class__`$name, "'"))
    return(paste0("inst:", as.character(hfn())))
  }
  if (is.null(k)) return("none:")
  if (is_num_scalar(k)) {
    if (is.double(k) && is.nan(k)) return("nan:")
    if ((is_int_like(k) || is.logical(k))) {
      return(paste0("num:", as.character(as_bigz_exact(k))))
    }
    if (is.finite(k) && k == trunc(k) && abs(k) < 1e16) {
      return(paste0("num:", sprintf("%.0f", k)))
    }
    return(paste0("num:", format(k, digits = 17, trim = TRUE)))
  }
  if (is.character(k) && length(k) == 1L) {
    return(paste0("str:", k))
  }
  if (inherits(k, "pyrt_tuple")) {
    sub_keys <- vapply(k$items, hash_key, character(1))
    return(paste0("tup:(", paste(nchar(sub_keys), sub_keys, sep = ":", collapse = ","), ")"))
  }
  paste0("obj:", paste(format(k), collapse = ";"))
}

#' Python `len()` and container constructors (`list`, `tuple`, `dict`, `set`)
#'
#' Reference-semantics containers that mirror Python's built-in `list`,
#' `tuple`, `dict`, and `set` types (with 0-based indexing, negative indices,
#' slices, and Python methods such as `$append()`, `$extend()`, `$pop()`,
#' `$sort()`, `$get()`, `$setdefault()`, `$update()`, `$items()`, `$union()`,
#' `$intersection()`, etc.). Mutating a `py_list`, `py_dict`, or `py_set`
#' updates it in place across all variables referencing it.
#'
#' @param x An object passed to `py_len()`, or initial elements / iterable for
#'   `py_list()`, `py_tuple()`, `py_set()`, or `py_dict()`.
#' @param ... Additional named or unnamed arguments (for `py_dict(...)`,
#'   keyword pairs to insert).
#' @returns A container object (`pyrt_list`, `pyrt_tuple`, `pyrt_dict`, or
#'   `pyrt_set`), or an integer length from `py_len()`.
#' @export
#' @examples
#' xs <- py_list(list(10L, 20L, 30L))
#' alias <- xs
#' xs$append(40L)
#' py_len(alias)
#' py_getitem(alias, -1L)
py_len <- function(x) {
  if (inherits(x, "pyrt_instance") || inherits(x, "pyrt_ndarray") ||
      inherits(x, "pyrt_Series") || inherits(x, "pyrt_DataFrame")) {
    fn <- py_getattr_default(x, "__len__", NULL)
    if (!is.null(fn)) return(as.integer(fn()))
  }
  if (inherits(x, "pyrt_list") || inherits(x, "pyrt_dict") || inherits(x, "pyrt_set")) {
    return(x$len())
  }
  if (inherits(x, "pyrt_tuple") || inherits(x, "pyrt_range")) {
    return(length(x$items))
  }
  if (is.character(x) && length(x) == 1L) {
    return(nchar(x, type = "chars"))
  }
  length(x)
}

coerce_items <- function(x) {
  if (is.null(x)) return(list())
  if (inherits(x, "pyrt_ndarray")) {
    n <- x$.shape[[1L]]
    return(lapply(seq(0L, n - 1L), function(i) ndarray_getitem(x, i)))
  }
  if (inherits(x, "pyrt_Series")) return(as.list(x$.vals))
  if (inherits(x, "pyrt_list")) return(x$as_r_list())
  if (inherits(x, "pyrt_tuple")) return(x$items)
  if (inherits(x, "pyrt_set")) return(x$as_r_list())
  if (inherits(x, "pyrt_dict")) return(x$keys()$as_r_list())
  if (inherits(x, "pyrt_iterator") || inherits(x, "pyrt_generator") || inherits(x, "pyrt_instance")) {
    return(py_collect(x))
  }
  if (is.character(x) && length(x) == 1L) {
    if (!nzchar(x)) return(list())
    return(as.list(strsplit(x, "", fixed = TRUE)[[1L]]))
  }
  as.list(x)
}

# ---- py_tuple (immutable) ---------------------------------------------------

#' @rdname py_len
#' @export
py_tuple <- function(x = NULL) {
  items <- unname(coerce_items(x))
  self <- new.env(parent = emptyenv())
  self$items <- items
  self$len <- function() length(self$items)
  self$as_r_list <- function() self$items
  self$count <- function(value) {
    sum(vapply(self$items, function(el) py_eq(el, value), logical(1)))
  }
  self$index <- function(value, start = 0L, stop = NULL) {
    n <- length(self$items)
    s <- clamp_slice_bound(as.integer(start), n)
    e <- if (is.null(stop)) n else clamp_slice_bound(as.integer(stop), n)
    if (e > s) {
      for (i in seq.int(s + 1L, e)) {
        if (py_eq(self$items[[i]], value)) return(i - 1L)
      }
    }
    py_raise("ValueError", "tuple.index(x): x not in tuple")
  }
  class(self) <- c("pyrt_tuple", "pyrt_container")
  self
}

#' @export
format.pyrt_tuple <- function(x, ...) {
  n <- length(x$items)
  if (n == 0L) return("()")
  strs <- vapply(x$items, py_repr, character(1))
  if (n == 1L) return(paste0("(", strs[[1L]], ",)"))
  paste0("(", paste(strs, collapse = ", "), ")")
}

#' @export
print.pyrt_tuple <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

# ---- py_list (mutable reference semantics) ----------------------------------

#' @rdname py_len
#' @export
py_list <- function(x = NULL) {
  self <- new.env(parent = emptyenv())
  self$.items <- unname(coerce_items(x))

  self$len <- function() length(self$.items)
  self$as_r_list <- function() self$.items

  self$append <- function(item) {
    self$.items[length(self$.items) + 1L] <- list(item)
    invisible(NULL)
  }
  self$extend <- function(iterable) {
    more <- coerce_items(iterable)
    if (length(more) > 0L) {
      self$.items <- c(self$.items, unname(more))
    }
    invisible(NULL)
  }
  self$insert <- function(index, item) {
    n <- length(self$.items)
    idx <- clamp_slice_bound(as.integer(index), n)
    if (idx <= 0L) {
      self$.items <- c(list(item), self$.items)
    } else if (idx >= n) {
      self$.items[n + 1L] <- list(item)
    } else {
      self$.items <- c(self$.items[seq_len(idx)], list(item), self$.items[seq.int(idx + 1L, n)])
    }
    invisible(NULL)
  }
  self$pop <- function(index = -1L) {
    n <- length(self$.items)
    if (n == 0L) py_raise("IndexError", "pop from empty list")
    r_idx <- norm_index(as.integer(index), n, "pop index out of range")
    val <- self$.items[[r_idx]]
    self$.items <- self$.items[-r_idx]
    val
  }
  self$remove <- function(value) {
    for (i in seq_along(self$.items)) {
      if (py_eq(self$.items[[i]], value)) {
        self$.items <- self$.items[-i]
        return(invisible(NULL))
      }
    }
    py_raise("ValueError", "list.remove(x): x not in list")
  }
  self$clear <- function() {
    self$.items <- list()
    invisible(NULL)
  }
  self$copy <- function() {
    py_list(self$.items)
  }
  self$reverse <- function() {
    self$.items <- rev(self$.items)
    invisible(NULL)
  }
  self$count <- function(value) {
    sum(vapply(self$.items, function(el) py_eq(el, value), logical(1)))
  }
  self$index <- function(value, start = 0L, stop = NULL) {
    n <- length(self$.items)
    s <- clamp_slice_bound(as.integer(start), n)
    e <- if (is.null(stop)) n else clamp_slice_bound(as.integer(stop), n)
    if (e > s) {
      for (i in seq.int(s + 1L, e)) {
        if (py_eq(self$.items[[i]], value)) return(i - 1L)
      }
    }
    py_raise("ValueError", paste0(py_repr(value), " is not in list"))
  }
  self$sort <- function(key = NULL, reverse = FALSE) {
    n <- length(self$.items)
    if (n <= 1L) return(invisible(NULL))
    keys <- if (is.null(key)) self$.items else lapply(self$.items, key)
    # Stable insertion sort using Python's `<` (`py_lt`).
    ord <- seq_len(n)
    for (i in 2:n) {
      cur_idx <- ord[[i]]
      cur_key <- keys[[cur_idx]]
      j <- i - 1L
      while (j >= 1L) {
        cmp <- if (isTRUE(reverse)) py_lt( keys[[ord[[j]]]], cur_key ) else py_lt( cur_key, keys[[ord[[j]]]] )
        if (!cmp) break
        ord[[j + 1L]] <- ord[[j]]
        j <- j - 1L
      }
      ord[[j + 1L]] <- cur_idx
    }
    self$.items <- self$.items[ord]
    invisible(NULL)
  }

  class(self) <- c("pyrt_list", "pyrt_container")
  self
}

#' @export
format.pyrt_list <- function(x, ...) {
  strs <- vapply(x$.items, py_repr, character(1))
  paste0("[", paste(strs, collapse = ", "), "]")
}

#' @export
print.pyrt_list <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

# ---- py_dict (insertion-ordered, arbitrary hashable keys) -------------------

missing_sentinel <- structure(list(), class = "pyrt_missing")

#' @rdname py_len
#' @export
py_dict <- function(x = NULL, ...) {
  self <- new.env(parent = emptyenv())
  self$.hashes <- character()
  self$.keys <- list()
  self$.vals <- list()

  find_pos <- function(k) {
    hk <- hash_key(k)
    cand <- which(self$.hashes == hk)
    for (idx in cand) {
      if (py_eq(self$.keys[[idx]], k)) return(list(idx = idx, hk = hk))
    }
    list(idx = 0L, hk = hk)
  }

  self$len <- function() length(self$.keys)

  self$set <- function(key, value) {
    loc <- find_pos(key)
    if (loc$idx > 0L) {
      self$.vals[loc$idx] <- list(value)
    } else {
      n <- length(self$.keys) + 1L
      self$.hashes[[n]] <- loc$hk
      self$.keys[n] <- list(key)
      self$.vals[n] <- list(value)
    }
    invisible(value)
  }

  self$getitem <- function(key) {
    loc <- find_pos(key)
    if (loc$idx == 0L) py_raise("KeyError", py_repr(key))
    self$.vals[[loc$idx]]
  }

  self$get <- function(key, default = NULL) {
    loc <- find_pos(key)
    if (loc$idx == 0L) default else self$.vals[[loc$idx]]
  }

  self$contains <- function(key) {
    find_pos(key)$idx > 0L
  }

  self$delitem <- function(key) {
    loc <- find_pos(key)
    if (loc$idx == 0L) py_raise("KeyError", py_repr(key))
    i <- loc$idx
    self$.hashes <- self$.hashes[-i]
    self$.keys <- self$.keys[-i]
    self$.vals <- self$.vals[-i]
    invisible(NULL)
  }

  self$pop <- function(key, default = missing_sentinel) {
    loc <- find_pos(key)
    if (loc$idx == 0L) {
      if (inherits(default, "pyrt_missing")) py_raise("KeyError", py_repr(key))
      return(default)
    }
    val <- self$.vals[[loc$idx]]
    self$delitem(key)
    val
  }

  self$popitem <- function() {
    n <- length(self$.keys)
    if (n == 0L) py_raise("KeyError", "popitem(): dictionary is empty")
    k <- self$.keys[[n]]
    v <- self$.vals[[n]]
    self$.hashes <- self$.hashes[-n]
    self$.keys <- self$.keys[-n]
    self$.vals <- self$.vals[-n]
    py_tuple(list(k, v))
  }

  self$setdefault <- function(key, default = NULL) {
    loc <- find_pos(key)
    if (loc$idx > 0L) return(self$.vals[[loc$idx]])
    self$set(key, default)
    default
  }

  self$clear <- function() {
    self$.hashes <- character()
    self$.keys <- list()
    self$.vals <- list()
    invisible(NULL)
  }

  self$copy <- function() {
    out <- py_dict()
    out$.hashes <- self$.hashes
    out$.keys <- self$.keys
    out$.vals <- self$.vals
    out
  }

  self$keys <- function() py_list(self$.keys)
  self$values <- function() py_list(self$.vals)
  self$items <- function() {
    n <- length(self$.keys)
    pairs <- vector("list", n)
    for (i in seq_len(n)) {
      pairs[[i]] <- py_tuple(list(self$.keys[[i]], self$.vals[[i]]))
    }
    py_list(pairs)
  }

  self$update <- function(other = NULL, ...) {
    if (!is.null(other)) {
      if (inherits(other, "pyrt_dict")) {
        for (i in seq_along(other$.keys)) {
          self$set(other$.keys[[i]], other$.vals[[i]])
        }
      } else if (is.list(other) && !is.null(names(other))) {
        nms <- names(other)
        for (i in seq_along(other)) self$set(nms[[i]], other[[i]])
      } else {
        for (pair in coerce_items(other)) {
          els <- coerce_items(pair)
          if (length(els) != 2L) py_raise("ValueError", "dictionary update sequence element has length != 2")
          self$set(els[[1L]], els[[2L]])
        }
      }
    }
    dots <- list(...)
    if (length(dots) > 0L) {
      nms <- names(dots)
      for (i in seq_along(dots)) self$set(nms[[i]], dots[[i]])
    }
    invisible(NULL)
  }

  self$equals <- function(other) {
    if (!inherits(other, "pyrt_dict")) return(FALSE)
    if (length(self$.keys) != length(other$.keys)) return(FALSE)
    for (i in seq_along(self$.keys)) {
      k <- self$.keys[[i]]
      if (!other$contains(k)) return(FALSE)
      if (!py_eq(self$.vals[[i]], other$getitem(k))) return(FALSE)
    }
    TRUE
  }

  if (!is.null(x) || ...length() > 0L) {
    self$update(x, ...)
  }
  class(self) <- c("pyrt_dict", "pyrt_container")
  self
}

#' @export
format.pyrt_dict <- function(x, ...) {
  n <- length(x$.keys)
  if (n == 0L) return("{}")
  parts <- character(n)
  for (i in seq_len(n)) {
    parts[[i]] <- paste0(py_repr(x$.keys[[i]]), ": ", py_repr(x$.vals[[i]]))
  }
  paste0("{", paste(parts, collapse = ", "), "}")
}

#' @export
print.pyrt_dict <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

# ---- py_set (mutable reference semantics) -----------------------------------

#' @rdname py_len
#' @export
py_set <- function(x = NULL) {
  self <- new.env(parent = emptyenv())
  self$.hashes <- character()
  self$.items <- list()

  find_pos <- function(v) {
    hk <- hash_key(v)
    cand <- which(self$.hashes == hk)
    for (idx in cand) {
      if (py_eq(self$.items[[idx]], v)) return(list(idx = idx, hk = hk))
    }
    list(idx = 0L, hk = hk)
  }

  self$len <- function() length(self$.items)
  self$as_r_list <- function() self$.items
  self$contains <- function(v) find_pos(v)$idx > 0L

  self$add <- function(v) {
    loc <- find_pos(v)
    if (loc$idx == 0L) {
      n <- length(self$.items) + 1L
      self$.hashes[[n]] <- loc$hk
      self$.items[n] <- list(v)
    }
    invisible(NULL)
  }
  self$discard <- function(v) {
    loc <- find_pos(v)
    if (loc$idx > 0L) {
      i <- loc$idx
      self$.hashes <- self$.hashes[-i]
      self$.items <- self$.items[-i]
    }
    invisible(NULL)
  }
  self$remove <- function(v) {
    loc <- find_pos(v)
    if (loc$idx == 0L) py_raise("KeyError", py_repr(v))
    i <- loc$idx
    self$.hashes <- self$.hashes[-i]
    self$.items <- self$.items[-i]
    invisible(NULL)
  }
  self$pop <- function() {
    if (length(self$.items) == 0L) py_raise("KeyError", "pop from an empty set")
    v <- self$.items[[1L]]
    self$.hashes <- self$.hashes[-1L]
    self$.items <- self$.items[-1L]
    v
  }
  self$clear <- function() {
    self$.hashes <- character()
    self$.items <- list()
    invisible(NULL)
  }
  self$copy <- function() {
    out <- py_set()
    out$.hashes <- self$.hashes
    out$.items <- self$.items
    out
  }
  self$update <- function(...) {
    for (it in list(...)) {
      for (el in coerce_items(it)) self$add(el)
    }
    invisible(NULL)
  }
  self$union <- function(...) {
    out <- self$copy()
    out$update(...)
    out
  }
  self$intersection <- function(other) {
    other_s <- if (inherits(other, "pyrt_set")) other else py_set(other)
    out <- py_set()
    for (el in self$.items) if (other_s$contains(el)) out$add(el)
    out
  }
  self$difference <- function(other) {
    other_s <- if (inherits(other, "pyrt_set")) other else py_set(other)
    out <- py_set()
    for (el in self$.items) if (!other_s$contains(el)) out$add(el)
    out
  }
  self$symmetric_difference <- function(other) {
    other_s <- if (inherits(other, "pyrt_set")) other else py_set(other)
    out <- self$difference(other_s)
    for (el in other_s$.items) if (!self$contains(el)) out$add(el)
    out
  }
  self$issubset <- function(other) {
    other_s <- if (inherits(other, "pyrt_set")) other else py_set(other)
    for (el in self$.items) if (!other_s$contains(el)) return(FALSE)
    TRUE
  }
  self$issuperset <- function(other) {
    other_s <- if (inherits(other, "pyrt_set")) other else py_set(other)
    other_s$issubset(self)
  }
  self$equals <- function(other) {
    if (!inherits(other, "pyrt_set")) return(FALSE)
    length(self$.items) == length(other$.items) && self$issubset(other)
  }

  if (!is.null(x)) {
    for (el in coerce_items(x)) self$add(el)
  }
  class(self) <- c("pyrt_set", "pyrt_container")
  self
}

#' @export
format.pyrt_set <- function(x, ...) {
  if (length(x$.items) == 0L) return("set()")
  strs <- vapply(x$.items, py_repr, character(1))
  paste0("{", paste(strs, collapse = ", "), "}")
}

#' @export
print.pyrt_set <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}
