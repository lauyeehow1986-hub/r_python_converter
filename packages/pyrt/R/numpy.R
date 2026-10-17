# Phase 5: NumPy / SciPy / Pandas Bridge for `pyrt`
#
# Provides `py_stdlib("numpy")`, `py_stdlib("scipy")`, and `py_stdlib("pandas")`
# backed natively by R numeric arrays/matrices, `stats`, and `data.frame`
# without any Python installation or reticulate dependency.

# ---- 1. `pyrt_ndarray` class & helpers --------------------------------------

#' Create a Python-compatible `ndarray` backed by an R vector/matrix/array
#'
#' Wraps an R atomic vector, matrix, or array in a reference-semantics
#' `pyrt_ndarray` object with 0-based indexing, `.shape` tuple, `.ndim`,
#' `.size`, `.dtype`, `.T` transpose, `.reshape()`, `.flatten()`, `.tolist()`,
#' `.sum()`, `.mean()`, `.std()`, `.var()`, `.min()`, `.max()`, `.argmin()`,
#' `.argmax()`, `.cumsum()`, `.dot()`, and elementwise arithmetic/comparison
#' operators with NumPy broadcasting semantics.
#'
#' @param data An R vector, matrix, array, `pyrt_list`, `pyrt_tuple`, or
#'   `pyrt_ndarray`.
#' @param dtype Optional dtype string (`"float64"`, `"int32"`, `"bool"`).
#' @returns A `pyrt_ndarray` environment.
#' @export
#' @examples
#' np <- py_stdlib("numpy")
#' a <- np$array(list(list(1, 2, 3), list(4, 5, 6)))
#' a$shape
#' a$sum(axis = 0L)
py_ndarray <- function(data, dtype = NULL) {
  if (inherits(data, "pyrt_ndarray")) {
    arr <- data$.arr
    shape <- data$.shape
    dt <- if (is.null(dtype)) data$.dtype else as.character(dtype)
  } else {
    parsed <- coerce_to_r_array(data)
    arr <- parsed$arr
    shape <- parsed$shape
    dt <- if (is.null(dtype)) parsed$dtype else as.character(dtype)
  }

  if (!is.null(dtype)) {
    if (dtype %in% c("int", "int32", "int64")) {
      storage.mode(arr) <- "integer"
      dt <- "int32"
    } else if (dtype %in% c("float", "float64", "double")) {
      storage.mode(arr) <- "double"
      dt <- "float64"
    } else if (dtype %in% c("bool", "bool_")) {
      storage.mode(arr) <- "logical"
      dt <- "bool"
    }
  }

  self <- new.env(parent = emptyenv())
  self$.arr <- arr
  self$.shape <- as.integer(shape)
  self$.dtype <- dt
  class(self) <- c("pyrt_ndarray", "pyrt_container")
  self
}

coerce_to_r_array <- function(x) {
  if (inherits(x, "pyrt_ndarray")) {
    return(list(arr = x$.arr, shape = x$.shape, dtype = x$.dtype))
  }
  if (inherits(x, "pyrt_list") || inherits(x, "pyrt_tuple")) {
    x <- x$as_r_list()
  } else if (inherits(x, "pyrt_iterator") || inherits(x, "pyrt_range")) {
    x <- py_collect(x)
  }
  if (is.array(x) || is.matrix(x)) {
    sh <- dim(x)
    dt <- infer_dtype(x)
    return(list(arr = x, shape = as.integer(sh), dtype = dt))
  }
  if (is.atomic(x) && !is.list(x)) {
    dt <- infer_dtype(x)
    if (length(x) == 0L) {
      return(list(arr = numeric(0), shape = 0L, dtype = "float64"))
    }
    if (length(x) == 1L) {
      return(list(arr = as.vector(x), shape = integer(0), dtype = dt))
    }
    return(list(arr = as.vector(x), shape = as.integer(length(x)), dtype = dt))
  }
  if (is.list(x)) {
    if (length(x) == 0L) {
      return(list(arr = numeric(0), shape = 0L, dtype = "float64"))
    }
    subs <- lapply(x, coerce_to_r_array)
    first_sh <- subs[[1L]]$shape
    same_shape <- all(vapply(subs, function(s) identical(s$shape, first_sh), logical(1)))
    if (same_shape) {
      new_shape <- c(length(subs), first_sh)
      if (length(first_sh) == 0L) {
        vec <- unlist(lapply(subs, `[[`, "arr"))
        return(list(arr = as.vector(vec), shape = as.integer(length(vec)), dtype = infer_dtype(vec)))
      }
      if (first_sh[[1L]] > 0L) {
        flat_list <- lapply(subs, function(s) as.vector(aperm_to_rowmajor(s$arr, s$shape)))
        row_major_vec <- unlist(flat_list)
        arr <- rowmajor_to_r_array(row_major_vec, new_shape)
        return(list(arr = arr, shape = as.integer(new_shape), dtype = infer_dtype(row_major_vec)))
      }
    }
    vec <- unlist(lapply(subs, `[[`, "arr"))
    return(list(arr = vec, shape = as.integer(length(vec)), dtype = infer_dtype(vec)))
  }
  vec <- as.numeric(x)
  list(arr = vec, shape = as.integer(length(vec)), dtype = "float64")
}

infer_dtype <- function(v) {
  if (is.logical(v)) "bool"
  else if (is.integer(v)) "int32"
  else "float64"
}

aperm_to_rowmajor <- function(arr, shape) {
  if (length(shape) <= 1L) return(as.vector(arr))
  d <- dim(arr)
  if (is.null(d)) dim(arr) <- shape
  as.vector(aperm(arr, rev(seq_along(shape))))
}

rowmajor_to_r_array <- function(vec, shape) {
  if (length(shape) == 0L) return(vec[[1L]])
  if (length(shape) == 1L) return(as.vector(vec))
  if (length(shape) == 2L) return(matrix(vec, nrow = shape[[1L]], ncol = shape[[2L]], byrow = TRUE))
  arr_rev <- array(vec, dim = rev(shape))
  aperm(arr_rev, rev(seq_along(shape)))
}

# Broadcast two `pyrt_ndarray` (or scalar) operands following NumPy rules
ndarray_binary_op <- function(a, b, op_fn, dtype = NULL) {
  na <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
  nb <- if (inherits(b, "pyrt_ndarray")) b else py_ndarray(b)

  sa <- na$.shape
  sb <- nb$.shape

  # Fast path: identical shapes or one is length-1 scalar
  if (identical(sa, sb)) {
    res <- op_fn(na$.arr, nb$.arr)
    if (!is.null(dim(na$.arr))) dim(res) <- dim(na$.arr)
    return(py_ndarray_raw(res, sa, dtype))
  }
  if (prod(sa) == 1L) {
    res <- op_fn(as.vector(na$.arr)[[1L]], nb$.arr)
    if (!is.null(dim(nb$.arr))) dim(res) <- dim(nb$.arr)
    return(py_ndarray_raw(res, sb, dtype))
  }
  if (prod(sb) == 1L) {
    res <- op_fn(na$.arr, as.vector(nb$.arr)[[1L]])
    if (!is.null(dim(na$.arr))) dim(res) <- dim(na$.arr)
    return(py_ndarray_raw(res, sa, dtype))
  }

  # 2D matrix + 1D row vector (m, n) op (n,)
  if (length(sa) == 2L && length(sb) == 1L && sa[[2L]] == sb[[1L]]) {
    mat_b <- matrix(rep(as.vector(nb$.arr), each = sa[[1L]]), nrow = sa[[1L]], ncol = sa[[2L]])
    res <- op_fn(na$.arr, mat_b)
    return(py_ndarray_raw(res, sa, dtype))
  }
  if (length(sb) == 2L && length(sa) == 1L && sb[[2L]] == sa[[1L]]) {
    mat_a <- matrix(rep(as.vector(na$.arr), each = sb[[1L]]), nrow = sb[[1L]], ncol = sb[[2L]])
    res <- op_fn(mat_a, nb$.arr)
    return(py_ndarray_raw(res, sb, dtype))
  }
  # 2D matrix + (m, 1) column vector
  if (length(sa) == 2L && length(sb) == 2L && sa[[1L]] == sb[[1L]] && sb[[2L]] == 1L) {
    mat_b <- matrix(rep(as.vector(nb$.arr), times = sa[[2L]]), nrow = sa[[1L]], ncol = sa[[2L]])
    res <- op_fn(na$.arr, mat_b)
    return(py_ndarray_raw(res, sa, dtype))
  }
  if (length(sa) == 2L && length(sb) == 2L && sa[[2L]] == sb[[2L]] && sb[[1L]] == 1L) {
    mat_b <- matrix(rep(as.vector(nb$.arr), each = sa[[1L]]), nrow = sa[[1L]], ncol = sa[[2L]])
    res <- op_fn(na$.arr, mat_b)
    return(py_ndarray_raw(res, sa, dtype))
  }

  # General N-D NumPy broadcasting
  nd <- max(length(sa), length(sb))
  pa <- c(rep(1L, nd - length(sa)), sa)
  pb <- c(rep(1L, nd - length(sb)), sb)
  if (all(pa == pb | pa == 1L | pb == 1L)) {
    out_sh <- pmax(pa, pb)
    arr_a <- array(na$.arr, dim = pa)
    arr_b <- array(nb$.arr, dim = pb)
    idx_a <- lapply(seq_len(nd), function(k) if (pa[[k]] == 1L) rep(1L, out_sh[[k]]) else seq_len(out_sh[[k]]))
    idx_b <- lapply(seq_len(nd), function(k) if (pb[[k]] == 1L) rep(1L, out_sh[[k]]) else seq_len(out_sh[[k]]))
    exp_a <- do.call(`[`, c(list(arr_a), idx_a, list(drop = FALSE)))
    exp_b <- do.call(`[`, c(list(arr_b), idx_b, list(drop = FALSE)))
    res <- op_fn(exp_a, exp_b)
    dim(res) <- out_sh
    return(py_ndarray_raw(res, out_sh, dtype))
  }

  py_raise("ValueError", sprintf(
    "operands could not be broadcast together with shapes (%s) (%s)",
    paste(sa, collapse = ","), paste(sb, collapse = ",")
  ))
}

py_ndarray_raw <- function(arr, shape, dtype = NULL) {
  self <- new.env(parent = emptyenv())
  self$.arr <- arr
  self$.shape <- as.integer(shape)
  self$.dtype <- if (is.null(dtype)) infer_dtype(arr) else dtype
  class(self) <- c("pyrt_ndarray", "pyrt_container")
  self
}

ndarray_reduce <- function(x, fn, axis = NULL, keepdims = FALSE) {
  sh <- x$.shape
  if (is.null(axis) || is_py_none(axis)) {
    val <- fn(as.vector(x$.arr))
    if (isTRUE(keepdims)) {
      new_sh <- rep(1L, max(1L, length(sh)))
      arr_kd <- if (length(new_sh) == 1L) val else array(val, dim = new_sh)
      return(py_ndarray_raw(arr_kd, new_sh))
    }
    return(val)
  }
  axes <- if (inherits(axis, "pyrt_tuple") || inherits(axis, "pyrt_list") || is.list(axis)) {
    as.integer(unlist(coerce_items(axis)))
  } else {
    as.integer(axis)
  }
  axes <- ifelse(axes < 0L, length(sh) + axes, axes)
  if (any(axes < 0L | axes >= length(sh))) {
    py_raise("ValueError", "axis is out of bounds for array")
  }
  r_axes <- unique(axes + 1L)
  keep_axes <- setdiff(seq_along(sh), r_axes)
  if (length(keep_axes) == 0L) {
    val <- fn(as.vector(x$.arr))
    if (isTRUE(keepdims)) {
      new_sh <- rep(1L, length(sh))
      arr_kd <- if (length(new_sh) == 1L) val else array(val, dim = new_sh)
      return(py_ndarray_raw(arr_kd, new_sh))
    }
    return(val)
  }
  res <- apply(x$.arr, keep_axes, fn)
  new_sh <- sh[keep_axes]
  if (isTRUE(keepdims)) {
    full_sh <- sh
    full_sh[r_axes] <- 1L
    dim(res) <- full_sh
    return(py_ndarray_raw(res, full_sh))
  } else if (length(new_sh) > 1L) {
    dim(res) <- new_sh
  }
  py_ndarray_raw(res, new_sh)
}

#' @export
`$.pyrt_ndarray` <- function(x, name) {
  sh <- get(".shape", envir = x, inherits = FALSE)
  arr <- get(".arr", envir = x, inherits = FALSE)
  dt <- get(".dtype", envir = x, inherits = FALSE)

  switch(name,
    shape = py_tuple(as.list(sh)),
    ndim = length(sh),
    size = as.integer(prod(sh)),
    dtype = dt,
    T = {
      if (length(sh) <= 1L) return(x)
      t_arr <- aperm(arr, rev(seq_along(sh)))
      py_ndarray_raw(t_arr, rev(sh), dt)
    },
    reshape = function(...) {
      args <- list(...)
      new_sh <- if (length(args) == 1L && (inherits(args[[1L]], "pyrt_tuple") || inherits(args[[1L]], "pyrt_list") || is.list(args[[1L]]))) {
        as.integer(unlist(coerce_items(args[[1L]])))
      } else {
        as.integer(unlist(args))
      }
      neg_idx <- which(new_sh < 0L)
      if (length(neg_idx) == 1L) {
        known_prod <- prod(new_sh[-neg_idx])
        new_sh[[neg_idx]] <- as.integer(prod(sh) %/% known_prod)
      }
      if (prod(new_sh) != prod(sh)) {
        py_raise("ValueError", sprintf("cannot reshape array of size %d into shape (%s)", as.integer(prod(sh)), paste(new_sh, collapse = ",")))
      }
      rm_vec <- aperm_to_rowmajor(arr, sh)
      new_arr <- rowmajor_to_r_array(rm_vec, new_sh)
      py_ndarray_raw(new_arr, new_sh, dt)
    },
    flatten = function() {
      rm_vec <- aperm_to_rowmajor(arr, sh)
      py_ndarray_raw(rm_vec, length(rm_vec), dt)
    },
    ravel = function() {
      rm_vec <- aperm_to_rowmajor(arr, sh)
      py_ndarray_raw(rm_vec, length(rm_vec), dt)
    },
    copy = function() py_ndarray_raw(arr, sh, dt),
    astype = function(new_dtype) py_ndarray(x, dtype = new_dtype),
    tolist = function() {
      if (length(sh) == 1L) {
        return(py_list(as.list(as.vector(arr))))
      }
      if (length(sh) == 2L) {
        rows <- lapply(seq_len(sh[[1L]]), function(i) py_list(as.list(arr[i, ])))
        return(py_list(rows))
      }
      py_list(as.list(aperm_to_rowmajor(arr, sh)))
    },
    as_r_array = function() arr,
    sum = function(axis = NULL, keepdims = FALSE) ndarray_reduce(x, base::sum, axis, keepdims),
    mean = function(axis = NULL, keepdims = FALSE) ndarray_reduce(x, base::mean, axis, keepdims),
    std = function(axis = NULL, ddof = 0, keepdims = FALSE) {
      d <- as.numeric(ddof)
      fn <- function(v) sqrt(sum((v - mean(v))^2) / (length(v) - d))
      ndarray_reduce(x, fn, axis, keepdims)
    },
    var = function(axis = NULL, ddof = 0, keepdims = FALSE) {
      d <- as.numeric(ddof)
      fn <- function(v) sum((v - mean(v))^2) / (length(v) - d)
      ndarray_reduce(x, fn, axis, keepdims)
    },
    min = function(axis = NULL, keepdims = FALSE) ndarray_reduce(x, base::min, axis, keepdims),
    max = function(axis = NULL, keepdims = FALSE) ndarray_reduce(x, base::max, axis, keepdims),
    argmin = function(axis = NULL) {
      fn <- function(v) as.integer(which.min(v) - 1L)
      if (is.null(axis) || is_py_none(axis)) return(fn(aperm_to_rowmajor(arr, sh)))
      ndarray_reduce(x, fn, axis, FALSE)
    },
    argmax = function(axis = NULL) {
      fn <- function(v) as.integer(which.max(v) - 1L)
      if (is.null(axis) || is_py_none(axis)) return(fn(aperm_to_rowmajor(arr, sh)))
      ndarray_reduce(x, fn, axis, FALSE)
    },
    cumsum = function(axis = NULL) {
      if (is.null(axis) || is_py_none(axis)) {
        v <- cumsum(aperm_to_rowmajor(arr, sh))
        return(py_ndarray_raw(v, length(v)))
      }
      ndarray_reduce(x, base::cumsum, axis, FALSE)
    },
    all = function(axis = NULL) ndarray_reduce(x, function(v) all(as.logical(v)), axis, FALSE),
    any = function(axis = NULL) ndarray_reduce(x, function(v) any(as.logical(v)), axis, FALSE),
    dot = function(other) np_dot(x, other),
    `__matmul__` = function(other) np_dot(x, other),
    `__len__` = function() sh[[1L]],
    `__getitem__` = function(key) ndarray_getitem(x, key),
    `__setitem__` = function(key, value) ndarray_setitem(x, key, value),
    `__add__` = function(other) ndarray_binary_op(x, other, `+`),
    `__radd__` = function(other) ndarray_binary_op(other, x, `+`),
    `__sub__` = function(other) ndarray_binary_op(x, other, `-`),
    `__rsub__` = function(other) ndarray_binary_op(other, x, `-`),
    `__mul__` = function(other) ndarray_binary_op(x, other, `*`),
    `__rmul__` = function(other) ndarray_binary_op(other, x, `*`),
    `__truediv__` = function(other) ndarray_binary_op(x, other, `/`, dtype = "float64"),
    `__rtruediv__` = function(other) ndarray_binary_op(other, x, `/`, dtype = "float64"),
    `__floordiv__` = function(other) ndarray_binary_op(x, other, `%/%`),
    `__mod__` = function(other) ndarray_binary_op(x, other, `%%`),
    `__pow__` = function(other) ndarray_binary_op(x, other, `^`),
    `__neg__` = function() py_ndarray_raw(-arr, sh, dt),
    `__pos__` = function() py_ndarray_raw(+arr, sh, dt),
    `__abs__` = function() py_ndarray_raw(abs(arr), sh, dt),
    `__eq__` = function(other) ndarray_binary_op(x, other, `==`, dtype = "bool"),
    `__ne__` = function(other) ndarray_binary_op(x, other, `!=`, dtype = "bool"),
    `__lt__` = function(other) ndarray_binary_op(x, other, `<`, dtype = "bool"),
    `__le__` = function(other) ndarray_binary_op(x, other, `<=`, dtype = "bool"),
    `__gt__` = function(other) ndarray_binary_op(x, other, `>`, dtype = "bool"),
    `__ge__` = function(other) ndarray_binary_op(x, other, `>=`, dtype = "bool"),
    get0(name, envir = x, inherits = FALSE)
  )
}

resolve_axis_index <- function(idx, dim_len) {
  if (inherits(idx, "pyrt_slice")) {
    return(slice_r_positions(idx, dim_len))
  }
  if (inherits(idx, "pyrt_ndarray")) {
    if (identical(idx$.dtype, "bool") || is.logical(idx$.arr)) {
      return(which(as.vector(idx$.arr)))
    }
    vals <- as.integer(idx$.arr)
    return(ifelse(vals < 0L, dim_len + vals + 1L, vals + 1L))
  }
  if (inherits(idx, "pyrt_list")) {
    vals <- as.integer(unlist(idx$as_r_list()))
    return(ifelse(vals < 0L, dim_len + vals + 1L, vals + 1L))
  }
  i <- as.integer(idx)
  if (i < 0L) i <- dim_len + i
  if (i < 0L || i >= dim_len) py_raise("IndexError", "index is out of bounds")
  i + 1L
}

ndarray_getitem <- function(x, key) {
  sh <- x$.shape
  arr <- x$.arr
  dt <- x$.dtype

  # Boolean mask indexing
  if (inherits(key, "pyrt_ndarray") && identical(key$.dtype, "bool")) {
    sel <- arr[as.logical(key$.arr)]
    return(py_ndarray_raw(as.vector(sel), length(sel), dt))
  }

  if (inherits(key, "pyrt_tuple") && length(sh) >= 2L) {
    k_items <- key$items
    idx1 <- resolve_axis_index(k_items[[1L]], sh[[1L]])
    idx2 <- if (length(k_items) >= 2L) resolve_axis_index(k_items[[2L]], sh[[2L]]) else seq_len(sh[[2L]])
    sub <- arr[idx1, idx2, drop = FALSE]
    is_scalar1 <- !inherits(k_items[[1L]], "pyrt_slice") && length(idx1) == 1L && !inherits(k_items[[1L]], "pyrt_list") && !inherits(k_items[[1L]], "pyrt_ndarray")
    is_scalar2 <- length(k_items) >= 2L && !inherits(k_items[[2L]], "pyrt_slice") && length(idx2) == 1L && !inherits(k_items[[2L]], "pyrt_list") && !inherits(k_items[[2L]], "pyrt_ndarray")
    if (is_scalar1 && is_scalar2) {
      return(as.vector(sub)[[1L]])
    }
    if (is_scalar1) {
      v <- as.vector(sub)
      return(py_ndarray_raw(v, length(v), dt))
    }
    if (is_scalar2) {
      v <- as.vector(sub)
      return(py_ndarray_raw(v, length(v), dt))
    }
    return(py_ndarray_raw(sub, dim(sub), dt))
  }

  idx1 <- resolve_axis_index(key, sh[[1L]])
  is_single <- !inherits(key, "pyrt_slice") && !inherits(key, "pyrt_list") && !inherits(key, "pyrt_ndarray") && length(idx1) == 1L
  if (length(sh) == 1L) {
    sub <- arr[idx1]
    if (is_single) return(sub[[1L]])
    return(py_ndarray_raw(as.vector(sub), length(sub), dt))
  }
  if (length(sh) == 2L) {
    sub <- arr[idx1, , drop = FALSE]
    if (is_single) {
      v <- as.vector(sub)
      return(py_ndarray_raw(v, sh[[2L]], dt))
    }
    return(py_ndarray_raw(sub, dim(sub), dt))
  }
  sub <- arr[idx1]
  py_ndarray_raw(as.vector(sub), length(sub), dt)
}

ndarray_setitem <- function(x, key, value) {
  sh <- x$.shape
  val <- if (inherits(value, "pyrt_ndarray")) value$.arr else value
  if (inherits(key, "pyrt_ndarray") && identical(key$.dtype, "bool")) {
    x$.arr[as.logical(key$.arr)] <- val
    return(invisible(x))
  }
  if (inherits(key, "pyrt_tuple") && length(sh) == 2L) {
    k_items <- key$items
    idx1 <- resolve_axis_index(k_items[[1L]], sh[[1L]])
    idx2 <- if (length(k_items) >= 2L) resolve_axis_index(k_items[[2L]], sh[[2L]]) else seq_len(sh[[2L]])
    x$.arr[idx1, idx2] <- val
    return(invisible(x))
  }
  idx1 <- resolve_axis_index(key, sh[[1L]])
  if (length(sh) == 1L) {
    x$.arr[idx1] <- val
  } else if (length(sh) == 2L) {
    x$.arr[idx1, ] <- val
  }
  invisible(x)
}

np_dot <- function(a, b) {
  na <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
  nb <- if (inherits(b, "pyrt_ndarray")) b else py_ndarray(b)
  sa <- na$.shape
  sb <- nb$.shape
  if (length(sa) == 1L && length(sb) == 1L) {
    return(sum(na$.arr * nb$.arr))
  }
  if (length(sa) == 2L && length(sb) == 1L) {
    v <- as.vector(na$.arr %*% nb$.arr)
    return(py_ndarray_raw(v, length(v), "float64"))
  }
  if (length(sa) == 1L && length(sb) == 2L) {
    v <- as.vector(na$.arr %*% nb$.arr)
    return(py_ndarray_raw(v, length(v), "float64"))
  }
  res <- na$.arr %*% nb$.arr
  py_ndarray_raw(res, dim(res), "float64")
}

unary_ufunc <- function(r_fn) {
  function(x) {
    if (inherits(x, "pyrt_ndarray")) {
      res <- r_fn(x$.arr)
      if (!is.null(dim(x$.arr))) dim(res) <- dim(x$.arr)
      return(py_ndarray_raw(res, x$.shape))
    }
    if (inherits(x, "pyrt_list") || inherits(x, "pyrt_tuple")) {
      nd <- py_ndarray(x)
      res <- r_fn(nd$.arr)
      if (!is.null(dim(nd$.arr))) dim(res) <- dim(nd$.arr)
      return(py_ndarray_raw(res, nd$.shape))
    }
    r_fn(as.numeric(x))
  }
}

parse_shape_arg <- function(shape) {
  if (inherits(shape, "pyrt_tuple") || inherits(shape, "pyrt_list") || is.list(shape)) {
    as.integer(unlist(coerce_items(shape)))
  } else {
    as.integer(shape)
  }
}

make_filled_ndarray <- function(shape, fill_val, dtype = "float64") {
  sh <- parse_shape_arg(shape)
  n <- as.integer(prod(sh))
  vec <- rep(fill_val, n)
  arr <- if (length(sh) <= 1L) vec else array(vec, dim = sh)
  py_ndarray(arr, dtype = dtype)
}

# ---- 2. `numpy` module builder ----------------------------------------------

build_numpy_module <- function() {
  linalg_mod <- make_mod_env(
    norm = function(x, ord = NULL) {
      nd <- if (inherits(x, "pyrt_ndarray")) x else py_ndarray(x)
      sqrt(sum(as.numeric(nd$.arr)^2))
    },
    inv = function(a) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      res <- solve(nd$.arr)
      py_ndarray_raw(res, dim(res), "float64")
    },
    solve = function(a, b) {
      na <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      nb <- if (inherits(b, "pyrt_ndarray")) b else py_ndarray(b)
      res <- solve(na$.arr, nb$.arr)
      sh <- if (is.null(dim(res))) length(res) else dim(res)
      py_ndarray_raw(res, sh, "float64")
    },
    det = function(a) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      as.double(det(nd$.arr))
    },
    eig = function(a) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      eg <- eigen(nd$.arr)
      py_tuple(list(
        py_ndarray_raw(eg$values, length(eg$values)),
        py_ndarray_raw(eg$vectors, dim(eg$vectors))
      ))
    },
    svd = function(a, full_matrices = TRUE) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      s <- svd(nd$.arr)
      py_tuple(list(
        py_ndarray_raw(s$u, dim(s$u)),
        py_ndarray_raw(s$d, length(s$d)),
        py_ndarray_raw(t(s$v), dim(t(s$v)))
      ))
    },
    qr = function(a) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      q <- qr(nd$.arr)
      Q <- qr.Q(q)
      R <- qr.R(q)
      py_tuple(list(py_ndarray_raw(Q, dim(Q)), py_ndarray_raw(R, dim(R))))
    },
    cholesky = function(a) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      L <- t(chol(nd$.arr))
      py_ndarray_raw(L, dim(L))
    },
    lstsq = function(a, b, rcond = NULL) {
      na <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      nb <- if (inherits(b, "pyrt_ndarray")) b else py_ndarray(b)
      fit <- qr.coef(qr(na$.arr), nb$.arr)
      py_tuple(list(
        py_ndarray_raw(as.vector(fit), length(fit)),
        py_ndarray(numeric(0)),
        ncol(na$.arr),
        py_ndarray(svd(na$.arr)$d)
      ))
    }
  )

  fft_mod <- make_mod_env(
    fft = function(a) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      res <- stats::fft(as.vector(nd$.arr))
      py_ndarray_raw(res, length(res), "complex128")
    },
    ifft = function(a) {
      nd <- if (inherits(a, "pyrt_ndarray")) a else py_ndarray(a)
      v <- as.vector(nd$.arr)
      res <- stats::fft(v, inverse = TRUE) / length(v)
      py_ndarray_raw(res, length(res), "complex128")
    }
  )

  make_mod_env(
    ndarray = py_ndarray,
    array = function(object, dtype = NULL) py_ndarray(object, dtype = dtype),
    asarray = function(a, dtype = NULL) py_ndarray(a, dtype = dtype),
    zeros = function(shape, dtype = "float64") make_filled_ndarray(shape, 0.0, dtype),
    ones = function(shape, dtype = "float64") make_filled_ndarray(shape, 1.0, dtype),
    full = function(shape, fill_value, dtype = NULL) make_filled_ndarray(shape, fill_value, dtype),
    empty = function(shape, dtype = "float64") make_filled_ndarray(shape, 0.0, dtype),
    zeros_like = function(a, dtype = NULL) {
      nd <- py_ndarray(a)
      make_filled_ndarray(nd$.shape, 0.0, if (is.null(dtype)) nd$.dtype else dtype)
    },
    ones_like = function(a, dtype = NULL) {
      nd <- py_ndarray(a)
      make_filled_ndarray(nd$.shape, 1.0, if (is.null(dtype)) nd$.dtype else dtype)
    },
    eye = function(N, M = NULL, dtype = "float64") {
      n <- as.integer(N)
      m <- if (is.null(M) || is_py_none(M)) n else as.integer(M)
      mat <- diag(1.0, nrow = n, ncol = m)
      py_ndarray(mat, dtype = dtype)
    },
    identity = function(n, dtype = "float64") {
      ni <- as.integer(n)
      py_ndarray(diag(1.0, nrow = ni, ncol = ni), dtype = dtype)
    },
    diag = function(v) {
      nd <- py_ndarray(v)
      if (length(nd$.shape) == 1L) {
        mat <- diag(as.vector(nd$.arr))
        return(py_ndarray_raw(mat, dim(mat), nd$.dtype))
      }
      d <- diag(nd$.arr)
      py_ndarray_raw(d, length(d), nd$.dtype)
    },
    arange = function(start, stop = NULL, step = 1, dtype = NULL) {
      if (is.null(stop) || is_py_none(stop)) {
        s0 <- 0
        s1 <- as.numeric(start)
      } else {
        s0 <- as.numeric(start)
        s1 <- as.numeric(stop)
      }
      st <- as.numeric(step)
      if ((st > 0 && s0 >= s1) || (st < 0 && s0 <= s1)) {
        return(py_ndarray(numeric(0), dtype = dtype))
      }
      n <- ceiling((s1 - s0) / st)
      vals <- s0 + seq(0, n - 1) * st
      if (is.null(dtype) && all(vals == floor(vals))) {
        vals <- as.integer(vals)
      }
      py_ndarray(vals, dtype = dtype)
    },
    linspace = function(start, stop, num = 50L, endpoint = TRUE) {
      n <- as.integer(num)
      if (n <= 0L) return(py_ndarray(numeric(0)))
      if (n == 1L) return(py_ndarray(as.double(start)))
      vals <- if (isTRUE(endpoint)) {
        seq(as.double(start), as.double(stop), length.out = n)
      } else {
        s0 <- as.double(start)
        s1 <- as.double(stop)
        s0 + (s1 - s0) * (0:(n - 1L)) / n
      }
      py_ndarray(vals, dtype = "float64")
    },
    reshape = function(a, newshape) py_getattr(py_ndarray(a), "reshape")(newshape),
    transpose = function(a) py_getattr(py_ndarray(a), "T"),
    concatenate = function(arrays, axis = 0L) {
      nds <- lapply(coerce_items(arrays), py_ndarray)
      ax <- as.integer(axis)
      if (length(nds[[1L]]$.shape) == 1L || ax == 0L && length(nds[[1L]]$.shape) == 1L) {
        vec <- unlist(lapply(nds, function(x) as.vector(x$.arr)))
        return(py_ndarray_raw(vec, length(vec)))
      }
      if (ax == 0L) {
        mat <- do.call(rbind, lapply(nds, `[[`, ".arr"))
        return(py_ndarray_raw(mat, dim(mat)))
      }
      mat <- do.call(cbind, lapply(nds, `[[`, ".arr"))
      py_ndarray_raw(mat, dim(mat))
    },
    vstack = function(tup) {
      nds <- lapply(coerce_items(tup), function(x) {
        nd <- py_ndarray(x)
        if (length(nd$.shape) == 1L) matrix(nd$.arr, nrow = 1L) else nd$.arr
      })
      mat <- do.call(rbind, nds)
      py_ndarray_raw(mat, dim(mat))
    },
    hstack = function(tup) {
      nds <- lapply(coerce_items(tup), py_ndarray)
      if (all(vapply(nds, function(x) length(x$.shape) == 1L, logical(1)))) {
        vec <- unlist(lapply(nds, function(x) as.vector(x$.arr)))
        return(py_ndarray_raw(vec, length(vec)))
      }
      mat <- do.call(cbind, lapply(nds, `[[`, ".arr"))
      py_ndarray_raw(mat, dim(mat))
    },
    where = function(condition, x = NULL, y = NULL) {
      cond_nd <- py_ndarray(condition, dtype = "bool")
      if (is.null(x) && is.null(y)) {
        idx <- which(as.logical(cond_nd$.arr)) - 1L
        return(py_tuple(list(py_ndarray(as.integer(idx)))))
      }
      nx <- if (inherits(x, "pyrt_ndarray")) x$.arr else x
      ny <- if (inherits(y, "pyrt_ndarray")) y$.arr else y
      res <- ifelse(as.logical(cond_nd$.arr), nx, ny)
      if (!is.null(dim(cond_nd$.arr))) dim(res) <- dim(cond_nd$.arr)
      py_ndarray_raw(res, cond_nd$.shape)
    },
    clip = function(a, a_min, a_max) {
      nd <- py_ndarray(a)
      res <- pmin(pmax(nd$.arr, as.numeric(a_min)), as.numeric(a_max))
      if (!is.null(dim(nd$.arr))) dim(res) <- dim(nd$.arr)
      py_ndarray_raw(res, nd$.shape)
    },
    dot = np_dot,
    matmul = np_dot,
    outer = function(a, b) {
      na <- py_ndarray(a)
      nb <- py_ndarray(b)
      mat <- outer(as.vector(na$.arr), as.vector(nb$.arr))
      py_ndarray_raw(mat, dim(mat))
    },
    inner = function(a, b) {
      na <- py_ndarray(a)
      nb <- py_ndarray(b)
      sum(as.vector(na$.arr) * as.vector(nb$.arr))
    },
    einsum = function(subscripts, ...) {
      sub <- gsub("\\s+", "", as.character(subscripts))
      ops <- lapply(list(...), py_ndarray)
      if (sub %in% c("ij->ji", "ji")) {
        return(py_getattr(ops[[1L]], "T"))
      }
      if (sub %in% c("ii->", "ii")) {
        return(sum(diag(ops[[1L]]$.arr)))
      }
      if (sub == "ii->i") {
        d <- diag(ops[[1L]]$.arr)
        return(py_ndarray_raw(d, length(d)))
      }
      if (sub %in% c("i->", "ij->", "ijk->")) {
        return(sum(ops[[1L]]$.arr))
      }
      if (sub == "ij->i") {
        v <- rowSums(ops[[1L]]$.arr)
        return(py_ndarray_raw(v, length(v)))
      }
      if (sub == "ij->j") {
        v <- colSums(ops[[1L]]$.arr)
        return(py_ndarray_raw(v, length(v)))
      }
      if (sub %in% c("i,i->", "i,i", "ij,ij->", "ij,ij")) {
        return(sum(ops[[1L]]$.arr * ops[[2L]]$.arr))
      }
      if (sub %in% c("i,i->i", "ij,ij->ij")) {
        return(ndarray_binary_op(ops[[1L]], ops[[2L]], `*`))
      }
      if (sub == "i,j->ij") {
        mat <- outer(as.vector(ops[[1L]]$.arr), as.vector(ops[[2L]]$.arr))
        return(py_ndarray_raw(mat, dim(mat)))
      }
      if (sub == "ij,j->i") {
        v <- as.vector(ops[[1L]]$.arr %*% as.vector(ops[[2L]]$.arr))
        return(py_ndarray_raw(v, length(v)))
      }
      if (sub == "i,ij->j") {
        v <- as.vector(as.vector(ops[[1L]]$.arr) %*% ops[[2L]]$.arr)
        return(py_ndarray_raw(v, length(v)))
      }
      if (sub %in% c("ij,jk->ik", "ij,jk")) {
        res <- ops[[1L]]$.arr %*% ops[[2L]]$.arr
        return(py_ndarray_raw(res, dim(res)))
      }
      if (sub == "ij,kj->ik") {
        res <- ops[[1L]]$.arr %*% t(ops[[2L]]$.arr)
        return(py_ndarray_raw(res, dim(res)))
      }
      if (sub == "ji,jk->ik") {
        res <- t(ops[[1L]]$.arr) %*% ops[[2L]]$.arr
        return(py_ndarray_raw(res, dim(res)))
      }
      if (sub == "bij,bjk->bik") {
        A <- ops[[1L]]$.arr
        B <- ops[[2L]]$.arr
        b_dim <- dim(A)[[1L]]
        i_dim <- dim(A)[[2L]]
        k_dim <- dim(B)[[3L]]
        out <- array(0.0, dim = c(b_dim, i_dim, k_dim))
        for (b in seq_len(b_dim)) {
          out[b, , ] <- matrix(A[b, , ], nrow = i_dim) %*% matrix(B[b, , ], ncol = k_dim)
        }
        return(py_ndarray_raw(out, c(b_dim, i_dim, k_dim)))
      }
      py_raise("ValueError", sprintf("Unsupported einsum subscripts: '%s'", subscripts))
    },
    # Reductions
    sum = function(a, axis = NULL, keepdims = FALSE) py_getattr(py_ndarray(a), "sum")(axis, keepdims),
    mean = function(a, axis = NULL, keepdims = FALSE) py_getattr(py_ndarray(a), "mean")(axis, keepdims),
    std = function(a, axis = NULL, ddof = 0, keepdims = FALSE) py_getattr(py_ndarray(a), "std")(axis, ddof, keepdims),
    var = function(a, axis = NULL, ddof = 0, keepdims = FALSE) py_getattr(py_ndarray(a), "var")(axis, ddof, keepdims),
    min = function(a, axis = NULL, keepdims = FALSE) py_getattr(py_ndarray(a), "min")(axis, keepdims),
    amin = function(a, axis = NULL, keepdims = FALSE) py_getattr(py_ndarray(a), "min")(axis, keepdims),
    max = function(a, axis = NULL, keepdims = FALSE) py_getattr(py_ndarray(a), "max")(axis, keepdims),
    amax = function(a, axis = NULL, keepdims = FALSE) py_getattr(py_ndarray(a), "max")(axis, keepdims),
    argmin = function(a, axis = NULL) py_getattr(py_ndarray(a), "argmin")(axis),
    argmax = function(a, axis = NULL) py_getattr(py_ndarray(a), "argmax")(axis),
    cumsum = function(a, axis = NULL) py_getattr(py_ndarray(a), "cumsum")(axis),
    median = function(a, axis = NULL) ndarray_reduce(py_ndarray(a), stats::median, axis, FALSE),
    percentile = function(a, q, axis = NULL) {
      nd <- py_ndarray(a)
      as.double(stats::quantile(as.vector(nd$.arr), probs = as.numeric(q) / 100.0, names = FALSE))
    },
    all = function(a, axis = NULL) py_getattr(py_ndarray(a), "all")(axis),
    any = function(a, axis = NULL) py_getattr(py_ndarray(a), "any")(axis),
    allclose = function(a, b, rtol = 1e-05, atol = 1e-08) {
      na <- as.numeric(py_ndarray(a)$.arr)
      nb <- as.numeric(py_ndarray(b)$.arr)
      all(abs(na - nb) <= (atol + rtol * abs(nb)))
    },
    isclose = function(a, b, rtol = 1e-05, atol = 1e-08) {
      na <- py_ndarray(a)
      nb <- py_ndarray(b)
      res <- abs(na$.arr - nb$.arr) <= (atol + rtol * abs(nb$.arr))
      py_ndarray_raw(res, na$.shape, "bool")
    },
    diff = function(a, n = 1L) {
      nd <- py_ndarray(a)
      v <- diff(as.vector(nd$.arr), differences = as.integer(n))
      py_ndarray_raw(v, length(v))
    },
    unique = function(ar) {
      nd <- py_ndarray(ar)
      v <- sort(unique(as.vector(nd$.arr)))
      py_ndarray_raw(v, length(v), nd$.dtype)
    },
    sort = function(a) {
      nd <- py_ndarray(a)
      v <- sort(as.vector(nd$.arr))
      py_ndarray_raw(v, length(v), nd$.dtype)
    },
    argsort = function(a) {
      nd <- py_ndarray(a)
      idx <- order(as.vector(nd$.arr)) - 1L
      py_ndarray_raw(as.integer(idx), length(idx), "int32")
    },
    corrcoef = function(x, y = NULL) {
      nx <- py_ndarray(x)
      if (!is.null(y)) {
        ny <- py_ndarray(y)
        mat <- stats::cor(cbind(as.vector(nx$.arr), as.vector(ny$.arr)))
      } else {
        mat <- stats::cor(t(nx$.arr))
      }
      py_ndarray_raw(mat, dim(mat))
    },
    cov = function(m, y = NULL, ddof = 1L) {
      nx <- py_ndarray(m)
      if (length(nx$.shape) == 1L && is.null(y)) {
        v <- as.vector(nx$.arr)
        return(sum((v - mean(v))^2) / (length(v) - as.numeric(ddof)))
      }
      mat <- stats::cov(t(nx$.arr))
      py_ndarray_raw(mat, dim(mat))
    },
    polyfit = function(x, y, deg) {
      xv <- as.numeric(py_ndarray(x)$.arr)
      yv <- as.numeric(py_ndarray(y)$.arr)
      d <- as.integer(deg)
      powers <- outer(xv, rev(0:d), `^`)
      coefs <- as.vector(qr.solve(powers, yv))
      py_ndarray_raw(coefs, length(coefs))
    },
    polyval = function(p, x) {
      pv <- as.numeric(py_ndarray(p)$.arr)
      xv <- as.numeric(py_ndarray(x)$.arr)
      d <- length(pv) - 1L
      powers <- outer(xv, rev(0:d), `^`)
      vals <- as.vector(powers %*% pv)
      if (length(vals) == 1L && !inherits(x, "pyrt_ndarray")) return(vals[[1L]])
      py_ndarray_raw(vals, length(vals))
    },
    # Universal functions (ufuncs)
    sqrt = unary_ufunc(base::sqrt),
    exp = unary_ufunc(base::exp),
    expm1 = unary_ufunc(base::expm1),
    log = unary_ufunc(base::log),
    log2 = unary_ufunc(base::log2),
    log10 = unary_ufunc(base::log10),
    log1p = unary_ufunc(base::log1p),
    sin = unary_ufunc(base::sin),
    cos = unary_ufunc(base::cos),
    tan = unary_ufunc(base::tan),
    arcsin = unary_ufunc(base::asin),
    arccos = unary_ufunc(base::acos),
    arctan = unary_ufunc(base::atan),
    sinh = unary_ufunc(base::sinh),
    cosh = unary_ufunc(base::cosh),
    tanh = unary_ufunc(base::tanh),
    abs = unary_ufunc(base::abs),
    fabs = unary_ufunc(base::abs),
    floor = unary_ufunc(base::floor),
    ceil = unary_ufunc(base::ceiling),
    round = function(a, decimals = 0L) {
      nd <- py_ndarray(a)
      res <- round(nd$.arr, digits = as.integer(decimals))
      if (!is.null(dim(nd$.arr))) dim(res) <- dim(nd$.arr)
      py_ndarray_raw(res, nd$.shape)
    },
    sign = unary_ufunc(base::sign),
    isnan = unary_ufunc(base::is.nan),
    isinf = unary_ufunc(base::is.infinite),
    isfinite = unary_ufunc(base::is.finite),
    maximum = function(x1, x2) ndarray_binary_op(x1, x2, pmax),
    minimum = function(x1, x2) ndarray_binary_op(x1, x2, pmin),
    add = function(x1, x2) ndarray_binary_op(x1, x2, `+`),
    subtract = function(x1, x2) ndarray_binary_op(x1, x2, `-`),
    multiply = function(x1, x2) ndarray_binary_op(x1, x2, `*`),
    divide = function(x1, x2) ndarray_binary_op(x1, x2, `/`, dtype = "float64"),
    power = function(x1, x2) ndarray_binary_op(x1, x2, `^`),
    # Constants & submodules
    pi = base::pi,
    e = base::exp(1),
    inf = Inf,
    nan = NaN,
    float64 = "float64",
    int32 = "int32",
    int64 = "int64",
    bool_ = "bool",
    linalg = linalg_mod,
    fft = fft_mod,
    random = build_numpy_random_module()
  )
}

build_numpy_random_module <- function() {
  shape_from_size <- function(size) {
    if (is.null(size) || is_py_none(size)) return(NULL)
    parse_shape_arg(size)
  }

  wrap_sized <- function(vals, sh, dtype = "float64") {
    if (is.null(sh)) return(vals[[1L]])
    arr <- rowmajor_to_r_array(vals, sh)
    py_ndarray_raw(arr, sh, dtype)
  }

  make_random_state <- function(seed = NULL) {
    ptr <- .Call(ffi_mt_new)
    do_seed <- function(s = NULL) {
      if (is.null(s) || is_py_none(s)) {
        .Call(ffi_mt_seed_uint32, ptr, as.double(as.integer(Sys.time())))
      } else {
        .Call(ffi_mt_seed_uint32, ptr, as.double(s))
      }
      invisible(py_none())
    }
    do_seed(seed)

    rs <- make_mod_env(
      seed = do_seed,
      random = function(size = NULL) {
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        vals <- .Call(ffi_mt_random, ptr, n)
        wrap_sized(vals, sh, "float64")
      },
      random_sample = function(size = NULL) rs$random(size),
      rand = function(...) {
        dims <- as.integer(unlist(list(...)))
        if (length(dims) == 0L) return(.Call(ffi_mt_random, ptr, 1L)[[1L]])
        vals <- .Call(ffi_mt_random, ptr, as.integer(prod(dims)))
        wrap_sized(vals, dims, "float64")
      },
      randn = function(...) {
        dims <- as.integer(unlist(list(...)))
        if (length(dims) == 0L) return(.Call(ffi_mt_np_randn, ptr, 1L, 0.0, 1.0)[[1L]])
        vals <- .Call(ffi_mt_np_randn, ptr, as.integer(prod(dims)), 0.0, 1.0)
        wrap_sized(vals, dims, "float64")
      },
      uniform = function(low = 0.0, high = 1.0, size = NULL) {
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        lo <- as.double(low)
        hi <- as.double(high)
        vals <- lo + (hi - lo) * .Call(ffi_mt_random, ptr, n)
        wrap_sized(vals, sh, "float64")
      },
      normal = function(loc = 0.0, scale = 1.0, size = NULL) {
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        vals <- .Call(ffi_mt_np_randn, ptr, n, as.double(loc), as.double(scale))
        wrap_sized(vals, sh, "float64")
      },
      standard_normal = function(size = NULL) {
        rs$normal(0.0, 1.0, size = size)
      },
      randint = function(low, high = NULL, size = NULL) {
        if (is.null(high) || is_py_none(high)) {
          lo <- 0L
          hi <- as.integer(low)
        } else {
          lo <- as.integer(low)
          hi <- as.integer(high)
        }
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        vals <- .Call(ffi_mt_np_randint, ptr, lo, hi, n)
        wrap_sized(vals, sh, "int32")
      },
      integers = function(low, high = NULL, size = NULL) {
        rs$randint(low, high = high, size = size)
      },
      choice = function(a, size = NULL, replace = TRUE) {
        pool <- if (is_int_like(a) && length(a) == 1L) seq(0L, as.integer(a) - 1L) else as.vector(py_ndarray(a)$.arr)
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        idx <- .Call(ffi_mt_np_randint, ptr, 0L, length(pool), n) + 1L
        vals <- pool[idx]
        wrap_sized(vals, sh, infer_dtype(vals))
      },
      permutation = function(x) {
        arr <- if (is_int_like(x) && length(x) == 1L) seq(0L, as.integer(x) - 1L) else as.vector(py_ndarray(x)$.arr)
        n <- length(arr)
        if (n <= 1L) return(py_ndarray(arr))
        for (i in seq(n, 2L, by = -1L)) {
          j <- .Call(ffi_mt_np_randint, ptr, 0L, i, 1L)[[1L]] + 1L
          tmp <- arr[[i]]
          arr[[i]] <- arr[[j]]
          arr[[j]] <- tmp
        }
        py_ndarray(arr)
      },
      shuffle = function(x) {
        if (inherits(x, "pyrt_ndarray")) {
          p <- rs$permutation(x$.arr)
          x$.arr <- p$.arr
        } else if (inherits(x, "pyrt_list")) {
          p <- rs$permutation(seq_along(x$.items) - 1L)
          x$.items <- x$.items[as.integer(p$.arr) + 1L]
        }
        invisible(py_none())
      }
    )
    rs
  }

  # Modern NumPy `default_rng(seed)` backed by bit-exact C `PCG64` + `SeedSequence`
  make_pcg64_generator <- function(seed = NULL) {
    ptr <- .Call(ffi_pcg64_new)
    if (is.null(seed) || is_py_none(seed)) {
      .Call(ffi_pcg64_seed, ptr, as.double(as.integer(Sys.time())))
    } else {
      .Call(ffi_pcg64_seed, ptr, as.double(seed))
    }

    gen <- make_mod_env(
      random = function(size = NULL) {
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        vals <- .Call(ffi_pcg64_random, ptr, n)
        wrap_sized(vals, sh, "float64")
      },
      uniform = function(low = 0.0, high = 1.0, size = NULL) {
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        lo <- as.double(low)
        hi <- as.double(high)
        vals <- lo + (hi - lo) * .Call(ffi_pcg64_random, ptr, n)
        wrap_sized(vals, sh, "float64")
      },
      integers = function(low, high = NULL, size = NULL, endpoint = FALSE) {
        if (is.null(high) || is_py_none(high)) {
          lo <- 0L
          hi <- as.integer(low)
        } else {
          lo <- as.integer(low)
          hi <- as.integer(high)
        }
        if (isTRUE(endpoint)) hi <- hi + 1L
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        vals <- .Call(ffi_pcg64_integers, ptr, lo, hi, n)
        wrap_sized(vals, sh, "int32")
      },
      standard_normal = function(size = NULL) {
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        u1 <- pmax(.Call(ffi_pcg64_random, ptr, n), 1e-15)
        u2 <- .Call(ffi_pcg64_random, ptr, n)
        vals <- sqrt(-2.0 * log(u1)) * cos(2.0 * pi * u2)
        wrap_sized(vals, sh, "float64")
      },
      normal = function(loc = 0.0, scale = 1.0, size = NULL) {
        sn <- gen$standard_normal(size = size)
        if (inherits(sn, "pyrt_ndarray")) {
          return(py_ndarray_raw(as.double(loc) + as.double(scale) * sn$.arr, sn$.shape, "float64"))
        }
        as.double(loc) + as.double(scale) * sn
      },
      choice = function(a, size = NULL, replace = TRUE) {
        pool <- if (is_int_like(a) && length(a) == 1L) seq(0L, as.integer(a) - 1L) else as.vector(py_ndarray(a)$.arr)
        sh <- shape_from_size(size)
        n <- if (is.null(sh)) 1L else as.integer(prod(sh))
        idx <- .Call(ffi_pcg64_integers, ptr, 0L, length(pool), n) + 1L
        vals <- pool[idx]
        wrap_sized(vals, sh, infer_dtype(vals))
      },
      permutation = function(x) {
        arr <- if (is_int_like(x) && length(x) == 1L) seq(0L, as.integer(x) - 1L) else as.vector(py_ndarray(x)$.arr)
        n <- length(arr)
        if (n <= 1L) return(py_ndarray(arr))
        for (i in seq(n, 2L, by = -1L)) {
          j <- .Call(ffi_pcg64_integers, ptr, 0L, i, 1L)[[1L]] + 1L
          tmp <- arr[[i]]
          arr[[i]] <- arr[[j]]
          arr[[j]] <- tmp
        }
        py_ndarray(arr)
      },
      shuffle = function(x) {
        if (inherits(x, "pyrt_ndarray")) {
          p <- gen$permutation(x$.arr)
          x$.arr <- p$.arr
        } else if (inherits(x, "pyrt_list")) {
          p <- gen$permutation(seq_along(x$.items) - 1L)
          x$.items <- x$.items[as.integer(p$.arr) + 1L]
        }
        invisible(py_none())
      }
    )
    gen
  }

  default_rs <- make_random_state(19650218L)
  make_mod_env(
    RandomState = make_random_state,
    Generator = make_pcg64_generator,
    default_rng = make_pcg64_generator,
    seed = default_rs$seed,
    random = default_rs$random,
    random_sample = default_rs$random_sample,
    rand = default_rs$rand,
    randn = default_rs$randn,
    uniform = default_rs$uniform,
    normal = default_rs$normal,
    standard_normal = default_rs$standard_normal,
    randint = default_rs$randint,
    choice = default_rs$choice,
    permutation = default_rs$permutation,
    shuffle = default_rs$shuffle
  )
}

# ---- 3. `scipy` module builder (`special`, `stats`, `optimize`, `linalg`) ---

build_scipy_module <- function() {
  special_mod <- make_mod_env(
    gamma = unary_ufunc(base::gamma),
    gammaln = unary_ufunc(base::lgamma),
    loggamma = unary_ufunc(base::lgamma),
    beta = function(a, b) base::beta(as.double(a), as.double(b)),
    betaln = function(a, b) base::lbeta(as.double(a), as.double(b)),
    digamma = unary_ufunc(base::digamma),
    psi = unary_ufunc(base::digamma),
    polygamma = function(n, x) base::psigamma(as.double(x), deriv = as.integer(n)),
    erf = unary_ufunc(function(x) 2 * stats::pnorm(x * sqrt(2)) - 1),
    erfc = unary_ufunc(function(x) 2 * stats::pnorm(x * sqrt(2), lower.tail = FALSE)),
    expit = unary_ufunc(stats::plogis),
    logit = unary_ufunc(stats::qlogis),
    comb = function(N, k, exact = FALSE) {
      if (isTRUE(exact)) py_stdlib("math")$comb(N, k) else choose(as.double(N), as.double(k))
    },
    factorial = unary_ufunc(base::factorial)
  )

  norm_dist <- make_mod_env(
    pdf = function(x, loc = 0.0, scale = 1.0) {
      unary_ufunc(function(v) stats::dnorm(v, mean = as.double(loc), sd = as.double(scale)))(x)
    },
    logpdf = function(x, loc = 0.0, scale = 1.0) {
      unary_ufunc(function(v) stats::dnorm(v, mean = as.double(loc), sd = as.double(scale), log = TRUE))(x)
    },
    cdf = function(x, loc = 0.0, scale = 1.0) {
      unary_ufunc(function(v) stats::pnorm(v, mean = as.double(loc), sd = as.double(scale)))(x)
    },
    sf = function(x, loc = 0.0, scale = 1.0) {
      unary_ufunc(function(v) stats::pnorm(v, mean = as.double(loc), sd = as.double(scale), lower.tail = FALSE))(x)
    },
    ppf = function(q, loc = 0.0, scale = 1.0) {
      unary_ufunc(function(v) stats::qnorm(v, mean = as.double(loc), sd = as.double(scale)))(q)
    }
  )

  t_dist <- make_mod_env(
    pdf = function(x, df, loc = 0.0, scale = 1.0) {
      sc <- as.double(scale)
      unary_ufunc(function(v) stats::dt((v - as.double(loc)) / sc, df = as.double(df)) / sc)(x)
    },
    cdf = function(x, df, loc = 0.0, scale = 1.0) {
      sc <- as.double(scale)
      unary_ufunc(function(v) stats::pt((v - as.double(loc)) / sc, df = as.double(df)))(x)
    },
    ppf = function(q, df, loc = 0.0, scale = 1.0) {
      sc <- as.double(scale)
      unary_ufunc(function(v) as.double(loc) + sc * stats::qt(v, df = as.double(df)))(q)
    }
  )

  chi2_dist <- make_mod_env(
    pdf = function(x, df) unary_ufunc(function(v) stats::dchisq(v, df = as.double(df)))(x),
    cdf = function(x, df) unary_ufunc(function(v) stats::pchisq(v, df = as.double(df)))(x),
    sf = function(x, df) unary_ufunc(function(v) stats::pchisq(v, df = as.double(df), lower.tail = FALSE))(x),
    ppf = function(q, df) unary_ufunc(function(v) stats::qchisq(v, df = as.double(df)))(q)
  )

  stats_mod <- make_mod_env(
    norm = norm_dist,
    t = t_dist,
    chi2 = chi2_dist,
    pearsonr = function(x, y) {
      xv <- as.numeric(py_ndarray(x)$.arr)
      yv <- as.numeric(py_ndarray(y)$.arr)
      ct <- stats::cor.test(xv, yv, method = "pearson")
      res <- py_tuple(list(unname(as.double(ct$estimate)), unname(as.double(ct$p.value))))
      attr(res, "nt_fields") <- c("statistic", "pvalue")
      class(res) <- c("pyrt_namedtuple", class(res))
      res
    },
    spearmanr = function(a, b) {
      xv <- as.numeric(py_ndarray(a)$.arr)
      yv <- as.numeric(py_ndarray(b)$.arr)
      ct <- stats::cor.test(xv, yv, method = "spearman", exact = FALSE)
      res <- py_tuple(list(unname(as.double(ct$estimate)), unname(as.double(ct$p.value))))
      attr(res, "nt_fields") <- c("statistic", "pvalue")
      class(res) <- c("pyrt_namedtuple", class(res))
      res
    },
    ttest_ind = function(a, b, equal_var = TRUE) {
      xv <- as.numeric(py_ndarray(a)$.arr)
      yv <- as.numeric(py_ndarray(b)$.arr)
      tt <- stats::t.test(xv, yv, var.equal = isTRUE(equal_var))
      res <- py_tuple(list(unname(as.double(tt$statistic)), unname(as.double(tt$p.value))))
      attr(res, "nt_fields") <- c("statistic", "pvalue")
      class(res) <- c("pyrt_namedtuple", class(res))
      res
    },
    ttest_1samp = function(a, popmean) {
      xv <- as.numeric(py_ndarray(a)$.arr)
      tt <- stats::t.test(xv, mu = as.double(popmean))
      res <- py_tuple(list(unname(as.double(tt$statistic)), unname(as.double(tt$p.value))))
      attr(res, "nt_fields") <- c("statistic", "pvalue")
      class(res) <- c("pyrt_namedtuple", class(res))
      res
    },
    linregress = function(x, y) {
      xv <- as.numeric(py_ndarray(x)$.arr)
      yv <- as.numeric(py_ndarray(y)$.arr)
      fit <- stats::lm(yv ~ xv)
      sm <- suppressWarnings(summary(fit))
      slope <- unname(stats::coef(fit)[[2L]])
      intercept <- unname(stats::coef(fit)[[1L]])
      rval <- stats::cor(xv, yv)
      pval <- unname(sm$coefficients[2L, 4L])
      stderr <- unname(sm$coefficients[2L, 2L])
      res <- py_tuple(list(slope, intercept, rval, pval, stderr))
      attr(res, "nt_fields") <- c("slope", "intercept", "rvalue", "pvalue", "stderr")
      class(res) <- c("pyrt_namedtuple", class(res))
      res
    }
  )

  optimize_mod <- make_mod_env(
    minimize_scalar = function(fun, bounds = NULL, bracket = NULL) {
      interval <- if (!is.null(bounds) && !is_py_none(bounds)) {
        as.numeric(unlist(coerce_items(bounds)))
      } else if (!is.null(bracket) && !is_py_none(bracket)) {
        b <- as.numeric(unlist(coerce_items(bracket)))
        c(min(b), max(b))
      } else {
        c(-1e4, 1e4)
      }
      opt <- stats::optimize(function(x) as.double(py_call(fun, list(x))), interval = interval, tol = 1e-10)
      make_mod_env(x = opt$minimum, fun = opt$objective, success = TRUE)
    },
    minimize = function(fun, x0, method = "BFGS") {
      init <- as.numeric(py_ndarray(x0)$.arr)
      r_method <- if (toupper(as.character(method)) %in% c("BFGS", "CG", "L-BFGS-B", "SANN")) {
        toupper(as.character(method))
      } else {
        "BFGS"
      }
      opt <- stats::optim(
        par = init,
        fn = function(par) as.double(py_call(fun, list(py_ndarray(par)))),
        method = if (length(init) == 1L && r_method == "NELDER-MEAD") "BFGS" else r_method
      )
      make_mod_env(
        x = py_ndarray(opt$par),
        fun = opt$value,
        success = (opt$convergence == 0L),
        nit = as.integer(opt$counts[[1L]])
      )
    },
    brentq = function(f, a, b) {
      rt <- stats::uniroot(function(x) as.double(py_call(f, list(x))), interval = c(as.double(a), as.double(b)), tol = 1e-12)
      rt$root
    }
  )

  make_mod_env(
    special = special_mod,
    stats = stats_mod,
    optimize = optimize_mod,
    linalg = build_numpy_module()$linalg
  )
}

# ---- 4. `pandas` module builder (`Series`, `DataFrame`) ---------------------

build_pandas_module <- function() {
  series_fn <- function(data = NULL, index = NULL, name = NULL) {
    nd <- py_ndarray(data)
    vals <- as.vector(nd$.arr)
    idx <- if (is.null(index) || is_py_none(index)) {
      seq(0L, length(vals) - 1L)
    } else {
      unlist(coerce_items(index))
    }
    self <- new.env(parent = emptyenv())
    self$.vals <- vals
    self$.index <- idx
    self$name <- if (is.null(name)) py_none() else name
    self$values <- nd
    self$shape <- py_tuple(list(length(vals)))
    self$size <- length(vals)
    self$mean <- function() mean(self$.vals, na.rm = TRUE)
    self$sum <- function() sum(self$.vals, na.rm = TRUE)
    self$std <- function(ddof = 1) {
      v <- self$.vals[!is.na(self$.vals)]
      sqrt(sum((v - mean(v))^2) / (length(v) - as.numeric(ddof)))
    }
    self$min <- function() min(self$.vals, na.rm = TRUE)
    self$max <- function() max(self$.vals, na.rm = TRUE)
    self$tolist <- function() py_list(as.list(self$.vals))
    self$to_numpy <- function() self$values
    self$`__len__` <- function() length(self$.vals)
    self$`__getitem__` <- function(k) {
      if (is_int_like(k) && is.integer(self$.index)) {
        pos <- match(as.integer(k), self$.index)
        if (!is.na(pos)) return(self$.vals[[pos]])
      }
      pos <- match(k, self$.index)
      if (!is.na(pos)) return(self$.vals[[pos]])
      self$.vals[[as.integer(k) + 1L]]
    }
    self$`__add__` <- function(other) {
      ov <- if (inherits(other, "pyrt_Series")) other$.vals else as.numeric(other)
      series_fn(self$.vals + ov, index = self$.index, name = self$name)
    }
    self$`__sub__` <- function(other) {
      ov <- if (inherits(other, "pyrt_Series")) other$.vals else as.numeric(other)
      series_fn(self$.vals - ov, index = self$.index, name = self$name)
    }
    self$`__mul__` <- function(other) {
      ov <- if (inherits(other, "pyrt_Series")) other$.vals else as.numeric(other)
      series_fn(self$.vals * ov, index = self$.index, name = self$name)
    }
    class(self) <- c("pyrt_Series", "pyrt_container")
    self
  }

  dataframe_fn <- function(data = NULL, columns = NULL) {
    df <- if (is.data.frame(data)) {
      data
    } else if (inherits(data, "pyrt_dict")) {
      cols <- list()
      for (i in seq_along(data$.keys)) {
        k <- as.character(data$.keys[[i]])
        v <- data$.vals[[i]]
        cols[[k]] <- if (inherits(v, "pyrt_ndarray")) as.vector(v$.arr) else unlist(coerce_items(v))
      }
      as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
    } else if (inherits(data, "pyrt_ndarray")) {
      d <- as.data.frame(data$.arr, stringsAsFactors = FALSE)
      if (!is.null(columns) && !is_py_none(columns)) {
        names(d) <- as.character(unlist(coerce_items(columns)))
      }
      d
    } else if (is.list(data)) {
      as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      data.frame()
    }

    self <- new.env(parent = emptyenv())
    self$.df <- df
    self$columns <- py_list(as.list(names(df)))
    self$shape <- py_tuple(list(nrow(df), ncol(df)))
    self$head <- function(n = 5L) dataframe_fn(utils::head(self$.df, as.integer(n)))
    self$tail <- function(n = 5L) dataframe_fn(utils::tail(self$.df, as.integer(n)))
    self$to_numpy <- function() py_ndarray(as.matrix(self$.df))
    self$as_r_data_frame <- function() self$.df
    self$`__len__` <- function() nrow(self$.df)
    self$`__getitem__` <- function(col) {
      col_str <- as.character(col)
      if (!(col_str %in% names(self$.df))) py_raise("KeyError", col_str)
      series_fn(self$.df[[col_str]], name = col_str)
    }
    self$`__setitem__` <- function(col, val) {
      col_str <- as.character(col)
      v <- if (inherits(val, "pyrt_Series")) val$.vals
           else if (inherits(val, "pyrt_ndarray")) as.vector(val$.arr)
           else if (inherits(val, "pyrt_container")) unlist(coerce_items(val))
           else val
      self$.df[[col_str]] <- v
      self$columns <- py_list(as.list(names(self$.df)))
      self$shape <- py_tuple(list(nrow(self$.df), ncol(self$.df)))
      invisible(self)
    }
    class(self) <- c("pyrt_DataFrame", "pyrt_container")
    self
  }

  make_mod_env(
    Series = series_fn,
    DataFrame = dataframe_fn,
    isna = function(x) is.na(x) || is_py_none(x),
    notna = function(x) !is.na(x) && !is_py_none(x)
  )
}
