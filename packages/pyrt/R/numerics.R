# Python numerics, exact floor division / modulo, arbitrary-precision ints
# (`gmp::bigz`), two's-complement bitwise operations on negative integers, and
# operator / comparison dispatch with dunder hooks.

is_int_like <- function(x) {
  (is.integer(x) && length(x) == 1L && !is.na(x)) || gmp::is.bigz(x)
}

is_num_scalar <- function(x) {
  is_int_like(x) || (is.double(x) && length(x) == 1L) || (is.logical(x) && length(x) == 1L)
}

# Promote small exact `bigz` back to R `integer` when it fits in signed 32-bit,
# and promote R integers to `bigz` when an operation could overflow 32 bits.
normalize_py_int <- function(z) {
  if (gmp::is.bigz(z) && length(z) == 1L && !is.na(z) &&
      z >= -2147483647L && z <= 2147483647L) {
    return(as.integer(z))
  }
  z
}

as_bigz_exact <- function(x) {
  if (gmp::is.bigz(x)) return(x)
  if (is.logical(x)) return(gmp::as.bigz(as.integer(x)))
  if (is.integer(x)) return(gmp::as.bigz(x))
  if (is.double(x)) return(gmp::as.bigz(sprintf("%.0f", trunc(x))))
  gmp::as.bigz(x)
}

#' Python `int()` constructor and exact integer / operator semantics
#'
#' `py_int()` converts numbers, booleans, or strings (in base 0 or 2..36, with
#' optional `_` separators) to a Python integer (`integer` when in 32-bit
#' signed range, `gmp::bigz` when larger). `py_floordiv()`, `py_mod()`,
#' `py_divmod()`, `py_pow()`, `py_bitand()`, `py_bitor()`, `py_bitxor()`,
#' `py_invert()`, `py_lshift()`, and `py_rshift()` implement Python's exact
#' numeric and two's-complement bitwise semantics (including floor division and
#' modulo toward `-Inf`, and raises `ZeroDivisionError` on division by zero).
#'
#' @param x,y Operands (integers, `gmp::bigz`, doubles, booleans, containers, or
#'   `pyrt_instance` objects implementing dunder methods such as `__add__`).
#' @param base Integer base for string conversion in `py_int()` (`0L` or `2..36`).
#' @param mod Optional modulus for 3-argument `pow(x, y, mod)`.
#' @returns An R `integer`, `gmp::bigz`, `double`, `complex`, or dunder result.
#' @export
#' @examples
#' py_floordiv(-7L, 2L)
#' py_mod(-7L, 2L)
#' py_bitand(-5L, 3L)
#' py_rshift(-8L, 2L)
py_int <- function(x = 0L, base = NULL) {
  if (!is.null(base)) {
    if (!is.character(x) || length(x) != 1L) {
      py_raise("TypeError", "int() can't convert non-string with explicit base")
    }
    return(parse_str_int(x, as.integer(base)))
  }
  if (inherits(x, "pyrt_instance")) {
    m <- py_getattr_default(x, "__int__", NULL)
    if (!is.null(m)) return(m())
  }
  if (gmp::is.bigz(x)) return(normalize_py_int(x))
  if (is.logical(x) && length(x) == 1L) return(if (isTRUE(x)) 1L else 0L)
  if (is.integer(x) && length(x) == 1L) return(x)
  if (is.double(x) && length(x) == 1L) {
    if (is.infinite(x)) py_raise("OverflowError", "cannot convert float infinity to integer")
    if (is.nan(x)) py_raise("ValueError", "cannot convert float NaN to integer")
    t <- trunc(x)
    if (t >= -2147483647 && t <= 2147483647) return(as.integer(t))
    return(normalize_py_int(gmp::as.bigz(sprintf("%.0f", t))))
  }
  if (is.character(x) && length(x) == 1L) {
    return(parse_str_int(x, 10L))
  }
  py_raise("TypeError", paste0("int() argument must be a string or a real number"))
}

parse_str_int <- function(s, base) {
  raw <- trimws(s)
  if (!nzchar(raw)) py_raise("ValueError", sprintf("invalid literal for int() with base %d: '%s'", base, s))
  sign <- 1L
  if ( startsWith(raw, "-") ) {
    sign <- -1L
    raw <- substring(raw, 2L)
  } else if ( startsWith(raw, "+") ) {
    raw <- substring(raw, 2L)
  }
  raw <- gsub("_", "", raw, fixed = TRUE)
  pfx <- tolower(substr(raw, 1L, 2L))
  if (base == 0L) {
    base <- if (pfx == "0x") 16L else if (pfx == "0o") 8L else if (pfx == "0b") 2L else 10L
  }
  if (base == 16L && pfx == "0x") raw <- substring(raw, 3L)
  if (base == 8L && pfx == "0o") raw <- substring(raw, 3L)
  if (base == 2L && pfx == "0b") raw <- substring(raw, 3L)
  if (!nzchar(raw)) py_raise("ValueError", sprintf("invalid literal for int() with base %d: '%s'", base, s))

  val <- if (base == 10L) {
    if (!grepl("^[0-9]+$", raw)) py_raise("ValueError", sprintf("invalid literal for int() with base 10: '%s'", s))
    gmp::as.bigz(sub("^0+([0-9])", "\\1", raw))
  } else if (base == 16L) {
    if (!grepl("^[0-9a-fA-F]+$", raw)) py_raise("ValueError", sprintf("invalid literal for int() with base 16: '%s'", s))
    gmp::as.bigz(paste0("0x", raw))
  } else if (base == 8L) {
    if (!grepl("^[0-7]+$", raw)) py_raise("ValueError", sprintf("invalid literal for int() with base 8: '%s'", s))
    gmp::as.bigz(paste0("0", raw))
  } else if (base == 2L) {
    if (!grepl("^[01]+$", raw)) py_raise("ValueError", sprintf("invalid literal for int() with base 2: '%s'", s))
    gmp::as.bigz(paste0("0b", raw))
  } else if (base >= 2L && base <= 36L) {
    digits <- utf8ToInt(tolower(raw))
    acc <- gmp::as.bigz(0L)
    b_big <- gmp::as.bigz(base)
    for (ch in digits) {
      d <- if (ch >= 48L && ch <= 57L) ch - 48L else if (ch >= 97L && ch <= 122L) ch - 87L else base
      if (d >= base) py_raise("ValueError", sprintf("invalid literal for int() with base %d: '%s'", base, s))
      acc <- acc * b_big + d
    }
    acc
  } else {
    py_raise("ValueError", "int() base must be >= 2 and <= 36, or 0")
  }
  if (sign < 0L) val <- -val
  normalize_py_int(val)
}

# ---- Dunder dispatch helper -------------------------------------------------

try_dunder_binop <- function(x, y, dunder, rdunder = NULL) {
  if (inherits(x, "pyrt_instance") || inherits(x, "pyrt_ndarray") || inherits(x, "pyrt_Series")) {
    fn <- py_getattr_default(x, dunder, NULL)
    if (!is.null(fn)) return(list(handled = TRUE, value = fn(y)))
  }
  if (!is.null(rdunder) && (inherits(y, "pyrt_instance") || inherits(y, "pyrt_ndarray") || inherits(y, "pyrt_Series"))) {
    fn <- py_getattr_default(y, rdunder, NULL)
    if (!is.null(fn)) return(list(handled = TRUE, value = fn(x)))
  }
  list(handled = FALSE, value = NULL)
}

#' @rdname py_int
#' @export
py_add <- function(x, y) {
  d <- try_dunder_binop(x, y, "__add__", "__radd__")
  if (d$handled) return(d$value)
  if (is.character(x) && is.character(y) && length(x) == 1L && length(y) == 1L) {
    return(paste0(x, y))
  }
  if (inherits(x, "pyrt_list") && inherits(y, "pyrt_list")) {
    return(py_list(c(x$as_r_list(), y$as_r_list())))
  }
  if (inherits(x, "pyrt_tuple") && inherits(y, "pyrt_tuple")) {
    return(py_tuple(c(x$items, y$items)))
  }
  if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
    if (!gmp::is.bigz(x) && !gmp::is.bigz(y)) {
      res_d <- as.double(x) + as.double(y)
      if (res_d >= -2147483647 && res_d <= 2147483647) return(as.integer(res_d))
    }
    return(normalize_py_int(as_bigz_exact(x) + as_bigz_exact(y)))
  }
  if (gmp::is.bigz(x)) x <- as.double(x)
  if (gmp::is.bigz(y)) y <- as.double(y)
  x + y
}

#' @rdname py_int
#' @export
py_sub <- function(x, y) {
  d <- try_dunder_binop(x, y, "__sub__", "__rsub__")
  if (d$handled) return(d$value)
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) {
    return(x$difference(y))
  }
  if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
    if (!gmp::is.bigz(x) && !gmp::is.bigz(y)) {
      res_d <- as.double(x) - as.double(y)
      if (res_d >= -2147483647 && res_d <= 2147483647) return(as.integer(res_d))
    }
    return(normalize_py_int(as_bigz_exact(x) - as_bigz_exact(y)))
  }
  if (gmp::is.bigz(x)) x <- as.double(x)
  if (gmp::is.bigz(y)) y <- as.double(y)
  x - y
}

#' @rdname py_int
#' @export
py_mul <- function(x, y) {
  d <- try_dunder_binop(x, y, "__mul__", "__rmul__")
  if (d$handled) return(d$value)
  # String / list / tuple repetition `s * n` or `n * s`.
  if (is.character(x) && length(x) == 1L && (is_int_like(y) || is.logical(y))) {
    n <- max(0L, as.integer(y))
    return(strrep(x, n))
  }
  if (is.character(y) && length(y) == 1L && (is_int_like(x) || is.logical(x))) {
    n <- max(0L, as.integer(x))
    return(strrep(y, n))
  }
  if (inherits(x, "pyrt_list") && (is_int_like(y) || is.logical(y))) {
    n <- max(0L, as.integer(y))
    return(py_list(rep(x$as_r_list(), times = n)))
  }
  if (inherits(x, "pyrt_tuple") && (is_int_like(y) || is.logical(y))) {
    n <- max(0L, as.integer(y))
    return(py_tuple(rep(x$items, times = n)))
  }
  if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
    if (!gmp::is.bigz(x) && !gmp::is.bigz(y)) {
      res_d <- as.double(x) * as.double(y)
      if (abs(res_d) <= 2147483647) return(as.integer(res_d))
    }
    return(normalize_py_int(as_bigz_exact(x) * as_bigz_exact(y)))
  }
  if (gmp::is.bigz(x)) x <- as.double(x)
  if (gmp::is.bigz(y)) y <- as.double(y)
  x * y
}

#' @rdname py_int
#' @export
py_truediv <- function(x, y) {
  d <- try_dunder_binop(x, y, "__truediv__", "__rtruediv__")
  if (d$handled) return(d$value)
  if (gmp::is.bigz(y)) {
    if (all(y == 0L)) py_raise("ZeroDivisionError", "division by zero")
  } else if (is.numeric(y) || is.logical(y) || is.complex(y)) {
    if (length(y) == 1L && y == 0) py_raise("ZeroDivisionError", "division by zero")
  }
  if (gmp::is.bigz(x)) x <- as.double(x)
  if (gmp::is.bigz(y)) y <- as.double(y)
  as.double(x) / as.double(y)
}

#' @rdname py_int
#' @export
py_floordiv <- function(x, y) {
  d <- try_dunder_binop(x, y, "__floordiv__", "__rfloordiv__")
  if (d$handled) return(d$value)
  if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
    zb <- as_bigz_exact(y)
    if (all(zb == 0L)) py_raise("ZeroDivisionError", "integer division or modulo by zero")
    za <- as_bigz_exact(x)
    # Compute exact floor division toward -Inf from truncated division on |za| and |zb|:
    q_abs <- gmp::divq.bigz(abs(za), abs(zb))
    r_abs <- abs(za) - q_abs * abs(zb)
    same_sign <- (za >= 0L && zb > 0L) || (za <= 0L && zb < 0L)
    q <- if (same_sign) {
      q_abs
    } else if (all(r_abs == 0L)) {
      -q_abs
    } else {
      -q_abs - 1L
    }
    return(normalize_py_int(q))
  }
  if (length(y) == 1L && y == 0) py_raise("ZeroDivisionError", "float floor division by zero")
  floor(as.double(x) / as.double(y))
}

#' @rdname py_int
#' @export
py_mod <- function(x, y) {
  d <- try_dunder_binop(x, y, "__mod__", "__rmod__")
  if (d$handled) return(d$value)
  if (is.character(x) && length(x) == 1L) {
    return(py_percent_format(x, y))
  }
  if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
    zb <- as_bigz_exact(y)
    if (all(zb == 0L)) py_raise("ZeroDivisionError", "integer modulo by zero")
    za <- as_bigz_exact(x)
    q <- as_bigz_exact(py_floordiv(za, zb))
    return(normalize_py_int(za - q * zb))
  }
  if (length(y) == 1L && y == 0) py_raise("ZeroDivisionError", "float modulo")
  xd <- as.double(x)
  yd <- as.double(y)
  xd - floor(xd / yd) * yd
}

#' @rdname py_int
#' @export
py_divmod <- function(x, y) {
  py_tuple(list(py_floordiv(x, y), py_mod(x, y)))
}

#' @rdname py_int
#' @export
py_pow <- function(x, y, mod = NULL) {
  if (is.null(mod)) {
    d <- try_dunder_binop(x, y, "__pow__", "__rpow__")
    if (d$handled) return(d$value)
  }
  if (!is.null(mod)) {
    za <- as_bigz_exact(x)
    zb <- as_bigz_exact(y)
    zm <- as_bigz_exact(mod)
    if (all(zm == 0L)) py_raise("ValueError", "pow() 3rd argument cannot be 0")
    return(normalize_py_int(gmp::powm(za, zb, zm)))
  }
  if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
    zb <- as_bigz_exact(y)
    if (zb < 0L) {
      return(py_truediv(1.0, as.double(py_pow(x, -zb))))
    }
    za <- as_bigz_exact(x)
    return(normalize_py_int(gmp::pow.bigz(za, zb)))
  }
  if (gmp::is.bigz(x)) x <- as.double(x)
  if (gmp::is.bigz(y)) y <- as.double(y)
  x ^ y
}

# ---- Two's-complement bitwise operations on arbitrary-precision integers ----
#
# Python integers behave as if represented in infinite-width two's complement:
# for `w` bits slightly wider than both operands (`max(bit_length(a), bit_length(b)) + 2`),
# mapping a negative integer `x` to `x + 2^w`, applying unsigned bitwise AND /
# OR / XOR on `[0, 2^w - 1]`, and mapping results with top sign bit `>= 2^(w-1)`
# back by subtracting `2^w` reproduces Python's exact result for all signs.

twos_comp_bitop <- function(x, y, op) {
  za <- as_bigz_exact(x)
  zb <- as_bigz_exact(y)
  wa <- if (all(za == 0L)) 1L else gmp::sizeinbase(abs(za), 2L)
  wb <- if (all(zb == 0L)) 1L else gmp::sizeinbase(abs(zb), 2L)
  w <- max(wa, wb) + 2L
  mod2w <- gmp::pow.bigz(gmp::as.bigz(2L), w)
  ua <- if (za < 0L) za + mod2w else za
  ub <- if (zb < 0L) zb + mod2w else zb

  # Decompose into bits in base 2 (exact for arbitrary width).
  res <- gmp::as.bigz(0L)
  bit_val <- gmp::as.bigz(1L)
  two <- gmp::as.bigz(2L)
  for (i in seq_len(w)) {
    ba <- as.integer(ua %% two)
    bb <- as.integer(ub %% two)
    ua <- gmp::divq.bigz(ua, two)
    ub <- gmp::divq.bigz(ub, two)
    br <- switch(op,
      and = bitwAnd(ba, bb),
      or  = bitwOr(ba, bb),
      xor = bitwXor(ba, bb)
    )
    if (br != 0L) res <- res + bit_val
    bit_val <- bit_val * two
  }
  sign_thresh <- gmp::pow.bigz(two, w - 1L)
  if (res >= sign_thresh) res <- res - mod2w
  normalize_py_int(res)
}

#' @rdname py_int
#' @export
py_bitand <- function(x, y) {
  d <- try_dunder_binop(x, y, "__and__", "__rand__")
  if (d$handled) return(d$value)
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) return(x$intersection(y))
  if (is.logical(x) && is.logical(y) && length(x) == 1L && length(y) == 1L) return(x && y)
  twos_comp_bitop(x, y, "and")
}

#' @rdname py_int
#' @export
py_bitor <- function(x, y) {
  d <- try_dunder_binop(x, y, "__or__", "__ror__")
  if (d$handled) return(d$value)
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) return(x$union(y))
  if (inherits(x, "pyrt_dict") && inherits(y, "pyrt_dict")) {
    out <- x$copy()
    out$update(y)
    return(out)
  }
  if (is.logical(x) && is.logical(y) && length(x) == 1L && length(y) == 1L) return(x || y)
  twos_comp_bitop(x, y, "or")
}

#' @rdname py_int
#' @export
py_bitxor <- function(x, y) {
  d <- try_dunder_binop(x, y, "__xor__", "__rxor__")
  if (d$handled) return(d$value)
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) return(x$symmetric_difference(y))
  if (is.logical(x) && is.logical(y) && length(x) == 1L && length(y) == 1L) return(xor(x, y))
  twos_comp_bitop(x, y, "xor")
}

#' @rdname py_int
#' @export
py_invert <- function(x) {
  if (inherits(x, "pyrt_instance")) {
    fn <- py_getattr_default(x, "__invert__", NULL)
    if (!is.null(fn)) return(fn())
  }
  # In Python, `~x == -(x + 1)` for all integers and booleans.
  normalize_py_int(-(as_bigz_exact(x) + 1L))
}

#' @rdname py_int
#' @export
py_lshift <- function(x, y) {
  d <- try_dunder_binop(x, y, "__lshift__", "__rlshift__")
  if (d$handled) return(d$value)
  n <- as.integer(y)
  if (n < 0L) py_raise("ValueError", "negative shift count")
  normalize_py_int(as_bigz_exact(x) * gmp::pow.bigz(gmp::as.bigz(2L), n))
}

#' @rdname py_int
#' @export
py_rshift <- function(x, y) {
  d <- try_dunder_binop(x, y, "__rshift__", "__rrshift__")
  if (d$handled) return(d$value)
  n <- as.integer(y)
  if (n < 0L) py_raise("ValueError", "negative shift count")
  # Arithmetic right shift is exact floor division by `2^n`.
  py_floordiv(as_bigz_exact(x), gmp::pow.bigz(gmp::as.bigz(2L), n))
}

#' @rdname py_int
#' @export
py_neg <- function(x) {
  if (inherits(x, "pyrt_instance") || inherits(x, "pyrt_ndarray")) {
    fn <- py_getattr_default(x, "__neg__", NULL)
    if (!is.null(fn)) return(fn())
  }
  if (is_int_like(x) || is.logical(x)) {
    return(normalize_py_int(-as_bigz_exact(x)))
  }
  -x
}

#' @rdname py_int
#' @export
py_pos <- function(x) {
  if (inherits(x, "pyrt_instance") || inherits(x, "pyrt_ndarray")) {
    fn <- py_getattr_default(x, "__pos__", NULL)
    if (!is.null(fn)) return(fn())
  }
  if (is.logical(x) && length(x) == 1L) return(as.integer(x))
  x
}

#' @rdname py_int
#' @export
py_abs <- function(x) {
  if (inherits(x, "pyrt_instance") || inherits(x, "pyrt_ndarray")) {
    fn <- py_getattr_default(x, "__abs__", NULL)
    if (!is.null(fn)) return(fn())
  }
  if (gmp::is.bigz(x)) return(normalize_py_int(abs(x)))
  if (is.integer(x) && length(x) == 1L && x == -2147483647L) return(2147483647L)
  abs(x)
}

#' @rdname py_int
#' @export
py_matmul <- function(x, y) {
  d <- try_dunder_binop(x, y, "__matmul__", "__rmatmul__")
  if (d$handled) return(d$value)
  np_dot(x, y)
}

# ---- Python equality, ordering, identity (`is`), and membership (`in`) ------

#' Python comparison, identity, and membership operators
#'
#' `py_eq()`, `py_ne()`, `py_lt()`, `py_le()`, `py_gt()`, `py_ge()` implement
#' Python's value equality and ordering rules (including `1 == 1.0 == True`,
#' `gmp::bigz` comparisons, structural equality on `py_list` / `py_tuple` /
#' `py_dict` / `py_set`, and dunder hooks `__eq__`, `__lt__`, etc.). `py_is()`
#' tests object identity (for reference-semantics objects, whether two bindings
#' point to the same underlying environment). `py_in()` tests membership (`x in
#' container`), dispatching to `__contains__` when present.
#'
#' @param x,y Operands.
#' @returns A single `logical` (`TRUE` or `FALSE`), or a `pyrt_ndarray` for
#'   array comparisons.
#' @export
#' @examples
#' py_eq(1L, TRUE)
#' py_eq(py_list(list(1L, 2L)), py_list(list(1.0, 2.0)))
py_eq <- function(x, y) {
  d <- try_dunder_binop(x, y, "__eq__", "__eq__")
  if (d$handled) return(if (inherits(d$value, "pyrt_ndarray")) d$value else py_bool(d$value))
  if (is.null(x) || is.null(y)) return(is.null(x) && is.null(y))
  if (inherits(x, "pyrt_ellipsis") || inherits(y, "pyrt_ellipsis")) {
    return(inherits(x, "pyrt_ellipsis") && inherits(y, "pyrt_ellipsis"))
  }
  if (is_num_scalar(x) && is_num_scalar(y)) {
    if (is.double(x) && is.nan(x)) return(FALSE)
    if (is.double(y) && is.nan(y)) return(FALSE)
    if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
      return(all(as_bigz_exact(x) == as_bigz_exact(y)))
    }
    return(as.double(x) == as.double(y))
  }
  if (is.character(x) && is.character(y) && length(x) == 1L && length(y) == 1L) {
    return(identical(x, y))
  }
  if (inherits(x, "pyrt_list") && inherits(y, "pyrt_list")) {
    lx <- x$as_r_list()
    ly <- y$as_r_list()
    if (length(lx) != length(ly)) return(FALSE)
    for (i in seq_along(lx)) if (!py_eq(lx[[i]], ly[[i]])) return(FALSE)
    return(TRUE)
  }
  if (inherits(x, "pyrt_tuple") && inherits(y, "pyrt_tuple")) {
    lx <- x$items
    ly <- y$items
    if (length(lx) != length(ly)) return(FALSE)
    for (i in seq_along(lx)) if (!py_eq(lx[[i]], ly[[i]])) return(FALSE)
    return(TRUE)
  }
  if (inherits(x, "pyrt_dict") && inherits(y, "pyrt_dict")) {
    return(x$equals(y))
  }
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) {
    return(x$equals(y))
  }
  if (is.environment(x) && is.environment(y)) {
    return(identical(x, y))
  }
  identical(x, y)
}

#' @rdname py_eq
#' @export
py_ne <- function(x, y) {
  d <- try_dunder_binop(x, y, "__ne__", "__ne__")
  if (d$handled) return(if (inherits(d$value, "pyrt_ndarray")) d$value else py_bool(d$value))
  !py_eq(x, y)
}

#' @rdname py_eq
#' @export
py_lt <- function(x, y) {
  d <- try_dunder_binop(x, y, "__lt__", "__gt__")
  if (d$handled) return(if (inherits(d$value, "pyrt_ndarray")) d$value else py_bool(d$value))
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) {
    return(x$issubset(y) && !x$equals(y))
  }
  if ((inherits(x, "pyrt_list") && inherits(y, "pyrt_list")) ||
      (inherits(x, "pyrt_tuple") && inherits(y, "pyrt_tuple"))) {
    lx <- if (inherits(x, "pyrt_list")) x$as_r_list() else x$items
    ly <- if (inherits(y, "pyrt_list")) y$as_r_list() else y$items
    n <- min(length(lx), length(ly))
    for (i in seq_len(n)) {
      if (!py_eq(lx[[i]], ly[[i]])) return(py_lt(lx[[i]], ly[[i]]))
    }
    return(length(lx) < length(ly))
  }
  if ((is_int_like(x) || is.logical(x)) && (is_int_like(y) || is.logical(y))) {
    return(all(as_bigz_exact(x) < as_bigz_exact(y)))
  }
  if (gmp::is.bigz(x)) x <- as.double(x)
  if (gmp::is.bigz(y)) y <- as.double(y)
  isTRUE(x < y)
}

#' @rdname py_eq
#' @export
py_le <- function(x, y) {
  d <- try_dunder_binop(x, y, "__le__", "__ge__")
  if (d$handled) return(if (inherits(d$value, "pyrt_ndarray")) d$value else py_bool(d$value))
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) return(x$issubset(y))
  py_lt(x, y) || py_eq(x, y)
}

#' @rdname py_eq
#' @export
py_gt <- function(x, y) {
  d <- try_dunder_binop(x, y, "__gt__", "__lt__")
  if (d$handled) return(if (inherits(d$value, "pyrt_ndarray")) d$value else py_bool(d$value))
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) {
    return(x$issuperset(y) && !x$equals(y))
  }
  py_lt(y, x)
}

#' @rdname py_eq
#' @export
py_ge <- function(x, y) {
  d <- try_dunder_binop(x, y, "__ge__", "__le__")
  if (d$handled) return(if (inherits(d$value, "pyrt_ndarray")) d$value else py_bool(d$value))
  if (inherits(x, "pyrt_set") && inherits(y, "pyrt_set")) return(x$issuperset(y))
  py_gt(x, y) || py_eq(x, y)
}

#' @rdname py_eq
#' @export
py_is <- function(x, y) {
  if (is.null(x) || is.null(y)) return(is.null(x) && is.null(y))
  if (is.logical(x) && is.logical(y) && length(x) == 1L && length(y) == 1L) {
    return(identical(x, y))
  }
  if (is.environment(x) || is.environment(y)) {
    return(identical(x, y))
  }
  identical(x, y)
}

#' @rdname py_eq
#' @export
py_in <- function(x, y) {
  if (inherits(y, "pyrt_instance")) {
    fn <- py_getattr_default(y, "__contains__", NULL)
    if (!is.null(fn)) return(py_bool(fn(x)))
  }
  if (inherits(y, "pyrt_dict") || inherits(y, "pyrt_set")) {
    return(y$contains(x))
  }
  if (inherits(y, "pyrt_list")) {
    for (el in y$as_r_list()) if (py_eq(x, el)) return(TRUE)
    return(FALSE)
  }
  if (inherits(y, "pyrt_tuple")) {
    for (el in y$items) if (py_eq(x, el)) return(TRUE)
    return(FALSE)
  }
  if (is.character(y) && length(y) == 1L) {
    if (!is.character(x) || length(x) != 1L) {
      py_raise("TypeError", "'in <string>' requires string as left operand")
    }
    return(grepl(x, y, fixed = TRUE))
  }
  it <- py_iter(y)
  repeat {
    nxt <- py_next_cell(it)
    if (nxt$stopped) break
    if (py_eq(x, nxt$value)) return(TRUE)
  }
  FALSE
}
