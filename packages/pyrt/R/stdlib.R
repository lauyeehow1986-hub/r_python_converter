# Python standard-library module implementations exposed via `py_stdlib(name)`:
# `math`, `itertools`, `functools`, `operator`, `collections`, `re`, `json`,
# `datetime`, `copy`, `heapq`, `bisect`, `string`, `sys`, `os`, `pathlib`,
# `io`, `warnings`, `typing`, `types`, `abc`, `dataclasses`, `enum`, `numbers`,
# and `random`.

make_mod_env <- function(...) {
  items <- list(...)
  env <- list2env(items, parent = emptyenv())
  class(env) <- "pyrt_module"
  env
}

#' @export
`$.pyrt_module` <- function(x, name) {
  if (exists(name, envir = x, inherits = FALSE)) {
    return(get(name, envir = x, inherits = FALSE))
  }
  py_raise("AttributeError", sprintf("module has no attribute '%s'", name))
}

# ---- 1. `math` --------------------------------------------------------------

build_math_module <- function() {
  gcd2 <- function(a, b) {
    za <- abs(as_bigz_exact(a))
    zb <- abs(as_bigz_exact(b))
    while (zb > 0L) {
      r <- za %% zb
      za <- zb
      zb <- r
    }
    za
  }

  make_mod_env(
    pi = base::pi,
    e = base::exp(1),
    tau = 2 * base::pi,
    inf = Inf,
    nan = NaN,
    sqrt = function(x) {
      d <- as.double(x)
      if (d < 0) py_raise("ValueError", "math domain error")
      base::sqrt(d)
    },
    exp = function(x) base::exp(as.double(x)),
    expm1 = function(x) base::expm1(as.double(x)),
    log = function(x, base = NULL) {
      d <- as.double(x)
      if (d <= 0) py_raise("ValueError", "math domain error")
      if (is.null(base)) base::log(d) else base::log(d, base = as.double(base))
    },
    log2 = function(x) {
      d <- as.double(x)
      if (d <= 0) py_raise("ValueError", "math domain error")
      base::log2(d)
    },
    log10 = function(x) {
      d <- as.double(x)
      if (d <= 0) py_raise("ValueError", "math domain error")
      base::log10(d)
    },
    log1p = function(x) {
      d <- as.double(x)
      if (d <= -1) py_raise("ValueError", "math domain error")
      base::log1p(d)
    },
    sin = function(x) base::sin(as.double(x)),
    cos = function(x) base::cos(as.double(x)),
    tan = function(x) base::tan(as.double(x)),
    asin = function(x) base::asin(as.double(x)),
    acos = function(x) base::acos(as.double(x)),
    atan = function(x) base::atan(as.double(x)),
    atan2 = function(y, x) base::atan2(as.double(y), as.double(x)),
    sinh = function(x) base::sinh(as.double(x)),
    cosh = function(x) base::cosh(as.double(x)),
    tanh = function(x) base::tanh(as.double(x)),
    asinh = function(x) base::asinh(as.double(x)),
    acosh = function(x) base::acosh(as.double(x)),
    atanh = function(x) base::atanh(as.double(x)),
    ceil = function(x) {
      if (is_int_like(x)) return(normalize_py_int(as_bigz_exact(x)))
      normalize_py_int(gmp::as.bigz(sprintf("%.0f", base::ceiling(as.double(x)))))
    },
    floor = function(x) {
      if (is_int_like(x)) return(normalize_py_int(as_bigz_exact(x)))
      normalize_py_int(gmp::as.bigz(sprintf("%.0f", base::floor(as.double(x)))))
    },
    trunc = function(x) {
      if (is_int_like(x)) return(normalize_py_int(as_bigz_exact(x)))
      normalize_py_int(gmp::as.bigz(sprintf("%.0f", base::trunc(as.double(x)))))
    },
    fabs = function(x) base::abs(as.double(x)),
    factorial = function(n) {
      zn <- as_bigz_exact(n)
      if (zn < 0L) py_raise("ValueError", "factorial() not defined for negative values")
      normalize_py_int(gmp::factorialZ(as.integer(zn)))
    },
    gcd = function(...) {
      args <- list(...)
      if (length(args) == 0L) return(0L)
      acc <- abs(as_bigz_exact(args[[1L]]))
      if (length(args) > 1L) {
        for (i in 2:length(args)) acc <- gcd2(acc, args[[i]])
      }
      normalize_py_int(acc)
    },
    lcm = function(...) {
      args <- list(...)
      if (length(args) == 0L) return(1L)
      acc <- abs(as_bigz_exact(args[[1L]]))
      if (length(args) > 1L) {
        for (i in 2:length(args)) {
          b <- abs(as_bigz_exact(args[[i]]))
          if (all(acc == 0L) || all(b == 0L)) {
            acc <- gmp::as.bigz(0L)
          } else {
            acc <- gmp::divq.bigz(acc, gcd2(acc, b)) * b
          }
        }
      }
      normalize_py_int(acc)
    },
    isclose = function(a, b, rel_tol = 1e-9, abs_tol = 0.0) {
      da <- as.double(a)
      db <- as.double(b)
      if (da == db) return(TRUE)
      if (is.infinite(da) || is.infinite(db) || is.nan(da) || is.nan(db)) return(FALSE)
      abs(da - db) <= max(rel_tol * max(abs(da), abs(db)), abs_tol)
    },
    isfinite = function(x) is.finite(as.double(x)),
    isinf = function(x) is.infinite(as.double(x)),
    isnan = function(x) is.nan(as.double(x)),
    degrees = function(x) as.double(x) * 180.0 / base::pi,
    radians = function(x) as.double(x) * base::pi / 180.0,
    hypot = function(...) base::sqrt(base::sum(as.numeric(list(...)) ^ 2)),
    copysign = function(x, y) {
      ax <- abs(as.double(x))
      dy <- as.double(y)
      if (dy < 0 || (dy == 0 && (1 / dy < 0))) -ax else ax
    },
    fmod = function(x, y) {
      dx <- as.double(x)
      dy <- as.double(y)
      if (dy == 0) py_raise("ValueError", "math domain error")
      dx - base::trunc(dx / dy) * dy
    },
    comb = function(n, k) {
      zn <- as_bigz_exact(n)
      zk <- as_bigz_exact(k)
      if (zn < 0L || zk < 0L) py_raise("ValueError", "n and k must be non-negative")
      if (zk > zn) return(0L)
      normalize_py_int(gmp::chooseZ(zn, as.integer(zk)))
    },
    perm = function(n, k = NULL) {
      zn <- as_bigz_exact(n)
      zk <- if (is.null(k)) zn else as_bigz_exact(k)
      if (zn < 0L || zk < 0L) py_raise("ValueError", "n and k must be non-negative")
      if (zk > zn) return(0L)
      normalize_py_int(gmp::chooseZ(zn, as.integer(zk)) * gmp::factorialZ(as.integer(zk)))
    },
    isqrt = function(n) {
      zn <- as_bigz_exact(n)
      if (zn < 0L) py_raise("ValueError", "isqrt() argument must be nonnegative")
      if (all(zn == 0L)) return(0L)
      # Newton integer square root on `gmp::bigz`:
      x <- gmp::pow.bigz(gmp::as.bigz(2L), (gmp::sizeinbase(zn, 2L) + 1L) %/% 2L)
      repeat {
        y <- gmp::divq.bigz(x + gmp::divq.bigz(zn, x), 2L)
        if (y >= x) break
        x <- y
      }
      normalize_py_int(x)
    },
    prod = function(iterable, start = 1L) {
      acc <- start
      for (item in py_collect(iterable)) {
        acc <- py_mul(acc, item)
      }
      acc
    }
  )
}

# ---- 2. `itertools` ---------------------------------------------------------

build_itertools_module <- function() {
  chain_fn <- function(...) {
    iters <- list(...)
    idx <- 1L
    cur_it <- NULL
    py_generator(function() {
      repeat {
        if (is.null(cur_it)) {
          if (idx > length(iters)) return(py_stop_iteration())
          cur_it <<- py_iter(iters[[idx]])
          idx <<- idx + 1L
        }
        cell <- py_next_cell(cur_it)
        if (!cell$stopped) return(py_yield(cell$value))
        cur_it <<- NULL
      }
    })
  }
  # Attach `from_iterable` to `chain`:
  chain_obj <- structure(
    function(...) chain_fn(...),
    from_iterable = function(iterables) {
      outer_it <- py_iter(iterables)
      cur_it <- NULL
      py_generator(function() {
        repeat {
          if (is.null(cur_it)) {
            oc <- py_next_cell(outer_it)
            if (oc$stopped) return(py_stop_iteration())
            cur_it <<- py_iter(oc$value)
          }
          ic <- py_next_cell(cur_it)
          if (!ic$stopped) return(py_yield(ic$value))
          cur_it <<- NULL
        }
      })
    },
    class = c("pyrt_chain_callable", "function")
  )

  make_mod_env(
    count = function(start = 0L, step = 1L) {
      cur <- start
      py_generator(function() {
        val <- cur
        cur <<- py_add(cur, step)
        py_yield(val)
      })
    },
    cycle = function(iterable) {
      saved <- list()
      it <- py_iter(iterable)
      exhausted_initial <- FALSE
      pos <- 1L
      py_generator(function() {
        if (!exhausted_initial) {
          cell <- py_next_cell(it)
          if (!cell$stopped) {
            saved[length(saved) + 1L] <<- list(cell$value)
            return(py_yield(cell$value))
          }
          exhausted_initial <<- TRUE
        }
        if (length(saved) == 0L) return(py_stop_iteration())
        val <- saved[[pos]]
        pos <<- if (pos >= length(saved)) 1L else pos + 1L
        py_yield(val)
      })
    },
    `repeat` = function(object, times = NULL) {
      remaining <- if (is.null(times)) NULL else as.integer(times)
      py_generator(function() {
        if (!is.null(remaining)) {
          if (remaining <= 0L) return(py_stop_iteration())
          remaining <<- remaining - 1L
        }
        py_yield(object)
      })
    },
    chain = chain_obj,
    islice = function(iterable, ...) {
      args <- list(...)
      if (length(args) == 1L) {
        start <- 0L
        stop_idx <- if (is_py_none(args[[1L]])) NULL else as.integer(args[[1L]])
        step <- 1L
      } else {
        start <- if (is_py_none(args[[1L]])) 0L else as.integer(args[[1L]])
        stop_idx <- if (is_py_none(args[[2L]])) NULL else as.integer(args[[2L]])
        step <- if (length(args) >= 3L && !is_py_none(args[[3L]])) as.integer(args[[3L]]) else 1L
      }
      if (start < 0L || (!is.null(stop_idx) && stop_idx < 0L) || step <= 0L) {
        py_raise("ValueError", "Indices for islice() must be None or an integer: 0 <= x <= sys.maxsize.")
      }
      it <- py_iter(iterable)
      cur <- 0L
      next_i <- start
      py_generator(function() {
        if (!is.null(stop_idx) && next_i >= stop_idx) return(py_stop_iteration())
        while (cur < next_i) {
          c0 <- py_next_cell(it)
          cur <<- cur + 1L
          if (c0$stopped) return(py_stop_iteration())
        }
        c1 <- py_next_cell(it)
        cur <<- cur + 1L
        if (c1$stopped) return(py_stop_iteration())
        next_i <<- next_i + step
        py_yield(c1$value)
      })
    },
    compress = function(data, selectors) {
      d_it <- py_iter(data)
      s_it <- py_iter(selectors)
      py_generator(function() {
        repeat {
          dc <- py_next_cell(d_it)
          sc <- py_next_cell(s_it)
          if (dc$stopped || sc$stopped) return(py_stop_iteration())
          if (py_bool(sc$value)) return(py_yield(dc$value))
        }
      })
    },
    dropwhile = function(predicate, iterable) {
      it <- py_iter(iterable)
      dropping <- TRUE
      py_generator(function() {
        repeat {
          cell <- py_next_cell(it)
          if (cell$stopped) return(py_stop_iteration())
          if (dropping) {
            if (!py_bool(py_call(predicate, list(cell$value)))) {
              dropping <<- FALSE
              return(py_yield(cell$value))
            }
          } else {
            return(py_yield(cell$value))
          }
        }
      })
    },
    takewhile = function(predicate, iterable) {
      it <- py_iter(iterable)
      done <- FALSE
      py_generator(function() {
        if (done) return(py_stop_iteration())
        cell <- py_next_cell(it)
        if (cell$stopped) return(py_stop_iteration())
        if (py_bool(py_call(predicate, list(cell$value)))) {
          return(py_yield(cell$value))
        }
        done <<- TRUE
        py_stop_iteration()
      })
    },
    filterfalse = function(predicate, iterable) {
      it <- py_iter(iterable)
      py_generator(function() {
        repeat {
          cell <- py_next_cell(it)
          if (cell$stopped) return(py_stop_iteration())
          keep <- if (is_py_none(predicate) || is.null(predicate)) {
            !py_bool(cell$value)
          } else {
            !py_bool(py_call(predicate, list(cell$value)))
          }
          if (keep) return(py_yield(cell$value))
        }
      })
    },
    accumulate = function(iterable, func = py_add, initial = NULL) {
      it <- py_iter(iterable)
      started <- FALSE
      has_initial <- !is.null(initial) && !is_py_none(initial)
      total <- initial
      py_generator(function() {
        if (!started) {
          started <<- TRUE
          if (has_initial) return(py_yield(total))
          c0 <- py_next_cell(it)
          if (c0$stopped) return(py_stop_iteration())
          total <<- c0$value
          return(py_yield(total))
        }
        c1 <- py_next_cell(it)
        if (c1$stopped) return(py_stop_iteration())
        total <<- py_call(func, list(total, c1$value))
        py_yield(total)
      })
    },
    starmap = function(func, iterable) {
      it <- py_iter(iterable)
      py_generator(function() {
        cell <- py_next_cell(it)
        if (cell$stopped) return(py_stop_iteration())
        py_yield(py_call(func, coerce_items(cell$value)))
      })
    },
    zip_longest = function(..., fillvalue = py_none()) {
      iters <- lapply(list(...), py_iter)
      if (length(iters) == 0L) {
        return(py_generator(function() py_stop_iteration()))
      }
      py_generator(function() {
        vals <- vector("list", length(iters))
        any_alive <- FALSE
        for (i in seq_along(iters)) {
          cell <- py_next_cell(iters[[i]])
          if (!cell$stopped) {
            any_alive <- TRUE
            vals[i] <- list(cell$value)
          } else {
            vals[i] <- list(fillvalue)
          }
        }
        if (!any_alive) return(py_stop_iteration())
        py_yield(py_tuple(vals))
      })
    },
    product = function(..., `repeat` = 1L) {
      pools <- lapply(list(...), py_collect)
      rep_n <- as.integer(`repeat`)
      pools <- rep(pools, times = rep_n)
      if (length(pools) == 0L) return(py_iter(list(py_tuple())))
      if (any(vapply(pools, length, integer(1)) == 0L)) return(py_iter(list()))
      res <- list(list())
      for (pool in pools) {
        nxt <- list()
        for (prefix in res) {
          for (y in pool) {
            nxt[[length(nxt) + 1L]] <- c(prefix, list(y))
          }
        }
        res <- nxt
      }
      py_iter(lapply(res, py_tuple))
    },
    permutations = function(iterable, r = NULL) {
      pool <- py_collect(iterable)
      n <- length(pool)
      r_int <- if (is.null(r) || is_py_none(r)) n else as.integer(r)
      if (r_int > n || r_int < 0L) return(py_iter(list()))
      if (r_int == 0L) return(py_iter(list(py_tuple())))
      gen_perms <- function(idxs, depth) {
        if (depth == r_int) return(list(py_tuple(pool[idxs])))
        out <- list()
        for (i in setdiff(seq_len(n), idxs)) {
          out <- c(out, gen_perms(c(idxs, i), depth + 1L))
        }
        out
      }
      py_iter(gen_perms(integer(), 0L))
    },
    combinations = function(iterable, r) {
      pool <- py_collect(iterable)
      n <- length(pool)
      r_int <- as.integer(r)
      if (r_int > n || r_int < 0L) return(py_iter(list()))
      if (r_int == 0L) return(py_iter(list(py_tuple())))
      idx_mat <- utils::combn(n, r_int, simplify = FALSE)
      py_iter(lapply(idx_mat, function(ix) py_tuple(pool[ix])))
    },
    combinations_with_replacement = function(iterable, r) {
      pool <- py_collect(iterable)
      n <- length(pool)
      r_int <- as.integer(r)
      if (r_int < 0L || (n == 0L && r_int > 0L)) return(py_iter(list()))
      if (r_int == 0L) return(py_iter(list(py_tuple())))
      gen_cwr <- function(start_i, depth, acc) {
        if (depth == r_int) return(list(py_tuple(pool[acc])))
        out <- list()
        for (i in start_i:n) {
          out <- c(out, gen_cwr(i, depth + 1L, c(acc, i)))
        }
        out
      }
      py_iter(gen_cwr(1L, 0L, integer()))
    },
    groupby = function(iterable, key = NULL) {
      items <- py_collect(iterable)
      if (length(items) == 0L) return(py_iter(list()))
      groups <- list()
      cur_key <- if (is.null(key) || is_py_none(key)) items[[1L]] else py_call(key, list(items[[1L]]))
      cur_vals <- list(items[[1L]])
      if (length(items) > 1L) {
        for (i in 2:length(items)) {
          k_i <- if (is.null(key) || is_py_none(key)) items[[i]] else py_call(key, list(items[[i]]))
          if (py_eq(k_i, cur_key)) {
            cur_vals[length(cur_vals) + 1L] <- list(items[[i]])
          } else {
            groups[[length(groups) + 1L]] <- py_tuple(list(cur_key, py_iter(cur_vals)))
            cur_key <- k_i
            cur_vals <- list(items[[i]])
          }
        }
      }
      groups[[length(groups) + 1L]] <- py_tuple(list(cur_key, py_iter(cur_vals)))
      py_iter(groups)
    },
    tee = function(iterable, n = 2L) {
      n_int <- as.integer(n)
      it <- py_iter(iterable)
      buffer <- list()
      make_branch <- function() {
        pos <- 1L
        py_generator(function() {
          if (pos > length(buffer)) {
            cell <- py_next_cell(it)
            if (cell$stopped) return(py_stop_iteration())
            buffer[length(buffer) + 1L] <<- list(cell$value)
          }
          val <- buffer[[pos]]
          pos <<- pos + 1L
          py_yield(val)
        })
      }
      py_tuple(lapply(seq_len(n_int), function(i) make_branch()))
    },
    batched = function(iterable, n) {
      n_int <- as.integer(n)
      if (n_int < 1L) py_raise("ValueError", "n must be at least one")
      it <- py_iter(iterable)
      py_generator(function() {
        batch <- list()
        for (i in seq_len(n_int)) {
          cell <- py_next_cell(it)
          if (cell$stopped) break
          batch[length(batch) + 1L] <- list(cell$value)
        }
        if (length(batch) == 0L) return(py_stop_iteration())
        py_yield(py_tuple(batch))
      })
    },
    pairwise = function(iterable) {
      it <- py_iter(iterable)
      started <- FALSE
      prev <- NULL
      py_generator(function() {
        if (!started) {
          started <<- TRUE
          c0 <- py_next_cell(it)
          if (c0$stopped) return(py_stop_iteration())
          prev <<- c0$value
        }
        c1 <- py_next_cell(it)
        if (c1$stopped) return(py_stop_iteration())
        pair <- py_tuple(list(prev, c1$value))
        prev <<- c1$value
        py_yield(pair)
      })
    }
  )
}

#' @export
`$.pyrt_chain_callable` <- function(x, name) {
  attr(x, name, exact = TRUE)
}

# ---- 3. `functools` ---------------------------------------------------------

build_functools_module <- function() {
  lru_cache_impl <- function(maxsize = 128L, typed = FALSE) {
    # Support both `@lru_cache` (direct function argument) and `@lru_cache(maxsize=...)`
    if (is.function(maxsize)) {
      user_fn <- maxsize
      return(lru_cache_impl(128L, FALSE)(user_fn))
    }
    function(user_fn) {
      store <- new.env(hash = TRUE, parent = emptyenv())
      hits <- 0L
      misses <- 0L
      wrapper <- function(...) {
        args <- list(...)
        key <- paste(vapply(args, py_repr, character(1)), collapse = "\x1f")
        if (exists(key, envir = store, inherits = FALSE)) {
          hits <<- hits + 1L
          return(get(key, envir = store, inherits = FALSE))
        }
        misses <<- misses + 1L
        val <- user_fn(...)
        assign(key, val, envir = store)
        val
      }
      attr(wrapper, "cache_clear") <- function() {
        rm(list = ls(store, all.names = TRUE), envir = store)
        hits <<- 0L
        misses <<- 0L
        invisible(NULL)
      }
      attr(wrapper, "cache_info") <- function() {
        py_tuple(list(hits, misses, maxsize, length(ls(store, all.names = TRUE))))
      }
      class(wrapper) <- c("pyrt_lru_wrapped", "function")
      wrapper
    }
  }

  make_mod_env(
    reduce = function(func, iterable, initializer = missing_sentinel) {
      items <- py_collect(iterable)
      if (inherits(initializer, "pyrt_missing")) {
        if (length(items) == 0L) {
          py_raise("TypeError", "reduce() of empty iterable with no initial value")
        }
        acc <- items[[1L]]
        start_i <- 2L
      } else {
        acc <- initializer
        start_i <- 1L
      }
      if (start_i <= length(items)) {
        for (i in start_i:length(items)) {
          acc <- py_call(func, list(acc, items[[i]]))
        }
      }
      acc
    },
    partial = function(func, ...) {
      bound_args <- list(...)
      function(...) {
        call_args <- c(bound_args, list(...))
        do.call(func, call_args)
      }
    },
    lru_cache = lru_cache_impl,
    cache = function(user_fn) lru_cache_impl(NULL, FALSE)(user_fn),
    wraps = function(wrapped) function(fn) fn,
    total_ordering = function(cls) {
      env <- unwrap_cls(cls)
      if (!("__le__" %in% names(env$dict)) && ("__lt__" %in% names(env$dict))) {
        env$dict$`__le__` <- function(self, other) py_lt(self, other) || py_eq(self, other)
      }
      if (!("__gt__" %in% names(env$dict)) && ("__lt__" %in% names(env$dict))) {
        env$dict$`__gt__` <- function(self, other) (!py_lt(self, other)) && (!py_eq(self, other))
      }
      if (!("__ge__" %in% names(env$dict)) && ("__lt__" %in% names(env$dict))) {
        env$dict$`__ge__` <- function(self, other) !py_lt(self, other)
      }
      cls
    },
    cmp_to_key = function(mycmp) {
      KeyCls <- py_class("K", dict = list(
        `__init__` = function(self, obj) py_setattr(self, "obj", obj),
        `__lt__` = function(self, other) py_lt(py_call(mycmp, list(py_getattr(self, "obj"), py_getattr(other, "obj"))), 0L),
        `__eq__` = function(self, other) py_eq(py_call(mycmp, list(py_getattr(self, "obj"), py_getattr(other, "obj"))), 0L)
      ))
      KeyCls
    }
  )
}

#' @export
`$.pyrt_lru_wrapped` <- function(x, name) {
  attr(x, name, exact = TRUE)
}

# ---- 4. `operator` ----------------------------------------------------------

build_operator_module <- function() {
  make_mod_env(
    add = py_add,
    sub = py_sub,
    mul = py_mul,
    truediv = py_truediv,
    floordiv = py_floordiv,
    mod = py_mod,
    pow = py_pow,
    neg = py_neg,
    pos = py_pos,
    abs = py_abs,
    invert = py_invert,
    inv = py_invert,
    and_ = py_bitand,
    or_ = py_bitor,
    xor = py_bitxor,
    lshift = py_lshift,
    rshift = py_rshift,
    eq = py_eq,
    ne = py_ne,
    lt = py_lt,
    le = py_le,
    gt = py_gt,
    ge = py_ge,
    not_ = function(a) !py_bool(a),
    truth = py_bool,
    is_ = py_is,
    is_not = function(a, b) !py_is(a, b),
    contains = function(a, b) py_in(b, a),
    getitem = py_getitem,
    setitem = py_setitem,
    delitem = py_delitem,
    index = py_int,
    itemgetter = function(...) {
      keys <- list(...)
      if (length(keys) == 1L) {
        k <- keys[[1L]]
        function(obj) py_getitem(obj, k)
      } else {
        function(obj) py_tuple(lapply(keys, function(k) py_getitem(obj, k)))
      }
    },
    attrgetter = function(...) {
      attrs <- as.character(list(...))
      resolve_one <- function(obj, path) {
        cur <- obj
        for (part in strsplit(path, ".", fixed = TRUE)[[1L]]) {
          cur <- py_getattr(cur, part)
        }
        cur
      }
      if (length(attrs) == 1L) {
        p <- attrs[[1L]]
        function(obj) resolve_one(obj, p)
      } else {
        function(obj) py_tuple(lapply(attrs, function(p) resolve_one(obj, p)))
      }
    },
    methodcaller = function(name, ...) {
      bound <- list(...)
      function(obj) {
        m <- py_getattr(obj, name)
        do.call(m, bound)
      }
    }
  )
}

# ---- 5. `collections` -------------------------------------------------------

build_collections_module <- function() {
  deque_fn <- function(iterable = NULL, maxlen = NULL) {
    self <- new.env(parent = emptyenv())
    items <- if (is.null(iterable)) list() else py_collect(iterable)
    ml <- if (is.null(maxlen) || is_py_none(maxlen)) NULL else as.integer(maxlen)
    if (!is.null(ml) && ml >= 0L && length(items) > ml) {
      items <- if (ml == 0L) list() else items[seq.int(length(items) - ml + 1L, length(items))]
    }
    self$.items <- items
    self$maxlen <- if (is.null(ml)) py_none() else ml

    trim_maxlen <- function(from_left = TRUE) {
      if (!is.null(ml) && length(self$.items) > ml) {
        if (ml == 0L) {
          self$.items <- list()
        } else if (from_left) {
          self$.items <- self$.items[seq.int(length(self$.items) - ml + 1L, length(self$.items))]
        } else {
          self$.items <- self$.items[seq_len(ml)]
        }
      }
    }

    self$append <- function(x) {
      self$.items[length(self$.items) + 1L] <- list(x)
      trim_maxlen(from_left = TRUE)
      invisible(py_none())
    }
    self$appendleft <- function(x) {
      self$.items <- c(list(x), self$.items)
      trim_maxlen(from_left = FALSE)
      invisible(py_none())
    }
    self$pop <- function() {
      n <- length(self$.items)
      if (n == 0L) py_raise("IndexError", "pop from an empty deque")
      val <- self$.items[[n]]
      self$.items <- self$.items[-n]
      val
    }
    self$popleft <- function() {
      n <- length(self$.items)
      if (n == 0L) py_raise("IndexError", "pop from an empty deque")
      val <- self$.items[[1L]]
      self$.items <- self$.items[-1L]
      val
    }
    self$extend <- function(it) {
      for (v in py_collect(it)) self$append(v)
      invisible(py_none())
    }
    self$extendleft <- function(it) {
      for (v in py_collect(it)) self$appendleft(v)
      invisible(py_none())
    }
    self$rotate <- function(n = 1L) {
      len <- length(self$.items)
      if (len <= 1L) return(invisible(py_none()))
      k <- py_mod(as.integer(n), len)
      if (k > 0L) {
        self$.items <- c(
          self$.items[seq.int(len - k + 1L, len)],
          self$.items[seq_len(len - k)]
        )
      }
      invisible(py_none())
    }
    self$clear <- function() {
      self$.items <- list()
      invisible(py_none())
    }
    self$copy <- function() deque_fn(self$.items, maxlen = ml)
    self$as_r_list <- function() self$.items
    class(self) <- c("pyrt_deque", "pyrt_list", "pyrt_container")
    self
  }

  defaultdict_fn <- function(default_factory = NULL, ...) {
    d <- py_builtin_dict(...)
    orig_get <- d$getitem
    d$default_factory <- default_factory
    d$getitem <- function(key) {
      if (d$contains(key)) return(orig_get(key))
      if (is.null(d$default_factory) || is_py_none(d$default_factory)) {
        py_raise("KeyError", py_repr(key))
      }
      val <- py_call(d$default_factory, list())
      d$set(key, val)
      val
    }
    class(d) <- c("pyrt_defaultdict", class(d))
    d
  }

  counter_fn <- function(iterable = NULL, ...) {
    d <- py_dict()
    if (!is.null(iterable)) {
      if (inherits(iterable, "pyrt_dict")) {
        for (k in iterable$.keys) d$set(k, iterable$getitem(k))
      } else {
        for (item in py_collect(iterable)) {
          cur <- if (d$contains(item)) d$getitem(item) else 0L
          d$set(item, py_add(cur, 1L))
        }
      }
    }
    kw <- list(...)
    if (length(kw) > 0L) {
      nms <- names(kw)
      for (i in seq_along(kw)) d$set(nms[[i]], kw[[i]])
    }
    orig_get <- d$getitem
    d$getitem <- function(key) {
      if (d$contains(key)) orig_get(key) else 0L
    }
    d$most_common <- function(n = NULL) {
      ord <- order(as.numeric(d$.vals), decreasing = TRUE)
      if (!is.null(n) && !is_py_none(n)) {
        ord <- ord[seq_len(min(as.integer(n), length(ord)))]
      }
      py_list(lapply(ord, function(i) py_tuple(list(d$.keys[[i]], d$.vals[[i]]))))
    }
    d$total <- function() sum(as.integer(d$.vals))
    class(d) <- c("pyrt_Counter", class(d))
    d
  }

  namedtuple_fn <- function(typename, field_names) {
    fields <- if (is.character(field_names) && length(field_names) == 1L) {
       Filter(nzchar, unlist(strsplit(field_names, "[, ]+")))
    } else {
      as.character(coerce_items(field_names))
    }
    ctor <- function(...) {
      args <- list(...)
      nms <- names(args)
      vals <- vector("list", length(fields))
      for (i in seq_along(fields)) {
        fnm <- fields[[i]]
        if (!is.null(nms) && fnm %in% nms) {
          vals[i] <- list(args[[fnm]])
        } else if (i <= length(args)) {
          vals[i] <- list(args[[i]])
        } else {
          py_raise("TypeError", sprintf("%s() missing required argument: '%s'", typename, fnm))
        }
      }
      tup <- py_tuple(vals)
      attr(tup, "nt_typename") <- typename
      attr(tup, "nt_fields") <- fields
      class(tup) <- c("pyrt_namedtuple", class(tup))
      tup
    }
    ctor
  }

  make_mod_env(
    deque = deque_fn,
    defaultdict = defaultdict_fn,
    Counter = counter_fn,
    OrderedDict = py_builtin_dict,
    namedtuple = namedtuple_fn
  )
}

#' @export
`$.pyrt_namedtuple` <- function(x, name) {
  fields <- attr(x, "nt_fields", exact = TRUE)
  if (name == "_fields") return(py_tuple(as.list(fields)))
  if (name == "_asdict") {
    return(function() {
      items <- get("items", envir = x, inherits = FALSE)
      d <- py_dict()
      for (i in seq_along(fields)) d$set(fields[[i]], items[[i]])
      d
    })
  }
  idx <- match(name, fields)
  if (!is.na(idx)) {
    items <- get("items", envir = x, inherits = FALSE)
    return(items[[idx]])
  }
  NextMethod("$")
}

# ---- 6. `re` ----------------------------------------------------------------

build_re_module <- function() {
  make_match_obj <- function(text, m_start, m_len, captures_start = NULL, captures_len = NULL) {
    self <- new.env(parent = emptyenv())
    s0 <- as.integer(m_start) - 1L
    e0 <- s0 + as.integer(m_len)
    full_str <- substr(text, m_start, m_start + m_len - 1L)
    groups_vec <- if (!is.null(captures_start) && length(captures_start) > 0L) {
      vapply(seq_along(captures_start), function(i) {
        cs <- captures_start[[i]]
        cl <- captures_len[[i]]
        if (cs <= 0L) NA_character_ else substr(text, cs, cs + cl - 1L)
      }, character(1))
    } else {
      character()
    }
    self$group <- function(idx = 0L) {
      idx <- as.integer(idx)
      if (idx == 0L) return(full_str)
      val <- groups_vec[[idx]]
      if (is.na(val)) py_none() else val
    }
    self$groups <- function(default = py_none()) {
      py_tuple(lapply(groups_vec, function(g) if (is.na(g)) default else g))
    }
    self$start <- function(idx = 0L) s0
    self$end <- function(idx = 0L) e0
    self$span <- function(idx = 0L) py_tuple(list(s0, e0))
    class(self) <- "pyrt_re_Match"
    self
  }

  compile_fn <- function(pattern, flags = 0L) {
    pat_obj <- new.env(parent = emptyenv())
    pat_obj$pattern <- pattern
    pat_obj$flags <- as.integer(flags)
    ig_case <- bitwAnd(pat_obj$flags, 2L) != 0L

    pat_obj$search <- function(string) {
      m <- regexpr(pattern, string, perl = TRUE, ignore.case = ig_case)
      if (m[[1L]] < 0L) return(py_none())
      make_match_obj(
        string, m[[1L]], attr(m, "match.length")[[1L]],
        attr(m, "capture.start"), attr(m, "capture.length")
      )
    }
    pat_obj$match <- function(string) {
      anchored <- if (startsWith(pattern, "^")) pattern else paste0("^(?:", pattern, ")")
      m <- regexpr(anchored, string, perl = TRUE, ignore.case = ig_case)
      if (m[[1L]] < 0L) return(py_none())
      make_match_obj(
        string, m[[1L]], attr(m, "match.length")[[1L]],
        attr(m, "capture.start"), attr(m, "capture.length")
      )
    }
    pat_obj$fullmatch <- function(string) {
      anchored <- paste0("^(?:", pattern, ")\\z")
      m <- regexpr(anchored, string, perl = TRUE, ignore.case = ig_case)
      if (m[[1L]] < 0L) return(py_none())
      make_match_obj(
        string, m[[1L]], attr(m, "match.length")[[1L]],
        attr(m, "capture.start"), attr(m, "capture.length")
      )
    }
    pat_obj$findall <- function(string) {
      m <- gregexpr(pattern, string, perl = TRUE, ignore.case = ig_case)[[1L]]
      if (length(m) == 1L && m[[1L]] < 0L) return(py_list())
      hits <- regmatches(string, list(m))[[1L]]
      py_list(as.list(hits))
    }
    pat_obj$sub <- function(repl, string, count = 0L) {
      if (as.integer(count) == 1L) {
        sub(pattern, repl, string, perl = TRUE, ignore.case = ig_case)
      } else {
        gsub(pattern, repl, string, perl = TRUE, ignore.case = ig_case)
      }
    }
    pat_obj$split <- function(string, maxsplit = 0L) {
      parts <- strsplit(string, pattern, perl = TRUE)[[1L]]
      py_list(as.list(parts))
    }
    class(pat_obj) <- "pyrt_re_Pattern"
    pat_obj
  }

  make_mod_env(
    IGNORECASE = 2L,
    I = 2L,
    MULTILINE = 8L,
    M = 8L,
    DOTALL = 16L,
    S = 16L,
    compile = compile_fn,
    search = function(pattern, string, flags = 0L) compile_fn(pattern, flags)$search(string),
    match = function(pattern, string, flags = 0L) compile_fn(pattern, flags)$match(string),
    fullmatch = function(pattern, string, flags = 0L) compile_fn(pattern, flags)$fullmatch(string),
    findall = function(pattern, string, flags = 0L) compile_fn(pattern, flags)$findall(string),
    sub = function(pattern, repl, string, count = 0L, flags = 0L) compile_fn(pattern, flags)$sub(repl, string, count),
    split = function(pattern, string, maxsplit = 0L, flags = 0L) compile_fn(pattern, flags)$split(string, maxsplit),
    escape = function(pattern) gsub("([.\\+*?\\[\\^\\]$(){}=!<>|:\\-#])", "\\\\\\1", pattern, perl = TRUE)
  )
}

# ---- 7. `json` --------------------------------------------------------------

build_json_module <- function() {
  serialize_json <- function(x) {
    if (is_py_none(x)) return("null")
    if (is.logical(x) && length(x) == 1L) return(if (isTRUE(x)) "true" else "false")
    if (is_int_like(x)) return(as.character(x))
    if (is.numeric(x) && length(x) == 1L) return(fmt_py_float(x))
    if (is.character(x) && length(x) == 1L) {
      s <- gsub("\\", "\\\\", x, fixed = TRUE)
      s <- gsub('"', '\\"', s, fixed = TRUE)
      s <- gsub("\n", "\\n", s, fixed = TRUE)
      s <- gsub("\r", "\\r", s, fixed = TRUE)
      s <- gsub("\t", "\\t", s, fixed = TRUE)
      return(paste0('"', s, '"'))
    }
    if (inherits(x, "pyrt_list") || inherits(x, "pyrt_tuple")) {
      elts <- vapply(x$as_r_list(), serialize_json, character(1))
      return(paste0("[", paste(elts, collapse = ", "), "]"))
    }
    if (inherits(x, "pyrt_dict")) {
      pairs <- vapply(seq_along(x$.keys), function(i) {
        paste0(serialize_json(as.character(x$.keys[[i]])), ": ", serialize_json(x$.vals[[i]]))
      }, character(1))
      return(paste0("{", paste(pairs, collapse = ", "), "}"))
    }
    py_raise("TypeError", "Object is not JSON serializable")
  }

  # Lightweight recursive-descent JSON parser returning `py_dict` / `py_list` /
  # `py_none()` / `integer` / `double` / `logical` / `character` without external deps!
  parse_json_str <- function(s) {
    pos <- 1L
    n <- nchar(s)
    skip_ws <- function() {
      while (pos <= n && substr(s, pos, pos) %in% c(" ", "\t", "\r", "\n")) pos <<- pos + 1L
    }
    parse_val <- function() {
      skip_ws()
      if (pos > n) py_raise("ValueError", "Unexpected end of JSON input")
      ch <- substr(s, pos, pos)
      if (ch == "{") {
        pos <<- pos + 1L
        d <- py_dict()
        skip_ws()
        if (pos <= n && substr(s, pos, pos) == "}") {
          pos <<- pos + 1L
          return(d)
        }
        repeat {
          k <- parse_val()
          skip_ws()
          if (substr(s, pos, pos) != ":") py_raise("ValueError", "Expected ':' in JSON object")
          pos <<- pos + 1L
          v <- parse_val()
          d$set(k, v)
          skip_ws()
          sep <- substr(s, pos, pos)
          if (sep == "}") {
            pos <<- pos + 1L
            return(d)
          }
          if (sep != ",") py_raise("ValueError", "Expected ',' or '}' in JSON object")
          pos <<- pos + 1L
        }
      }
      if (ch == "[") {
        pos <<- pos + 1L
        items <- list()
        skip_ws()
        if (pos <= n && substr(s, pos, pos) == "]") {
          pos <<- pos + 1L
          return(py_list())
        }
        repeat {
          items[length(items) + 1L] <- list(parse_val())
          skip_ws()
          sep <- substr(s, pos, pos)
          if (sep == "]") {
            pos <<- pos + 1L
            return(py_list(items))
          }
          if (sep != ",") py_raise("ValueError", "Expected ',' or ']' in JSON array")
          pos <<- pos + 1L
        }
      }
      if (ch == '"') {
        rem <- substring(s, pos)
        m <- regmatches(rem, regexpr('^"(?:[^"\\\\]|\\\\.)*"', rem, perl = TRUE))
        if (length(m) == 0L) py_raise("ValueError", "Unterminated JSON string")
        pos <<- pos + nchar(m)
        inner <- substr(m, 2L, nchar(m) - 1L)
        inner <- gsub('\\"', '"', inner, fixed = TRUE)
        inner <- gsub("\\n", "\n", inner, fixed = TRUE)
        inner <- gsub("\\r", "\r", inner, fixed = TRUE)
        inner <- gsub("\\t", "\t", inner, fixed = TRUE)
        inner <- gsub("\\\\", "\\", inner, fixed = TRUE)
        return(inner)
      }
      if (substr(s, pos, pos + 3L) == "true") {
        pos <<- pos + 4L
        return(TRUE)
      }
      if (substr(s, pos, pos + 4L) == "false") {
        pos <<- pos + 5L
        return(FALSE)
      }
      if (substr(s, pos, pos + 3L) == "null") {
        pos <<- pos + 4L
        return(py_none())
      }
      rem <- substring(s, pos)
      m <- regmatches(rem, regexpr("^-?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?", rem, perl = TRUE))
      if (length(m) == 0L) py_raise("ValueError", "Invalid JSON token")
      pos <<- pos + nchar(m)
      if (grepl("[.eE]", m)) as.double(m) else py_int(m)
    }
    parse_val()
  }

  make_mod_env(
    dumps = function(obj, indent = NULL, sort_keys = FALSE) serialize_json(obj),
    loads = function(s) parse_json_str(s)
  )
}

# ---- 8. `copy`, `heapq`, `bisect`, `string`, and misc stdlib stubs ----------

build_copy_module <- function() {
  deepcopy_fn <- function(x) {
    if (inherits(x, "pyrt_list")) return(py_list(lapply(x$.items, deepcopy_fn)))
    if (inherits(x, "pyrt_tuple")) return(py_tuple(lapply(x$items, deepcopy_fn)))
    if (inherits(x, "pyrt_set")) return(py_set(lapply(x$.items, deepcopy_fn)))
    if (inherits(x, "pyrt_dict")) {
      d <- py_dict()
      for (i in seq_along(x$.keys)) {
        d$set(deepcopy_fn(x$.keys[[i]]), deepcopy_fn(x$.vals[[i]]))
      }
      return(d)
    }
    x
  }
  make_mod_env(
    copy = function(x) {
      if (inherits(x, "pyrt_container") && !is.null(x$copy)) return(x$copy())
      x
    },
    deepcopy = deepcopy_fn
  )
}

build_bisect_module <- function() {
  bisect_left_fn <- function(a, x, lo = 0L, hi = NULL) {
    items <- coerce_items(a)
    lo <- as.integer(lo)
    hi <- if (is.null(hi) || is_py_none(hi)) length(items) else as.integer(hi)
    while (lo < hi) {
      mid <- (lo + hi) %/% 2L
      if (py_lt(items[[mid + 1L]], x)) lo <- mid + 1L else hi <- mid
    }
    lo
  }
  bisect_right_fn <- function(a, x, lo = 0L, hi = NULL) {
    items <- coerce_items(a)
    lo <- as.integer(lo)
    hi <- if (is.null(hi) || is_py_none(hi)) length(items) else as.integer(hi)
    while (lo < hi) {
      mid <- (lo + hi) %/% 2L
      if (py_lt(x, items[[mid + 1L]])) hi <- mid else lo <- mid + 1L
    }
    lo
  }
  make_mod_env(
    bisect_left = bisect_left_fn,
    bisect_right = bisect_right_fn,
    bisect = bisect_right_fn,
    insort_left = function(a, x, lo = 0L, hi = NULL) {
      idx <- bisect_left_fn(a, x, lo, hi)
      a$insert(idx, x)
      invisible(py_none())
    },
    insort_right = function(a, x, lo = 0L, hi = NULL) {
      idx <- bisect_right_fn(a, x, lo, hi)
      a$insert(idx, x)
      invisible(py_none())
    },
    insort = function(a, x, lo = 0L, hi = NULL) {
      idx <- bisect_right_fn(a, x, lo, hi)
      a$insert(idx, x)
      invisible(py_none())
    }
  )
}

build_heapq_module <- function() {
  make_mod_env(
    heapify = function(x) {
      x$sort()
      invisible(py_none())
    },
    heappush = function(heap, item) {
      heap$append(item)
      heap$sort()
      invisible(py_none())
    },
    heappop = function(heap) {
      heap$pop(0L)
    },
    nlargest = function(n, iterable, key = NULL) {
      s <- py_builtin_sorted(iterable, key = key, reverse = TRUE)
      items <- s$as_r_list()
      py_list(items[seq_len(min(as.integer(n), length(items)))])
    },
    nsmallest = function(n, iterable, key = NULL) {
      s <- py_builtin_sorted(iterable, key = key, reverse = FALSE)
      items <- s$as_r_list()
      py_list(items[seq_len(min(as.integer(n), length(items)))])
    }
  )
}

build_string_module <- function() {
  make_mod_env(
    ascii_lowercase = "abcdefghijklmnopqrstuvwxyz",
    ascii_uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    ascii_letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    digits = "0123456789",
    hexdigits = "0123456789abcdefABCDEF",
    octdigits = "01234567",
    punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~",
    whitespace = " \t\n\r\x0b\x0c"
  )
}

build_datetime_module <- function() {
  is_leap_year <- function(y) {
    (y %% 4L == 0L && y %% 100L != 0L) || (y %% 400L == 0L)
  }

  days_in_month <- function(y, m) {
    dim_tbl <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
    if (m == 2L && is_leap_year(y)) 29L else dim_tbl[[m]]
  }

  # Proleptic Gregorian ordinal (0001-01-01 -> 1L), matching Python's date.toordinal()
  ymd_to_ordinal <- function(y, m, d) {
    y1 <- y - 1L
    days_before_year <- y1 * 365L + (y1 %/% 4L) - (y1 %/% 100L) + (y1 %/% 400L)
    days_before_month <- 0L
    if (m > 1L) {
      for (mm in seq_len(m - 1L)) days_before_month <- days_before_month + days_in_month(y, mm)
    }
    as.integer(days_before_year + days_before_month + d)
  }

  ordinal_to_ymd <- function(n) {
    # Inverse of ymd_to_ordinal via R Date offset from 1970-01-01 (ordinal 719163)
    r_date <- as.Date(as.integer(n) - 719163L, origin = "1970-01-01")
    parts <- as.integer(strsplit(format(r_date, "%Y-%m-%d"), "-", fixed = TRUE)[[1L]])
    list(year = parts[[1L]], month = parts[[2L]], day = parts[[3L]])
  }

  timedelta_cls <- py_class(
    "timedelta",
    dict = list(
      `__init__` = function(self, days = 0, seconds = 0, microseconds = 0,
                            milliseconds = 0, minutes = 0, hours = 0, weeks = 0) {
        total_us <- round(
          ((as.numeric(weeks) * 7 + as.numeric(days)) * 86400 +
             as.numeric(hours) * 3600 + as.numeric(minutes) * 60 +
             as.numeric(seconds)) * 1e6 +
            as.numeric(milliseconds) * 1e3 + as.numeric(microseconds)
        )
        us_rem <- total_us %% 1e6
        if (us_rem < 0) us_rem <- us_rem + 1e6
        total_sec <- floor((total_us - us_rem) / 1e6)
        sec_rem <- total_sec %% 86400
        if (sec_rem < 0) sec_rem <- sec_rem + 86400
        d_int <- as.integer(round((total_sec - sec_rem) / 86400))
        py_setattr(self, "days", d_int)
        py_setattr(self, "seconds", as.integer(round(sec_rem)))
        py_setattr(self, "microseconds", as.integer(round(us_rem)))
        py_setattr(self, ".total_us", total_us)
        py_none()
      },
      total_seconds = function(self) {
        py_getattr(self, ".total_us") / 1e6
      },
      `__add__` = function(self, other) {
        if (py_isinstance(other, timedelta_cls)) {
          return(py_call(timedelta_cls, list(microseconds = py_getattr(self, ".total_us") + py_getattr(other, ".total_us"))))
        }
        py_raise("TypeError", "unsupported operand type for timedelta +")
      },
      `__sub__` = function(self, other) {
        if (py_isinstance(other, timedelta_cls)) {
          return(py_call(timedelta_cls, list(microseconds = py_getattr(self, ".total_us") - py_getattr(other, ".total_us"))))
        }
        py_raise("TypeError", "unsupported operand type for timedelta -")
      },
      `__neg__` = function(self) {
        py_call(timedelta_cls, list(microseconds = -py_getattr(self, ".total_us")))
      },
      `__eq__` = function(self, other) {
        py_isinstance(other, timedelta_cls) && py_getattr(self, ".total_us") == py_getattr(other, ".total_us")
      },
      `__lt__` = function(self, other) {
        py_getattr(self, ".total_us") < py_getattr(other, ".total_us")
      },
      `__repr__` = function(self) {
        sprintf("datetime.timedelta(days=%d, seconds=%d)", py_getattr(self, "days"), py_getattr(self, "seconds"))
      }
    )
  )

  date_cls <- py_class(
    "date",
    dict = list(
      `__init__` = function(self, year, month, day) {
        y <- as.integer(year)
        m <- as.integer(month)
        d <- as.integer(day)
        if (m < 1L || m > 12L) py_raise("ValueError", "month must be in 1..12")
        if (d < 1L || d > days_in_month(y, m)) py_raise("ValueError", "day is out of range for month")
        py_setattr(self, "year", y)
        py_setattr(self, "month", m)
        py_setattr(self, "day", d)
        py_setattr(self, ".ordinal", ymd_to_ordinal(y, m, d))
        py_none()
      },
      isoformat = function(self) {
        sprintf("%04d-%02d-%02d", py_getattr(self, "year"), py_getattr(self, "month"), py_getattr(self, "day"))
      },
      toordinal = function(self) py_getattr(self, ".ordinal"),
      weekday = function(self) (py_getattr(self, ".ordinal") + 6L) %% 7L,
      isoweekday = function(self) ((py_getattr(self, ".ordinal") + 6L) %% 7L) + 1L,
      replace = function(self, year = NULL, month = NULL, day = NULL) {
        py_call(date_cls, list(
          if (is.null(year) || is_py_none(year)) py_getattr(self, "year") else year,
          if (is.null(month) || is_py_none(month)) py_getattr(self, "month") else month,
          if (is.null(day) || is_py_none(day)) py_getattr(self, "day") else day
        ))
      },
      `__add__` = function(self, other) {
        if (py_isinstance(other, timedelta_cls)) {
          ymd <- ordinal_to_ymd(py_getattr(self, ".ordinal") + py_getattr(other, "days"))
          return(py_call(date_cls, list(ymd$year, ymd$month, ymd$day)))
        }
        py_raise("TypeError", "unsupported operand for date +")
      },
      `__sub__` = function(self, other) {
        if (py_isinstance(other, timedelta_cls)) {
          ymd <- ordinal_to_ymd(py_getattr(self, ".ordinal") - py_getattr(other, "days"))
          return(py_call(date_cls, list(ymd$year, ymd$month, ymd$day)))
        }
        if (py_isinstance(other, date_cls)) {
          return(py_call(timedelta_cls, list(days = py_getattr(self, ".ordinal") - py_getattr(other, ".ordinal"))))
        }
        py_raise("TypeError", "unsupported operand for date -")
      },
      `__eq__` = function(self, other) {
        py_isinstance(other, date_cls) && !py_isinstance(other, datetime_cls) &&
          py_getattr(self, ".ordinal") == py_getattr(other, ".ordinal")
      },
      `__lt__` = function(self, other) {
        py_getattr(self, ".ordinal") < py_getattr(other, ".ordinal")
      },
      `__str__` = function(self) py_getattr(self, "isoformat")(),
      `__repr__` = function(self) {
        sprintf("datetime.date(%d, %d, %d)", py_getattr(self, "year"), py_getattr(self, "month"), py_getattr(self, "day"))
      },
      fromisoformat = py_staticmethod(function(s) {
        parts <- as.integer(strsplit(as.character(s), "-", fixed = TRUE)[[1L]])
        py_call(date_cls, list(parts[[1L]], parts[[2L]], parts[[3L]]))
      }),
      fromordinal = py_staticmethod(function(n) {
        ymd <- ordinal_to_ymd(n)
        py_call(date_cls, list(ymd$year, ymd$month, ymd$day))
      })
    )
  )

  time_cls <- py_class(
    "time",
    dict = list(
      `__init__` = function(self, hour = 0L, minute = 0L, second = 0L, microsecond = 0L) {
        py_setattr(self, "hour", as.integer(hour))
        py_setattr(self, "minute", as.integer(minute))
        py_setattr(self, "second", as.integer(second))
        py_setattr(self, "microsecond", as.integer(microsecond))
        py_none()
      },
      isoformat = function(self) {
        us <- py_getattr(self, "microsecond")
        base <- sprintf("%02d:%02d:%02d", py_getattr(self, "hour"), py_getattr(self, "minute"), py_getattr(self, "second"))
        if (us > 0L) sprintf("%s.%06d", base, us) else base
      },
      `__str__` = function(self) py_getattr(self, "isoformat")(),
      `__eq__` = function(self, other) {
        py_isinstance(other, time_cls) &&
          py_getattr(self, "hour") == py_getattr(other, "hour") &&
          py_getattr(self, "minute") == py_getattr(other, "minute") &&
          py_getattr(self, "second") == py_getattr(other, "second") &&
          py_getattr(self, "microsecond") == py_getattr(other, "microsecond")
      }
    )
  )

  datetime_cls <- py_class(
    "datetime",
    bases = list(date_cls),
    dict = list(
      `__init__` = function(self, year, month, day, hour = 0L, minute = 0L, second = 0L, microsecond = 0L) {
        y <- as.integer(year)
        m <- as.integer(month)
        d <- as.integer(day)
        if (m < 1L || m > 12L) py_raise("ValueError", "month must be in 1..12")
        if (d < 1L || d > days_in_month(y, m)) py_raise("ValueError", "day is out of range for month")
        ord <- ymd_to_ordinal(y, m, d)
        py_setattr(self, "year", y)
        py_setattr(self, "month", m)
        py_setattr(self, "day", d)
        py_setattr(self, "hour", as.integer(hour))
        py_setattr(self, "minute", as.integer(minute))
        py_setattr(self, "second", as.integer(second))
        py_setattr(self, "microsecond", as.integer(microsecond))
        py_setattr(self, ".ordinal", ord)
        total_us <- (as.numeric(ord) * 86400 + as.numeric(hour) * 3600 +
                       as.numeric(minute) * 60 + as.numeric(second)) * 1e6 + as.numeric(microsecond)
        py_setattr(self, ".epoch_us", total_us)
        py_none()
      },
      date = function(self) {
        py_call(date_cls, list(py_getattr(self, "year"), py_getattr(self, "month"), py_getattr(self, "day")))
      },
      time = function(self) {
        py_call(time_cls, list(py_getattr(self, "hour"), py_getattr(self, "minute"),
                               py_getattr(self, "second"), py_getattr(self, "microsecond")))
      },
      isoformat = function(self, sep = "T") {
        us <- py_getattr(self, "microsecond")
        base <- sprintf(
          "%04d-%02d-%02d%s%02d:%02d:%02d",
          py_getattr(self, "year"), py_getattr(self, "month"), py_getattr(self, "day"),
          sep, py_getattr(self, "hour"), py_getattr(self, "minute"), py_getattr(self, "second")
        )
        if (us > 0L) sprintf("%s.%06d", base, us) else base
      },
      `__add__` = function(self, other) {
        if (py_isinstance(other, timedelta_cls)) {
          new_us <- py_getattr(self, ".epoch_us") + py_getattr(other, ".total_us")
          us_rem <- new_us %% 1e6
          sec_total <- floor((new_us - us_rem) / 1e6)
          sec_day <- sec_total %% 86400
          ord <- as.integer(round((sec_total - sec_day) / 86400))
          ymd <- ordinal_to_ymd(ord)
          hh <- as.integer(sec_day) %/% 3600L
          mm <- (as.integer(sec_day) %% 3600L) %/% 60L
          ss <- as.integer(sec_day) %% 60L
          return(py_call(datetime_cls, list(ymd$year, ymd$month, ymd$day, hh, mm, ss, as.integer(us_rem))))
        }
        py_raise("TypeError", "unsupported operand for datetime +")
      },
      `__sub__` = function(self, other) {
        if (py_isinstance(other, timedelta_cls)) {
          return(py_add(self, py_neg(other)))
        }
        if (py_isinstance(other, datetime_cls)) {
          return(py_call(timedelta_cls, list(microseconds = py_getattr(self, ".epoch_us") - py_getattr(other, ".epoch_us"))))
        }
        py_raise("TypeError", "unsupported operand for datetime -")
      },
      `__eq__` = function(self, other) {
        py_isinstance(other, datetime_cls) && py_getattr(self, ".epoch_us") == py_getattr(other, ".epoch_us")
      },
      `__lt__` = function(self, other) {
        py_getattr(self, ".epoch_us") < py_getattr(other, ".epoch_us")
      },
      `__str__` = function(self) py_getattr(self, "isoformat")(" ")
    )
  )

  make_mod_env(
    date = date_cls,
    time = time_cls,
    datetime = datetime_cls,
    timedelta = timedelta_cls
  )
}

build_dataclasses_module <- function() {
  dataclass_fn <- function(cls = NULL, ...) {
    decorate <- function(target_cls) {
      env <- unwrap_cls(target_cls)
      # Collect class-level defaults (annotations or non-callable fields)
      field_names <- setdiff(names(env$dict), c("__init__", "__repr__", "__eq__"))
      field_names <- Filter(function(nm) !is.function(env$dict[[nm]]), field_names)
      if (!("__init__" %in% names(env$dict))) {
        defaults <- env$dict[field_names]
        env$dict$`__init__` <- function(self, ...) {
          args <- list(...)
          nms <- names(args)
          for (i in seq_along(field_names)) {
            fnm <- field_names[[i]]
            val <- if (!is.null(nms) && fnm %in% nms) {
              args[[fnm]]
            } else if (i <= length(args)) {
              args[[i]]
            } else {
              defaults[[fnm]]
            }
            py_setattr(self, fnm, val)
          }
          if (!is.null(nms)) {
            for (k in setdiff(nms[nzchar(nms)], field_names)) {
              py_setattr(self, k, args[[k]])
            }
          }
          if (is.null(nms) && length(args) > length(field_names)) {
            for (i in seq_along(args)) {
              py_setattr(self, paste0("field_", i), args[[i]])
            }
          }
          py_none()
        }
      }
      target_cls
    }
    if (!is.null(cls) && inherits(cls, "pyrt_class")) decorate(cls) else decorate
  }
  make_mod_env(
    dataclass = dataclass_fn,
    field = function(default = py_none(), default_factory = NULL, ...) {
      if (!is.null(default_factory) && !is_py_none(default_factory)) py_call(default_factory, list()) else default
    }
  )
}

build_enum_module <- function() {
  EnumCls <- py_class("Enum")
  IntEnumCls <- py_class("IntEnum", bases = list(EnumCls))
  make_mod_env(
    Enum = EnumCls,
    IntEnum = IntEnumCls,
    auto = function() 1L
  )
}

stdlib_cache <- new.env(hash = TRUE, parent = emptyenv())

#' Retrieve a Python standard-library module implemented in `pyrt`
#'
#' Returns an environment implementing the requested Python standard-library
#' module (`math`, `itertools`, `functools`, `operator`, `collections`, `re`,
#' `json`, `copy`, `bisect`, `heapq`, `string`, `datetime`, `dataclasses`,
#' `enum`, `random`, `sys`).
#'
#' @param name Module name string (e.g. `"math"`, `"itertools"`, `"collections"`).
#' @returns A `pyrt_module` environment.
#' @export
#' @examples
#' m <- py_stdlib("math")
#' m$gcd(48L, 18L)
#' it <- py_stdlib("itertools")
#' py_collect(it$islice(it$count(10L, 2L), 4L))
py_stdlib <- function(name) {
  parts <- strsplit(name, ".", fixed = TRUE)[[1L]]
  top <- parts[[1L]]
  if (identical(top, "random")) return(py_random)
  if (!exists(top, envir = stdlib_cache, inherits = FALSE)) {
    mod <- switch(top,
      math = build_math_module(),
      itertools = build_itertools_module(),
      functools = build_functools_module(),
      operator = build_operator_module(),
      collections = build_collections_module(),
      re = build_re_module(),
      json = build_json_module(),
      copy = build_copy_module(),
      bisect = build_bisect_module(),
      heapq = build_heapq_module(),
      string = build_string_module(),
      datetime = build_datetime_module(),
      dataclasses = build_dataclasses_module(),
      enum = build_enum_module(),
      numpy = build_numpy_module(),
      scipy = build_scipy_module(),
      pandas = build_pandas_module(),
      sys = make_mod_env(
        maxsize = gmp::as.bigz("9223372036854775807"),
        version_info = py_tuple(list(3L, 12L, 0L, "final", 0L))
      ),
      make_mod_env()
    )
    assign(top, mod, envir = stdlib_cache)
  }
  cur <- get(top, envir = stdlib_cache, inherits = FALSE)
  if (length(parts) > 1L) {
    for (p in parts[-1L]) {
      sub <- get0(p, envir = cur, inherits = FALSE)
      if (!is.null(sub)) cur <- sub
    }
  }
  cur
}

#' @rdname py_stdlib
#' @export
py_stdlib_names <- function() {
  c(
    "math", "itertools", "functools", "operator", "collections", "re",
    "json", "copy", "bisect", "heapq", "string", "datetime", "dataclasses",
    "enum", "random", "sys", "numpy", "scipy", "pandas",
    "typing", "types", "abc", "warnings", "numbers"
  )
}

