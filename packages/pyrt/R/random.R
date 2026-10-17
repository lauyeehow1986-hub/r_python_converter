# Bit-exact CPython `random` module (`Random` class + default module-level
# instance `py_random`), backed by `src/random.c` (MT19937 + SHA-512 version=2
# seeding) and `gmp::bigz` for arbitrary-precision `getrandbits` / `randrange`.

# Decompose a non-negative `gmp::bigz` into 32-bit unsigned words
# (little-endian word order, matching CPython's `_PyLong_AsByteArray` /
# `random_seed`).
bigz_to_uint32_words <- function(z) {
  az <- abs(as_bigz_exact(z))
  if (all(az == 0L)) return(0)
  two32 <- gmp::pow.bigz(gmp::as.bigz(2L), 32L)
  words <- numeric()
  while (az > 0L) {
    rem <- az %% two32
    words[[length(words) + 1L]] <- as.double(rem)
    az <- gmp::divq.bigz(az, two32)
  }
  words
}

# Reconstruct an arbitrary-precision integer from 32-bit unsigned words
# (little-endian word order) returned by `ffi_mt_getrandbits_words`.
uint32_words_to_py_int <- function(words) {
  n <- length(words)
  if (n == 0L) return(0L)
  if (n == 1L && words[[1L]] <= 2147483647) {
    return(as.integer(words[[1L]]))
  }
  two32 <- gmp::pow.bigz(gmp::as.bigz(2L), 32L)
  acc <- gmp::as.bigz(0L)
  mult <- gmp::as.bigz(1L)
  for (w in words) {
    acc <- acc + gmp::as.bigz(sprintf("%.0f", w)) * mult
    mult <- mult * two32
  }
  normalize_py_int(acc)
}

#' Bit-exact CPython `random.Random` generator and module-level `py_random`
#'
#' `py_random_generator(seed)` creates an independent `random.Random` instance
#' whose Mersenne Twister (`MT19937`) state, `version = 2` seeding (for `int`,
#' `gmp::bigz`, `bool`, `str`, and `bytes`), `random()`, `getrandbits()`,
#' `_randbelow()`, `randrange()`, `randint()`, `choice()`, `shuffle()`,
#' `sample()`, `choices()`, `uniform()`, `triangular()`, `gauss()`,
#' `normalvariate()`, `expovariate()`, `lognormvariate()`, `getstate()`, and
#' `setstate()` match CPython's `random` module bit-for-bit. R's own RNG is
#' never touched. `py_random` is a pre-instantiated module-like object exposing
#' the same functions at top level (`py_random$seed(42L)`,
#' `py_random$random()`, etc.).
#'
#' @param a Optional seed (`NULL`, integer, `gmp::bigz`, logical, character
#'   string, or `raw` bytes).
#' @returns `py_random_generator()` returns a `pyrt_Random` environment;
#'   `py_random` is the default module-level generator.
#' @export
#' @examples
#' rng <- py_random_generator(12345L)
#' rng$random()
#' rng$randint(1L, 100L)
#' rng$getrandbits(100L)
py_random_generator <- function(a = NULL) {
  self <- new.env(parent = emptyenv())
  self$.ptr <- .Call(ffi_mt_new)

  self$seed <- function(a = NULL, version = 2L) {
    if (is.null(a)) {
      t_words <- as.double(as.integer(Sys.time())) + as.double(Sys.getpid()) * 65537
      .Call(ffi_mt_seed_words, self$.ptr, t_words)
      return(invisible(NULL))
    }
    if (is.logical(a) && length(a) == 1L) {
      a <- if (isTRUE(a)) 1L else 0L
    }
    if (is_int_like(a)) {
      words <- bigz_to_uint32_words(a)
      .Call(ffi_mt_seed_words, self$.ptr, words)
      return(invisible(NULL))
    }
    if (is.character(a) && length(a) == 1L) {
      raw_b <- charToRaw(enc2utf8(a))
      .Call(ffi_mt_seed_bytes, self$.ptr, raw_b)
      return(invisible(NULL))
    }
    if (is.raw(a)) {
      .Call(ffi_mt_seed_bytes, self$.ptr, a)
      return(invisible(NULL))
    }
    py_raise("TypeError", "The only supported seed types are: None, int, str, bytes")
  }

  self$random <- function() {
    .Call(ffi_mt_random, self$.ptr, 1L)
  }

  self$getrandbits <- function(k) {
    k <- as.integer(k)
    if (k < 0L) py_raise("ValueError", "number of bits must be non-negative")
    if (k == 0L) return(0L)
    words <- .Call(ffi_mt_getrandbits_words, self$.ptr, k)
    uint32_words_to_py_int(words)
  }

  self$`_randbelow` <- function(n) {
    if (!gmp::is.bigz(n) && is.numeric(n) && length(n) == 1L && n >= 1 && n <= 9007199254740992) {
      r <- .Call(ffi_mt_randbelow, self$.ptr, as.double(n))
      if (r <= 2147483647) return(as.integer(r))
      return(normalize_py_int(gmp::as.bigz(sprintf("%.0f", r))))
    }
    zn <- as_bigz_exact(n)
    if (zn <= 1L) return(0L)
    k <- gmp::sizeinbase(zn - 1L, 2L)
    repeat {
      r <- as_bigz_exact(self$getrandbits(k))
      if (r < zn) return(normalize_py_int(r))
    }
  }

  self$randrange <- function(start, stop = NULL, step = 1L) {
    if (is.null(stop)) {
      istart <- as_bigz_exact(start)
      if (istart <= 0L) py_raise("ValueError", "empty range for randrange()")
      return(self$`_randbelow`(istart))
    }
    istart <- as_bigz_exact(start)
    istop <- as_bigz_exact(stop)
    istep <- as_bigz_exact(step)
    width <- istop - istart
    if (istep == 1L) {
      if (width <= 0L) py_raise("ValueError", "empty range for randrange()")
      return(normalize_py_int(istart + as_bigz_exact(self$`_randbelow`(width))))
    }
    if (istep > 0L) {
      n <- py_floordiv(width + istep - 1L, istep)
    } else if (istep < 0L) {
      n <- py_floordiv(width + istep + 1L, istep)
    } else {
      py_raise("ValueError", "zero step for randrange()")
    }
    if (n <= 0L) py_raise("ValueError", "empty range in randrange()")
    normalize_py_int(istart + istep * as_bigz_exact(self$`_randbelow`(n)))
  }

  self$randint <- function(a, b) {
    self$randrange(as_bigz_exact(a), as_bigz_exact(b) + 1L)
  }

  self$choice <- function(seq) {
    n <- py_len(seq)
    if (n == 0L) py_raise("IndexError", "Cannot choose from an empty sequence")
    idx <- self$`_randbelow`(n)
    py_getitem(seq, idx)
  }

  self$shuffle <- function(x) {
    if (!inherits(x, "pyrt_list")) {
      py_raise("TypeError", "shuffle() expects a mutable py_list")
    }
    n <- length(x$.items)
    if (n <= 1L) return(invisible(NULL))
    perm <- .Call(ffi_mt_shuffle_perm, self$.ptr, n)
    x$.items <- x$.items[perm]
    invisible(NULL)
  }

  # Exact port of CPython's `Random.sample(population, k)` (pool copy for small
  # populations, hash set tracking for larger ones).
  self$sample <- function(population, k) {
    pop <- coerce_items(population)
    n <- length(pop)
    k <- as.integer(k)
    if (k < 0L || k > n) {
      py_raise("ValueError", "Sample larger than population or is negative")
    }
    result <- vector("list", k)
    setsize <- 21L
    if (k > 5L) {
      setsize <- setsize + as.integer(4 ^ ceiling(log(k * 3, 4)))
    }
    if (n <= setsize) {
      pool <- pop
      for (i in seq_len(k)) {
        j <- as.integer(self$`_randbelow`(n - i + 1L))
        result[i] <- list(pool[[j + 1L]])
        pool[j + 1L] <- list(pool[[n - i + 1L]])
      }
    } else {
      selected <- new.env(hash = TRUE, parent = emptyenv())
      for (i in seq_len(k)) {
        j <- as.integer(self$`_randbelow`(n))
        key <- as.character(j)
        while (exists(key, envir = selected, inherits = FALSE)) {
          j <- as.integer(self$`_randbelow`(n))
          key <- as.character(j)
        }
        assign(key, TRUE, envir = selected)
        result[i] <- list(pop[[j + 1L]])
      }
    }
    py_list(result)
  }

  # Exact port of CPython's `Random.choices(population, weights=None, *, cum_weights=None, k=1)`.
  self$choices <- function(population, weights = NULL, cum_weights = NULL, k = 1L) {
    pop <- coerce_items(population)
    n <- length(pop)
    k <- as.integer(k)
    if (n == 0L) py_raise("IndexError", "Cannot choose from an empty sequence")
    if (is.null(weights) && is.null(cum_weights)) {
      out <- vector("list", k)
      for (i in seq_len(k)) {
        idx <- floor(self$random() * n)
        out[i] <- list(pop[[idx + 1L]])
      }
      return(py_list(out))
    }
    if (!is.null(weights) && !is.null(cum_weights)) {
      py_raise("TypeError", "Cannot specify both weights and cumulative weights")
    }
    cw <- if (!is.null(cum_weights)) {
      as.numeric(coerce_items(cum_weights))
    } else {
      cumsum(as.numeric(coerce_items(weights)))
    }
    if (length(cw) != n) py_raise("ValueError", "The number of weights does not match the population")
    total <- cw[[n]]
    if (total <= 0) py_raise("ValueError", "Total of weights must be greater than zero")
    out <- vector("list", k)
    for (i in seq_len(k)) {
      x <- self$random() * total
      # `bisect.bisect(cw, x, 0, n - 1)`
      lo <- 0L
      hi <- n - 1L
      while (lo < hi) {
        mid <- (lo + hi) %/% 2L
        if (x < cw[[mid + 1L]]) hi <- mid else lo <- mid + 1L
      }
      out[i] <- list(pop[[lo + 1L]])
    }
    py_list(out)
  }

  self$uniform <- function(a, b) {
    a + (b - a) * self$random()
  }

  self$triangular <- function(low = 0.0, high = 1.0, mode = NULL) {
    u <- self$random()
    c_val <- if (is.null(mode)) 0.5 else (mode - low) / (high - low)
    if (u > c_val) {
      u <- 1.0 - u
      c_val <- 1.0 - c_val
      tmp <- low
      low <- high
      high <- tmp
    }
    low + (high - low) * sqrt(u * c_val)
  }

  self$gauss <- function(mu = 0.0, sigma = 1.0) {
    .Call(ffi_mt_gauss, self$.ptr, as.double(mu), as.double(sigma))
  }

  # CPython's `Random.normalvariate` (Ratio of Uniforms, Kinderman & Monahan).
  self$normalvariate <- function(mu = 0.0, sigma = 1.0) {
    nv_magicconst <- 4.0 * exp(-0.5) / sqrt(2.0)
    repeat {
      u1 <- self$random()
      u2 <- 1.0 - self$random()
      z <- nv_magicconst * (u1 - 0.5) / u2
      zz <- z * z / 4.0
      if (zz <= -log(u2)) break
    }
    mu + z * sigma
  }

  self$expovariate <- function(lambd = 1.0) {
    if (lambd == 0.0) py_raise("ZeroDivisionError", "float division by zero")
    -log(1.0 - self$random()) / lambd
  }

  self$lognormvariate <- function(mu, sigma) {
    exp(self$normalvariate(mu, sigma))
  }

  self$getstate <- function() {
    raw_st <- .Call(ffi_mt_getstate, self$.ptr)
    gauss_next <- if (isTRUE(raw_st[[3L]])) raw_st[[4L]] else NULL
    internal <- py_tuple(c(as.list(raw_st[[1L]]), list(raw_st[[2L]])))
    py_tuple(list(3L, internal, gauss_next))
  }

  self$setstate <- function(state) {
    items <- coerce_items(state)
    if (length(items) != 3L || !identical(items[[1L]], 3L)) {
      py_raise("ValueError", "state with version 3 expected")
    }
    internal <- coerce_items(items[[2L]])
    if (length(internal) != 625L) {
      py_raise("ValueError", "invalid state vector length")
    }
    state_vec <- as.double(unlist(internal[1:624]))
    index <- as.integer(internal[[625L]])
    gauss_next <- items[[3L]]
    has_gauss <- !is.null(gauss_next)
    .Call(
      ffi_mt_setstate, self$.ptr, state_vec, index,
      has_gauss, if (has_gauss) as.double(gauss_next) else 0.0
    )
    invisible(NULL)
  }

  self$seed(a)
  class(self) <- c("pyrt_Random", "pyrt_container")
  self
}

#' @rdname py_random_generator
#' @export
py_random <- NULL

.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  assign("py_random", py_random_generator(0L), envir = ns)
  # Also expose the `Random` constructor on `py_random$Random` so
  # `random$Random(seed)` works directly in generated code.
  py_random$Random <- py_random_generator
}
