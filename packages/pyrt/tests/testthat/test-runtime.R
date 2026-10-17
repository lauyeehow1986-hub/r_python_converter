test_that("py_list, py_tuple, py_dict, and py_set have reference semantics and Python methods", {
  xs <- py_list(list(10L, 20L, 30L))
  alias <- xs
  xs$append(40L)
  expect_identical(py_len(alias), 4L)
  expect_identical(py_getitem(alias, -1L), 40L)

  xs$insert(1L, 15L)
  expect_identical(xs$as_r_list(), list(10L, 15L, 20L, 30L, 40L))
  expect_identical(xs$pop(1L), 15L)
  xs$extend(list(5L, 25L))
  xs$sort()
  expect_identical(xs$as_r_list(), list(5L, 10L, 20L, 25L, 30L, 40L))
  xs$sort(reverse = TRUE)
  expect_identical(xs$as_r_list(), list(40L, 30L, 25L, 20L, 10L, 5L))

  # Slice read, assignment, extended slice assignment, and deletion
  expect_identical(py_getitem(xs, py_slice(1L, 5L, 2L))$as_r_list(), list(30L, 20L))
  py_setitem(xs, py_slice(1L, 3L), list(99L))
  expect_identical(xs$as_r_list(), list(40L, 99L, 20L, 10L, 5L))
  py_delitem(xs, py_slice(NULL, NULL, 2L))
  expect_identical(xs$as_r_list(), list(99L, 10L))

  # Sequence unpacking (`a, *b, c = ...`)
  u <- py_unpack(list(1L, 2L, 3L, 4L, 5L), n_before = 1L, n_after = 2L, has_star = TRUE)
  expect_identical(u[[1L]], 1L)
  expect_identical(u[[2L]]$as_r_list(), list(2L, 3L))
  expect_identical(u[[3L]], 4L)
  expect_identical(u[[4L]], 5L)

  # Dict: `1L`, `1.0`, and `TRUE` hash and compare as the same key
  d <- py_dict()
  d$set(1L, "one")
  d$set(TRUE, "true_overwrites_one")
  d$set(1.0, "float_overwrites")
  expect_identical(d$len(), 1L)
  expect_identical(d$getitem(1L), "float_overwrites")
  expect_identical(d$keys()$as_r_list(), list(1L))
  expect_true(py_in(1.0, d))
  expect_identical(d$setdefault("k", 99L), 99L)
  expect_identical(d$pop("k"), 99L)
  expect_identical(d$pop("missing", "fallback"), "fallback")

  # Set operations and operators (`|`, `&`, `-`, `^`, `<=`)
  s1 <- py_set(list(1L, 2L, 3L, 2L))
  s2 <- py_set(list(3.0, 4L, 5L))
  expect_identical(s1$len(), 3L)
  expect_true(py_eq(py_bitor(s1, s2), py_set(list(1L, 2L, 3L, 4L, 5L))))
  expect_true(py_eq(py_bitand(s1, s2), py_set(list(3L))))
  expect_true(py_eq(py_sub(s1, s2), py_set(list(1L, 2L))))
  expect_true(py_eq(py_bitxor(s1, s2), py_set(list(1L, 2L, 4L, 5L))))
  expect_true(py_le(py_set(list(1L, 2L)), s1))
})

test_that("Python numerics, floor division, modulo, bigints, and two's-complement bitwise ops match CPython", {
  expect_identical(py_floordiv(-7L, 2L), -4L)
  expect_identical(py_mod(-7L, 2L), 1L)
  expect_identical(py_floordiv(7L, -2L), -4L)
  expect_identical(py_mod(7L, -2L), -1L)
  expect_identical(py_divmod(-7L, 2L)$items, list(-4L, 1L))

  # Two's-complement bitwise operations on negative numbers
  expect_identical(py_bitand(-5L, 3L), 3L)
  expect_identical(py_bitand(-5L, -3L), -7L)
  expect_identical(py_bitor(-5L, 3L), -5L)
  expect_identical(py_bitxor(-5L, 3L), -8L)
  expect_identical(py_invert(0L), -1L)
  expect_identical(py_invert(-5L), 4L)
  expect_identical(py_rshift(-8L, 2L), -2L)
  expect_identical(py_rshift(-7L, 2L), -2L)

  # Automatic promotion to gmp::bigz on 32-bit overflow
  big <- py_lshift(1L, 100L)
  expect_s3_class(big, "bigz")
  expect_identical(as.character(big), "1267650600228229401496703205376")
  expect_identical(py_rshift(big, 100L), 1L)
  expect_identical(py_pow(2L, 10L, 1000L), 24L)
  expect_identical(py_int("0xdead_beef", 0L), gmp::as.bigz("3735928559"))

  expect_error(py_truediv(1L, 0L), class = "ZeroDivisionError")
  expect_error(py_floordiv(1L, 0L), class = "ZeroDivisionError")
})

test_that("Python classes, C3 MRO, super(), descriptors, dunders, and with context managers work", {
  # Diamond inheritance: D(B, C) where B(A) and C(A) -> MRO is [D, B, C, A, object]
  A <- py_class("A", dict = list(
    who = function(self) "A"
  ))
  B <- py_class("B", bases = list(A), dict = list(
    who = function(self) paste0("B->", py_super(B, self)$who())
  ))
  C <- py_class("C", bases = list(A), dict = list(
    who = function(self) paste0("C->", py_super(C, self)$who())
  ))
  D <- py_class("D", bases = list(B, C), dict = list(
    who = function(self) paste0("D->", py_super(D, self)$who())
  ))

  expect_identical(D$mro_names, c("D", "B", "C", "A", "object"))
  d <- D()
  expect_identical(d$who(), "D->B->C->A")
  expect_true(py_isinstance(d, A))
  expect_true(py_isinstance(d, py_tuple(list(B, C))))
  expect_true(py_issubclass(D, A))

  # Property, staticmethod, classmethod, and operator dunders
  Vec <- py_class("Vec", dict = list(
    `__init__` = function(self, x) {
      self$x <- x
    },
    `__add__` = function(self, other) Vec(py_add(self$x, other$x)),
    `__repr__` = function(self) sprintf("Vec(%s)", py_repr(self$x)),
    doubled = py_property(
      fget = function(self) py_mul(self$x, 2L),
      fset = function(self, v) { self$x <- py_floordiv(v, 2L) }
    ),
    unit = py_classmethod(function(cls) wrap_callable_class(cls)(1L)),
    util = py_staticmethod(function(a, b) a + b)
  ))

  v <- Vec$unit()
  expect_identical(v$x, 1L)
  expect_identical(v$doubled, 2L)
  v$doubled <- 20L
  expect_identical(v$x, 10L)
  expect_identical(py_repr(py_add(v, Vec(5L))), "Vec(15)")
  expect_identical(Vec$util(3L, 4L), 7L)

  # Context manager (`with`) with normal exit and exception suppression
  events <- character()
  Mgr <- py_class("Mgr", dict = list(
    `__init__` = function(self, suppress = FALSE) { self$suppress <- suppress },
    `__enter__` = function(self) {
      events <<- c(events, "enter")
      "resource"
    },
    `__exit__` = function(self, exc_type, exc_val, tb) {
      events <<- c(events, if (is.null(exc_type)) "exit:ok" else paste0("exit:", exc_type))
      self$suppress
    }
  ))

  res <- py_with(Mgr(FALSE), function(r) paste0("used:", r))
  expect_identical(res, "used:resource")
  expect_identical(events, c("enter", "exit:ok"))

  py_with(Mgr(TRUE), function(r) py_raise("ValueError", "suppressed"))
  expect_identical(events, c("enter", "exit:ok", "enter", "exit:ValueError"))
})

test_that("exceptions, iteration, generators, strings, and format specs match Python", {
  # Exception hierarchy (`KeyError` caught by `LookupError`; `else` and `finally`)
  log_steps <- character()
  out <- py_try(
    {
      log_steps <- c(log_steps, "try")
      py_raise("KeyError", "missing_k")
    },
    handlers = list(
      ValueError = function(e) "val",
      LookupError = function(e) {
        log_steps <<- c(log_steps, paste0("caught:", e$type))
        "lookup"
      }
    ),
    orelse = function() { log_steps <<- c(log_steps, "else") },
    finalbody = function() { log_steps <<- c(log_steps, "finally") }
  )
  expect_identical(out, "lookup")
  expect_identical(log_steps, c("try", "caught:KeyError", "finally"))

  # Stateful generator + `py_for` with `else` vs `break`
  i <- 0L
  gen <- py_generator(function() {
    i <<- i + 1L
    if (i <= 3L) py_yield(i * 10L) else py_stop_iteration()
  })
  expect_identical(py_next(gen), 10L)
  expect_identical(py_collect(gen), list(20L, 30L))
  expect_identical(py_next(gen, "done"), "done")

  ran_else <- FALSE
  py_for(py_range(3L), function(v) NULL, orelse = function() { ran_else <<- TRUE })
  expect_true(ran_else)
  ran_else2 <- FALSE
  py_for(py_range(3L), function(v) py_break_signal(), orelse = function() { ran_else2 <<- TRUE })
  expect_false(ran_else2)

  # Format Specification Mini-Language and str methods
  expect_identical(py_format(3.14159, ">+10.2f"), "     +3.14")
  expect_identical(py_format(255L, "#06x"), "0x00ff")
  expect_identical(py_format(1234567L, ",d"), "1,234,567")
  expect_identical(py_format("hi", "^6s"), "  hi  ")
  expect_identical(py_str_method("  a  b  c  ", "split")(NULL, 1L)$as_r_list(), list("a", "b  c"))
  expect_identical(py_str_method("a--b--c", "rsplit")("--", 1L)$as_r_list(), list("a--b", "c"))
  expect_identical(py_str_method("-42", "zfill")(5L), "-0042")
  expect_identical(py_str_method("hello={0!r} {k:04d}", "format")("x", k = 7L), "hello='x' 0007")
})

test_that("py_random_generator is bit-exact with CPython 3.12 across seed types and methods", {
  # Golden outputs generated from CPython 3.12 `random.Random`:
  rng_int <- py_random_generator(42L)
  expect_identical(
    vapply(1:5, function(i) sprintf("%.17g", rng_int$random()), character(1)),
    c("0.63942679845788375", "0.025010755222666936", "0.27502931836911926",
      "0.22321073814882275", "0.7364712141640124")
  )
  expect_identical(
    c(
      as.character(rng_int$getrandbits(1L)),
      as.character(rng_int$getrandbits(31L)),
      as.character(rng_int$getrandbits(32L)),
      as.character(rng_int$getrandbits(64L)),
      as.character(rng_int$getrandbits(150L))
    ),
    c(
      "1",
      "1590571865",
      "3831882064",
      "1603738325357503540",
      "133726216386261155218499106674059874818229353"
    )
  )
  expect_identical(
    c(
      rng_int$randrange(10L),
      rng_int$randrange(5L, 50L, 3L),
      rng_int$randrange(100L, 10L, -7L),
      rng_int$randint(-20L, 20L)
    ),
    c(3L, 14L, 44L, 18L)
  )
  xs <- py_list(as.list(1:10))
  rng_int$shuffle(xs)
  expect_identical(unlist(xs$as_r_list()), c(3L, 2L, 10L, 8L, 5L, 7L, 6L, 4L, 9L, 1L))
  expect_identical(
    unlist(rng_int$sample(py_list(as.list(1:15)), 6L)$as_r_list()),
    c(10L, 5L, 13L, 1L, 3L, 7L)
  )
  expect_identical(
    unlist(rng_int$choices(py_list(list("a", "b", "c", "d")), weights = list(1, 3, 2, 4), k = 8L)$as_r_list()),
    c("b", "b", "d", "b", "a", "a", "d", "d")
  )

  # String seed (uses SHA-512 version=2 seeding) and huge bigint seed:
  rng_str <- py_random_generator("slice-ar-seed-string")
  expect_identical(
    vapply(1:3, function(i) sprintf("%.17g", rng_str$random()), character(1)),
    c("0.65427923391627796", "0.2016421812291721", "0.14665745840357758")
  )
  rng_big <- py_random_generator(gmp::as.bigz("1234567890123456789012345678901234567890"))
  expect_identical(
    vapply(1:3, function(i) sprintf("%.17g", rng_big$random()), character(1)),
    c("0.81426557975240599", "0.40974435695164046", "0.2204900885895138")
  )

  # getstate / setstate round-trip (including cached Box-Muller `gauss_next`)
  invisible(rng_str$gauss(0.0, 1.0))
  st <- rng_str$getstate()
  g1 <- rng_str$gauss(0.0, 1.0)
  r1 <- rng_str$random()
  rng_str$setstate(st)
  expect_identical(rng_str$gauss(0.0, 1.0), g1)
  expect_identical(rng_str$random(), r1)
})
