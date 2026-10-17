test_that("math module provides exact integer and floating-point functions", {
  m <- py_stdlib("math")
  expect_identical(m$gcd(48L, 18L, 12L), 6L)
  expect_identical(m$lcm(4L, 6L, 8L), 24L)
  expect_identical(m$isqrt(15L), 3L)
  expect_identical(m$isqrt(16L), 4L)
  expect_identical(m$comb(10L, 3L), 120L)
  expect_identical(m$perm(5L, 2L), 20L)
  expect_equal(m$hypot(3, 4), 5)
  expect_true(m$isclose(1.0, 1.0 + 1e-10))
  expect_equal(m$prod(py_list(list(2L, 3L, 4L))), 24L)
})

test_that("itertools module supports lazy infinite and combinatoric iterators", {
  it <- py_stdlib("itertools")

  # count + islice
  cnt <- it$count(10L, 3L)
  expect_equal(py_collect(it$islice(cnt, 4L)), list(10L, 13L, 16L, 19L))

  # cycle
  cyc <- it$cycle(py_list(list("a", "b")))
  expect_equal(py_collect(it$islice(cyc, 5L)), list("a", "b", "a", "b", "a"))

  # chain and chain.from_iterable
  expect_equal(
    py_collect(it$chain(py_list(list(1L, 2L)), py_list(list(3L)))),
    list(1L, 2L, 3L)
  )
  expect_equal(
    py_collect(it$chain$from_iterable(py_list(list(py_list(list(1L, 2L)), py_list(list(3L, 4L)))))),
    list(1L, 2L, 3L, 4L)
  )

  # compress, dropwhile, takewhile
  expect_equal(
    py_collect(it$compress(py_list(list("A", "B", "C", "D")), py_list(list(1L, 0L, 1L, 0L)))),
    list("A", "C")
  )
  expect_equal(
    py_collect(it$takewhile(function(x) x < 4L, py_list(list(1L, 3L, 5L, 2L)))),
    list(1L, 3L)
  )
  expect_equal(
    py_collect(it$dropwhile(function(x) x < 4L, py_list(list(1L, 3L, 5L, 2L)))),
    list(5L, 2L)
  )

  # accumulate
  expect_equal(
    py_collect(it$accumulate(py_list(list(1L, 2L, 3L, 4L)))),
    list(1L, 3L, 6L, 10L)
  )

  # product, permutations, combinations, pairwise, batched
  prod_res <- py_collect(it$product(py_list(list(1L, 2L)), py_list(list("x", "y"))))
  expect_length(prod_res, 4L)
  expect_equal(prod_res[[1L]]$as_r_list(), list(1L, "x"))

  comb_res <- py_collect(it$combinations(py_list(list(1L, 2L, 3L)), 2L))
  expect_length(comb_res, 3L)

  perm_res <- py_collect(it$permutations(py_list(list(1L, 2L, 3L)), 2L))
  expect_length(perm_res, 6L)

  pair_res <- py_collect(it$pairwise(py_list(list(10L, 20L, 30L))))
  expect_length(pair_res, 2L)
  expect_equal(pair_res[[2L]]$as_r_list(), list(20L, 30L))

  batch_res <- py_collect(it$batched(py_list(list(1L, 2L, 3L, 4L, 5L)), 2L))
  expect_length(batch_res, 3L)
  expect_equal(batch_res[[3L]]$as_r_list(), list(5L))

  # tee
  branches <- it$tee(py_list(list(7L, 8L, 9L)), 2L)
  b1 <- py_getitem(branches, 0L)
  b2 <- py_getitem(branches, 1L)
  expect_equal(py_collect(b1), list(7L, 8L, 9L))
  expect_equal(py_collect(b2), list(7L, 8L, 9L))
})

test_that("functools and operator modules work as expected", {
  ft <- py_stdlib("functools")
  op <- py_stdlib("operator")

  expect_identical(ft$reduce(op$add, py_list(list(1L, 2L, 3L, 4L))), 10L)
  expect_identical(ft$reduce(op$mul, py_list(list(2L, 3L, 4L)), 10L), 240L)

  add5 <- ft$partial(op$add, 5L)
  expect_identical(add5(12L), 17L)

  calls <- 0L
  cached_sq <- ft$lru_cache(maxsize = 16L)(function(x) {
    calls <<- calls + 1L
    x * x
  })
  expect_identical(cached_sq(6L), 36L)
  expect_identical(cached_sq(6L), 36L)
  expect_identical(calls, 1L)
})

test_that("collections (deque, defaultdict, Counter, namedtuple) work", {
  col <- py_stdlib("collections")

  dq <- col$deque(py_list(list(1L, 2L, 3L)), maxlen = 3L)
  dq$append(4L)
  expect_equal(dq$as_r_list(), list(2L, 3L, 4L))
  dq$appendleft(10L)
  expect_equal(dq$as_r_list(), list(10L, 2L, 3L))
  dq$rotate(1L)
  expect_equal(dq$as_r_list(), list(3L, 10L, 2L))
  expect_identical(dq$popleft(), 3L)

  dd <- col$defaultdict(py_builtin("list"))
  py_getitem(dd, "k")$append(42L)
  expect_equal(py_getitem(dd, "k")$as_r_list(), list(42L))

  cnt <- col$Counter("abracadabra")
  expect_identical(py_getitem(cnt, "a"), 5L)
  expect_identical(py_getitem(cnt, "z"), 0L)
  top2 <- cnt$most_common(2L)$as_r_list()
  expect_equal(top2[[1L]]$as_r_list(), list("a", 5L))

  Point <- col$namedtuple("Point", "x, y")
  p <- Point(3L, y = 4L)
  expect_identical(p$x, 3L)
  expect_identical(p$y, 4L)
  expect_identical(py_getitem(p, 0L), 3L)
})

test_that("re, json, bisect, heapq, and datetime modules work", {
  re <- py_stdlib("re")
  m <- re$search("(\\d+)-(\\w+)", "item 42-foo end")
  expect_identical(m$group(0L), "42-foo")
  expect_identical(m$group(1L), "42")
  expect_identical(m$group(2L), "foo")
  expect_equal(re$findall("\\d+", "a1b22c333")$as_r_list(), list("1", "22", "333"))

  js <- py_stdlib("json")
  txt <- js$dumps(py_builtin_dict(a = 1L, b = py_list(list(TRUE, py_none()))))
  parsed <- js$loads(txt)
  expect_identical(py_getitem(parsed, "a"), 1L)
  expect_identical(py_getitem(py_getitem(parsed, "b"), 0L), TRUE)
  expect_true(is_py_none(py_getitem(py_getitem(parsed, "b"), 1L)))

  bs <- py_stdlib("bisect")
  arr <- py_list(list(10L, 20L, 30L, 40L))
  bs$insort(arr, 25L)
  expect_equal(arr$as_r_list(), list(10L, 20L, 25L, 30L, 40L))

  hq <- py_stdlib("heapq")
  h <- py_list(list(5L, 1L, 9L, 3L))
  hq$heapify(h)
  hq$heappush(h, 2L)
  expect_identical(hq$heappop(h), 1L)
  expect_identical(hq$heappop(h), 2L)

  dt <- py_stdlib("datetime")
  d1 <- py_call(dt$date, list(2024L, 2L, 28L))
  td <- py_call(dt$timedelta, list(days = 2L))
  d2 <- py_add(d1, td)
  expect_identical(py_str(d2), "2024-03-01")
  expect_identical(py_getattr(py_sub(d2, d1), "days"), 2L)
})
