test_that("numpy ndarray creation, shapes, 0-based indexing, and broadcasting work", {
  np <- py_stdlib("numpy")

  a <- np$array(list(list(1L, 2L, 3L), list(4L, 5L, 6L)))
  expect_equal(a$shape$as_r_list(), list(2L, 3L))
  expect_identical(a$ndim, 2L)
  expect_identical(a$size, 6L)

  # 0-based indexing and negative indexing
  expect_identical(py_getitem(a, py_tuple(list(0L, 1L))), 2L)
  expect_identical(py_getitem(a, py_tuple(list(1L, -1L))), 6L)

  # Row slice and column slice
  row0 <- py_getitem(a, 0L)
  expect_equal(row0$tolist()$as_r_list(), list(1L, 2L, 3L))
  col1 <- py_getitem(a, py_tuple(list(py_slice(NULL, NULL, NULL), 1L)))
  expect_equal(col1$tolist()$as_r_list(), list(2L, 5L))

  # Transpose & reshape
  at <- a$T
  expect_equal(at$shape$as_r_list(), list(3L, 2L))
  r <- a$reshape(3L, 2L)
  expect_equal(r$shape$as_r_list(), list(3L, 2L))
  expect_equal(r$flatten()$tolist()$as_r_list(), list(1L, 2L, 3L, 4L, 5L, 6L))

  # Broadcasting (2, 3) + (3,)
  b <- np$array(list(10L, 20L, 30L))
  s <- py_add(a, b)
  expect_equal(py_getitem(s, 0L)$tolist()$as_r_list(), list(11, 22, 33))
  expect_equal(py_getitem(s, 1L)$tolist()$as_r_list(), list(14, 25, 36))

  # Axis reductions
  expect_equal(a$sum(), 21L)
  expect_equal(a$sum(axis = 0L)$tolist()$as_r_list(), list(5L, 7L, 9L))
  expect_equal(a$sum(axis = 1L)$tolist()$as_r_list(), list(6L, 15L))
  expect_equal(a$mean(axis = 0L)$tolist()$as_r_list(), list(2.5, 3.5, 4.5))
  expect_identical(a$argmax(), 5L)
  expect_identical(a$argmin(), 0L)
})

test_that("numpy linalg, polyfit, where, and ufuncs work", {
  np <- py_stdlib("numpy")

  m <- np$array(list(list(4.0, 7.0), list(2.0, 6.0)))
  expect_equal(np$linalg$det(m), 10.0)
  minv <- np$linalg$inv(m)
  prod_id <- py_matmul(m, minv)
  expect_true(np$allclose(prod_id, np$eye(2L)))

  # Solve A x = b
  rhs <- np$array(list(1.0, 0.0))
  sol <- np$linalg$solve(m, rhs)
  expect_true(np$allclose(py_matmul(m, sol), rhs))

  # Boolean mask + np$where
  x <- np$arange(6L)
  masked <- py_getitem(x, py_gt(x, 2L))
  expect_equal(masked$tolist()$as_r_list(), list(3L, 4L, 5L))
  w <- np$where(py_gt(x, 2L), 100L, -1L)
  expect_equal(w$tolist()$as_r_list(), list(-1, -1, -1, 100, 100, 100))
})

test_that("scipy.special, scipy.stats, and scipy.optimize work", {
  sp <- py_stdlib("scipy")

  expect_equal(sp$special$gamma(5.0), 24.0)
  expect_equal(sp$special$erf(0.0), 0.0)
  expect_equal(sp$special$expit(0.0), 0.5)

  expect_equal(sp$stats$norm$cdf(0.0), 0.5)
  expect_equal(sp$stats$norm$ppf(0.5), 0.0)

  x <- c(1, 2, 3, 4, 5)
  y <- c(2, 4, 6, 8, 10)
  lr <- sp$stats$linregress(x, y)
  expect_equal(lr$slope, 2.0)
  expect_equal(lr$intercept, 0.0)
  expect_equal(lr$rvalue, 1.0)

  opt <- sp$optimize$minimize_scalar(function(v) (v - 3.5)^2, bounds = list(-10, 10))
  expect_equal(opt$x, 3.5, tolerance = 1e-5)

  root <- sp$optimize$brentq(function(v) v^2 - 9.0, 0.0, 5.0)
  expect_equal(root, 3.0, tolerance = 1e-6)
})

test_that("pandas Series and DataFrame work", {
  pd <- py_stdlib("pandas")

  s <- pd$Series(list(10, 20, 30))
  expect_equal(s$mean(), 20)
  expect_equal(s$sum(), 60)
  expect_identical(py_len(s), 3L)

  df <- pd$DataFrame(py_builtin_dict(a = list(1, 2, 3), b = list(4, 5, 6)))
  expect_equal(df$shape$as_r_list(), list(3L, 2L))
  py_setitem(df, "c", py_add(py_getitem(df, "a"), py_getitem(df, "b")))
  expect_equal(py_getitem(df, "c")$tolist()$as_r_list(), list(5, 7, 9))
})

test_that("numpy.random RandomState(seed) produces reproducible MT19937 draws", {
  np <- py_stdlib("numpy")
  rs1 <- np$random$RandomState(42L)
  rs2 <- np$random$RandomState(42L)

  u1 <- rs1$rand(5L)
  u2 <- rs2$rand(5L)
  expect_equal(u1$tolist()$as_r_list(), u2$tolist()$as_r_list())
  # Golden first draw of np.random.RandomState(42).rand(): 0.3745401188473625
  expect_equal(py_getitem(u1, 0L), 0.3745401188473625, tolerance = 1e-15)
  expect_equal(py_getitem(u1, 1L), 0.9507143064099162, tolerance = 1e-15)

  ints1 <- rs1$randint(0L, 100L, size = 6L)
  ints2 <- rs2$randint(0L, 100L, size = 6L)
  expect_equal(ints1$tolist()$as_r_list(), ints2$tolist()$as_r_list())

  n1 <- rs1$randn(4L)
  n2 <- rs2$randn(4L)
  expect_equal(n1$tolist()$as_r_list(), n2$tolist()$as_r_list())
})

