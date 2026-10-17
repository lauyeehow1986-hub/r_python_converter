test_that("check_python_license accepts permissive and copyleft licenses and refuses proprietary ones", {
  mit <- check_python_license("MIT")
  expect_identical(mit$category, "permissive")
  expect_identical(mit$cran_license, "MIT + file LICENSE")
  expect_true(mit$needs_license_file)

  bsd <- check_python_license("BSD-3-Clause")
  expect_identical(bsd$category, "permissive")
  expect_identical(bsd$cran_license, "BSD_3_clause + file LICENSE")

  apache <- check_python_license("Apache-2.0")
  expect_identical(apache$category, "permissive")
  expect_identical(apache$cran_license, "Apache License (>= 2.0)")
  expect_false(apache$needs_license_file)

  gpl <- check_python_license("GPL-3.0-or-later")
  expect_identical(gpl$category, "copyleft")
  expect_identical(gpl$cran_license, "GPL (>= 3)")
  expect_false(gpl$needs_license_file)

  lgpl <- check_python_license("LGPL-2.1")
  expect_identical(lgpl$category, "copyleft")
  expect_identical(lgpl$cran_license, "LGPL (>= 2.1)")

  expect_error(
    check_python_license("Proprietary - All Rights Reserved"),
    class = "pyport_license_error"
  )
  expect_error(
    check_python_license("CC-BY-NC-4.0"),
    class = "pyport_license_error"
  )
})

test_that("convert_pytest_code lowers pytest and unittest suites into runnable testthat tests", {
  py_suite <- "
import pytest
import unittest

def helper_add(x, y):
    return x + y

def test_basic_math():
    assert helper_add(2, 3) == 5
    assert helper_add(-1, 1) == 0
    with pytest.raises(ZeroDivisionError):
        _ = 10 // 0

class TestSuite(unittest.TestCase):
    def test_assertions(self):
        self.assertEqual(helper_add(10, 20), 30)
        self.assertTrue(5 > 2)
        self.assertFalse(2 > 5)
        self.assertIsNone(None)
"
  r_test_code <- convert_pytest_code(py_suite)
  expect_match(r_test_code, "test_that\\(\"test_basic_math\"")
  expect_match(r_test_code, "test_that\\(\"TestSuite::test_assertions\"")
  expect_match(r_test_code, "testthat::expect_error")

  # Evaluate the generated testthat code in a fresh environment!
  env <- new.env(parent = globalenv())
  eval(parse(text = r_test_code), envir = env)
})

test_that("pyport::convert builds a standalone multi-module R package with translated tests", {
  py_dir <- tempfile("pypkg_iterbox_")
  dir.create(file.path(py_dir, "iterbox"), recursive = TRUE)
  dir.create(file.path(py_dir, "tests"), recursive = TRUE)
  on.exit(unlink(py_dir, recursive = TRUE), add = TRUE)

  writeLines(c(
    "[project]",
    "name = \"iter-box\"",
    "version = \"0.2.1\"",
    "description = \"Iteration and windowing utilities\"",
    "license = \"MIT\""
  ), file.path(py_dir, "pyproject.toml"))

  writeLines(c(
    "def chunked(iterable, n):",
    "    if n <= 0:",
    "        raise ValueError('n must be positive')",
    "    buf = []",
    "    for item in iterable:",
    "        buf.append(item)",
    "        if len(buf) == n:",
    "            yield buf",
    "            buf = []",
    "    if len(buf) > 0:",
    "        yield buf",
    "",
    "class Accumulator:",
    "    def __init__(self, start=0):",
    "        self.total = start",
    "    def add(self, x):",
    "        self.total += x",
    "        return self.total"
  ), file.path(py_dir, "iterbox", "core.py"))

  writeLines(c(
    "from .core import chunked, Accumulator",
    "",
    "__all__ = ['chunked', 'Accumulator', 'flatten_once']",
    "",
    "def flatten_once(nested):",
    "    return [item for sub in nested for item in sub]"
  ), file.path(py_dir, "iterbox", "__init__.py"))

  writeLines(c(
    "import pytest",
    "from iterbox import chunked, Accumulator, flatten_once",
    "",
    "def test_chunked_and_flatten():",
    "    chunks = list(chunked([1, 2, 3, 4, 5], 2))",
    "    assert len(chunks) == 3",
    "    flat = flatten_once(chunks)",
    "    assert flat == [1, 2, 3, 4, 5]",
    "    with pytest.raises(ValueError):",
    "        list(chunked([1, 2], 0))",
    "",
    "def test_accumulator():",
    "    acc = Accumulator(10)",
    "    assert acc.add(5) == 15",
    "    assert acc.add(7) == 22"
  ), file.path(py_dir, "tests", "test_iterbox.py"))

  r_pkg_dir <- tempfile("rpkg_iterbox_")
  on.exit(unlink(r_pkg_dir, recursive = TRUE), add = TRUE)

  res <- convert(py_dir, r_pkg_dir)
  expect_identical(res$pkg_name, "iter.box")
  expect_identical(res$version, "0.2.1")
  expect_true(all(c("chunked", "Accumulator", "flatten_once") %in% res$exports))
  expect_true(file.exists(file.path(r_pkg_dir, "DESCRIPTION")))
  expect_true(file.exists(file.path(r_pkg_dir, "NAMESPACE")))
  expect_true(file.exists(file.path(r_pkg_dir, "LICENSE")))
  expect_true(file.exists(file.path(r_pkg_dir, "tests", "testthat", "test-iterbox.R")))

  # Evaluate the generated package's R files and run its generated testthat suite!
  pkg_env <- new.env(parent = globalenv())
  r_sources <- sort(list.files(file.path(r_pkg_dir, "R"), full.names = TRUE))
  for (rf in r_sources) {
    eval(parse(file = rf, keep.source = FALSE), envir = pkg_env)
  }
  test_file <- file.path(r_pkg_dir, "tests", "testthat", "test-iterbox.R")
  eval(parse(file = test_file, keep.source = FALSE), envir = pkg_env)
})
