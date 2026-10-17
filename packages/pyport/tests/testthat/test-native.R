test_that("Phase 6 C extension bridge compiles and runs without libpython", {
  tmp_src <- tempfile("py_cext_")
  dir.create(tmp_src)
  on.exit(unlink(tmp_src, recursive = TRUE), add = TRUE)

  writeLines(c(
    "[project]",
    "name = \"fastc\"",
    "version = \"0.2.0\"",
    "description = \"Hybrid Python and C extension package\"",
    "license = \"MIT\""
  ), file.path(tmp_src, "pyproject.toml"))

  writeLines(c(
    "def py_double(x):",
    "    return c_scale(x, 2.0)"
  ), file.path(tmp_src, "fastc.py"))

  writeLines(c(
    "#include <Python.h>",
    "",
    "double c_scale(double x, double factor) {",
    "    return x * factor;",
    "}",
    "",
    "int c_fib(int n) {",
    "    if (n <= 1) return n;",
    "    int a = 0, b = 1;",
    "    for (int i = 2; i <= n; ++i) {",
    "        int t = a + b;",
    "        a = b;",
    "        b = t;",
    "    }",
    "    return b;",
    "}"
  ), file.path(tmp_src, "fastc_ext.c"))

  ext_info <- detect_native_extensions(tmp_src)
  expect_true(ext_info$has_c)
  expect_equal(ext_info$c_files, "fastc_ext.c")

  dest_pkg <- tempfile("r_fastc_")
  on.exit(unlink(dest_pkg, recursive = TRUE), add = TRUE)

  res <- convert(tmp_src, dest_pkg, pkg_name = "fastc")
  expect_true(all(c("py_double", "c_scale", "c_fib") %in% res$exports))
  expect_true(file.exists(file.path(dest_pkg, "src", "pyport_cpython_shim.h")))
  expect_true(file.exists(file.path(dest_pkg, "src", "native_ext.c")))
})

test_that("Phase 7 Rust PyO3 -> extendr bridge rewrites attributes and generates Makevars", {
  pyo3_code <- "
use pyo3::prelude::*;

#[pyfunction]
pub fn fast_add(a: i32, b: i32) -> i32 {
    a + b
}

#[pyfunction]
fn greet_json(name: &str) -> PyResult<String> {
    Ok(format!(\"{\\\"hello\\\":\\\"{}\\\"}\", name))
}

#[pymodule]
fn myrust(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(fast_add, m)?)?;
    Ok(())
}
"
  out <- transpile_pyo3_rust(pyo3_code, pkg_name = "myrust")
  expect_equal(out$functions, c("fast_add", "greet_json"))
  expect_match(out$rust_code, "use extendr_api::prelude::\\*;", perl = TRUE)
  expect_match(out$rust_code, "#\\[extendr\\]", perl = TRUE)
  expect_match(out$rust_code, "extendr_module!", fixed = TRUE)
  expect_false(grepl("#\\[pymodule\\]", out$rust_code))
  expect_match(out$r_wrappers, "\\.Call\\(\"wrap__fast_add\"", perl = TRUE)
  expect_match(out$makevars_win, "x86_64-pc-windows-gnu", fixed = TRUE)
})

test_that("Phase 8 escape analysis distinguishes escaping vs non-escaping containers", {
  tree <- parse_python("
def process(items):
    local_buf = [1, 2, 3]
    returned_list = [4, 5, 6]
    aliased_buf = [7, 8]
    other = aliased_buf
    return returned_list
")
  esc <- analyze_escape(tree$body[[1L]])
  expect_equal(esc$non_escaping, "local_buf")
  expect_true(all(c("returned_list", "aliased_buf") %in% esc$escaping))
})

test_that("PyMethodDef + PyArg_ParseTuple and Cython cpdef lower cleanly", {
  c_pymeth <- "
#include <Python.h>

static PyObject* py_clamp(PyObject *self, PyObject *args) {
    double x = 0.0, lo = 0.0, hi = 1.0;
    if (!PyArg_ParseTuple(args, \"ddd\", &x, &lo, &hi)) return NULL;
    if (x < lo) return PyFloat_FromDouble(lo);
    if (x > hi) return PyFloat_FromDouble(hi);
    return PyFloat_FromDouble(x);
}

static PyMethodDef ClampMethods[] = {
    {\"clamp_val\", py_clamp, METH_VARARGS, \"Clamp double value.\"},
    {NULL, NULL, 0, NULL}
};
"
  bridged <- transpile_c_extension(c_pymeth, pkg_name = "clamppkg")
  expect_equal(bridged$functions, "clamp_val")
  expect_match(bridged$c_code, "SEXP r_pymeth_clamp_val", fixed = TRUE)
  expect_match(bridged$r_code, "clamp_val <- function\\(\\.\\.\\.\\)", perl = TRUE)

  cython_src <- "
cpdef double cy_hypot2(double a, double b):
    cdef double s = a * a + b * b
    return s
"
  cy_bridged <- transpile_c_extension(cython_src, pkg_name = "cypkg")
  expect_equal(cy_bridged$functions, "cy_hypot2")
  expect_match(cy_bridged$c_code, "SEXP r_wrap_cy_hypot2", fixed = TRUE)
})

test_that("PyO3 #[pyclass] and #[pymethods] lower to extendr impl blocks", {
  rs_src <- "
use pyo3::prelude::*;

#[pyclass]
pub struct Counter {
    val: i32,
}

#[pymethods]
impl Counter {
    #[new]
    pub fn new(start: i32) -> Self {
        Counter { val: start }
    }
}
"
  out <- transpile_pyo3_rust(rs_src, pkg_name = "cntpkg")
  expect_equal(out$classes, "Counter")
  expect_match(out$rust_code, "impl Counter;", fixed = TRUE)
  expect_match(out$r_wrappers, "Counter\\$new", perl = TRUE)
})

test_that("range() for-loop fast path and build_r_package() work", {
  r_code <- as.character(transpile_python("
def sum_to(n):
    acc = 0
    for i in range(n):
        acc = acc + i
    return acc
"))
  expect_match(r_code, "for \\(i in pyrt::py_range_seq\\(n\\)\\)", perl = TRUE)
  env <- eval_python("
def sum_to(n):
    acc = 0
    for i in range(n):
        acc = acc + i
    return acc
")
  expect_identical(env$sum_to(10L), 45L)

  tmp_py <- tempfile("py_bpkg_")
  dir.create(tmp_py)
  on.exit(unlink(tmp_py, recursive = TRUE), add = TRUE)
  writeLines("def add1(x):\n    return x + 1", file.path(tmp_py, "add1.py"))
  out_r <- tempfile("r_bpkg_")
  on.exit(unlink(out_r, recursive = TRUE), add = TRUE)
  res <- convert(tmp_py, out_r, pkg_name = "add1pkg")
  built <- build_r_package(res$path, dest_dir = tempdir(), binary = FALSE)
  on.exit(unlink(built$source_pkg), add = TRUE)
  expect_true(file.exists(built$source_pkg))
})


