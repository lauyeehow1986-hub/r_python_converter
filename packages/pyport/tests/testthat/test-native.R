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

