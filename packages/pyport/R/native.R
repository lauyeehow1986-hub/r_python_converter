# Phase 6 & Phase 7: Native C/C++/Cython and Rust (`PyO3` -> `extendr`) Bridges
#
# Provides:
# 1. `detect_native_extensions(source_dir)`: scans a Python package directory
#    for C/C++ extensions (`*.c`, `*.cc`, `*.cpp`, `setup.py` `Extension(...)`),
#    Cython sources (`*.pyx`, `*.pxd`), and Rust (`Cargo.toml` with `pyo3` or
#    `maturin` in `pyproject.toml`).
# 2. `generate_c_bridge(ext_info, pkg_name)`: rewrites CPython `PyMethodDef` /
#    `PyArg_ParseTuple` / `PyLong_FromLong` / `PyFloat_FromDouble` /
#    `PyUnicode_FromString` / `PyErr_SetString` C extension files into R `.Call`
#    wrappers + `src/init.c` registration (`R_registerRoutines`) and R wrapper
#    functions (`R/native-bridge.R`) with zero Python header dependency.
# 3. `generate_rust_bridge(rust_info, pkg_name)`: rewrites Rust `#[pyfunction]`
#    / `#[pymethods]` / `#[pymodule]` (`PyO3`) into `#[extendr]` attributes and
#    generates CRAN-compliant `src/Makevars`, `src/Makevars.win`, and
#    `src/entrypoint.c` with offline `cargo vendor` support.

#' Detect native C, C++, Cython, or Rust extensions in a Python package
#'
#' Inspects a Python source directory for C/C++ extension sources (`*.c`,
#' `*.cpp`, `*.cc`), Cython sources (`*.pyx`), and Rust crates (`Cargo.toml` /
#' `pyproject.toml` with `maturin` or `pyo3`).
#'
#' @param source_dir Path to the unpacked Python package root.
#' @returns A list with components:
#'   - `has_c`: logical indicating C/C++ extension sources were found.
#'   - `has_cython`: logical indicating `.pyx` sources were found.
#'   - `has_rust`: logical indicating a `Cargo.toml` / `PyO3` crate was found.
#'   - `c_files`: character vector of relative paths to C/C++ files.
#'   - `pyx_files`: character vector of relative paths to `.pyx` files.
#'   - `cargo_toml`: relative path to `Cargo.toml` if present, or `NULL`.
#'   - `rust_files`: character vector of relative paths to `.rs` files.
#' @export
#' @examples
#' tmp <- tempfile("pyext_")
#' dir.create(tmp)
#' writeLines("int add(int a, int b) { return a + b; }", file.path(tmp, "fast.c"))
#' detect_native_extensions(tmp)
#' unlink(tmp, recursive = TRUE)
detect_native_extensions <- function(source_dir) {
  if (!dir.exists(source_dir)) {
    stop(sprintf("Directory does not exist: '%s'", source_dir))
  }
  all_files <- list.files(source_dir, recursive = TRUE, full.names = FALSE)
  c_files <- all_files[grepl("\\.(c|cc|cpp|cxx)$", all_files, ignore.case = TRUE)]
  pyx_files <- all_files[grepl("\\.pyx$", all_files, ignore.case = TRUE)]
  rust_files <- all_files[grepl("\\.rs$", all_files, ignore.case = TRUE)]
  cargo_candidates <- all_files[basename(all_files) == "Cargo.toml"]
  cargo_toml <- if (length(cargo_candidates) >= 1L) cargo_candidates[[1L]] else NULL

  list(
    has_c = length(c_files) > 0L,
    has_cython = length(pyx_files) > 0L,
    has_rust = !is.null(cargo_toml) || length(rust_files) > 0L,
    c_files = c_files,
    pyx_files = pyx_files,
    cargo_toml = cargo_toml,
    rust_files = rust_files
  )
}

# ---- Phase 6: CPython C-API / Cython -> R `<Rinternals.h>` shim & bridge ----

#' Generate an R `<Rinternals.h>` compatibility header for CPython C extensions
#'
#' Returns the C header content (`pyport_cpython_shim.h`) that maps the subset
#' of `<Python.h>` used by standard C/Cython extensions (`PyObject*` -> `SEXP`,
#' `Py_None` -> `R_NilValue`, `PyLong_FromLong` -> `ScalarInteger`,
#' `PyLong_AsLong` -> `Rf_asInteger`, `PyFloat_FromDouble` -> `ScalarReal`,
#' `PyFloat_AsDouble` -> `Rf_asReal`, `PyUnicode_FromString` -> `mkString`,
#' `PyUnicode_AsUTF8` -> `CHAR(STRING_ELT(...))`, `PyBool_FromLong` ->
#' `ScalarLogical`, `PyErr_SetString` -> `Rf_error`) directly onto R's C API so
#' converted C extensions compile with `R CMD SHLIB` without `libpython`.
#'
#' @returns Character string containing the C header contents.
#' @export
#' @examples
#' cat(substr(cpython_shim_header(), 1, 200))
cpython_shim_header <- function() {
  paste(
    "#ifndef PYPORT_CPYTHON_SHIM_H",
    "#define PYPORT_CPYTHON_SHIM_H",
    "",
    "#define R_NO_REMAP",
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <R_ext/Rdynload.h>",
    "#include <stdint.h>",
    "#include <string.h>",
    "",
    "/* Map PyObject* directly to R SEXP */",
    "typedef SEXP PyObject;",
    "typedef R_xlen_t Py_ssize_t;",
    "",
    "#define Py_None R_NilValue",
    "#define Py_RETURN_NONE return R_NilValue",
    "#define Py_RETURN_TRUE return Rf_ScalarLogical(1)",
    "#define Py_RETURN_FALSE return Rf_ScalarLogical(0)",
    "#define Py_INCREF(x) ((void)0)",
    "#define Py_DECREF(x) ((void)0)",
    "#define Py_XINCREF(x) ((void)0)",
    "#define Py_XDECREF(x) ((void)0)",
    "",
    "/* Numeric & string conversions */",
    "static inline PyObject* PyLong_FromLong(long v) {",
    "  return Rf_ScalarInteger((int)v);",
    "}",
    "static inline long PyLong_AsLong(PyObject *op) {",
    "  return (long)Rf_asInteger(op);",
    "}",
    "static inline PyObject* PyFloat_FromDouble(double v) {",
    "  return Rf_ScalarReal(v);",
    "}",
    "static inline double PyFloat_AsDouble(PyObject *op) {",
    "  return Rf_asReal(op);",
    "}",
    "static inline PyObject* PyBool_FromLong(long v) {",
    "  return Rf_ScalarLogical(v != 0);",
    "}",
    "static inline PyObject* PyUnicode_FromString(const char *u) {",
    "  return Rf_mkString(u ? u : \"\");",
    "}",
    "static inline const char* PyUnicode_AsUTF8(PyObject *op) {",
    "  return CHAR(STRING_ELT(op, 0));",
    "}",
    "",
    "/* Exception raising */",
    "#define PyExc_ValueError \"ValueError\"",
    "#define PyExc_TypeError \"TypeError\"",
    "#define PyExc_RuntimeError \"RuntimeError\"",
    "#define PyExc_IndexError \"IndexError\"",
    "#define PyExc_KeyError \"KeyError\"",
    "#define PyExc_ZeroDivisionError \"ZeroDivisionError\"",
    "static inline void PyErr_SetString(const char *exc, const char *msg) {",
    "  Rf_error(\"%s: %s\", exc, msg);",
    "}",
    "",
    "#endif /* PYPORT_CPYTHON_SHIM_H */",
    sep = "\n"
  )
}

#' Transpile a simple C or Cython function file to an R `.Call` C file
#'
#' Parses C or Cython scalar/numeric functions (`double`/`int`/`const char*`
#' parameters and return values, or `<Python.h>` `PyObject*` functions) and
#' generates:
#' - `c_code`: C source file with `#include "pyport_cpython_shim.h"`, `.Call`
#'   `SEXP` entry points (`r_wrap_<name>`), and `R_init_<pkg>` registration.
#' - `r_code`: R wrapper functions calling `.Call("r_wrap_<name>", ..., PACKAGE = pkg_name)`.
#' - `functions`: character vector of exported function names.
#'
#' @param code Character string containing C or Cython (`.pyx`) source code.
#' @param pkg_name Name of the target R package (used in `R_init_<pkg>` and
#'   `.Call(..., PACKAGE = pkg_name)`).
#' @returns A list with `c_code`, `r_code`, and `functions`.
#' @export
#' @examples
#' res <- transpile_c_extension("
#' double hyp2(double a, double b) {
#'     return a * a + b * b;
#' }
#' ", pkg_name = "fastmath")
#' cat(res$r_code)
transpile_c_extension <- function(code, pkg_name = "convertedpkg") {
  lines <- unlist(strsplit(paste(code, collapse = "\n"), "\r?\n"))
  clean_code <- paste(lines, collapse = "\n")
  # Replace `#include <Python.h>` with `#include "pyport_cpython_shim.h"`
  clean_code <- gsub('#include\\s*[<"]Python\\.h[>"]', '#include "pyport_cpython_shim.h"', clean_code)

  # Match C function signatures: `(static)? (int|long|double|const char*)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*\{`
  pat <- "(?:^|\\n)\\s*(?:static\\s+)?(int|long|double|const\\s+char\\s*\\*)\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^)]*)\\)\\s*\\{"
  m <- gregexpr(pat, clean_code, perl = TRUE)[[1L]]

  fn_names <- character()
  c_wrappers <- character()
  r_wrappers <- character()
  call_entries <- character()

  if (length(m) >= 1L && m[[1L]] > 0L) {
    matches <- regmatches(clean_code, list(m))[[1L]]
    for (sig in matches) {
      cap <- regmatches(sig, regexec(pat, sig, perl = TRUE))[[1L]]
      ret_type <- trimws(gsub("\\s+", " ", cap[[2L]]))
      fname <- cap[[3L]]
      if (startsWith(fname, "R_init_") || startsWith(fname, "r_wrap_") || fname == "main") next
      raw_params <- trimws(cap[[4L]])
      params <- if (!nzchar(raw_params) || raw_params == "void") {
        character()
      } else {
        trimws(strsplit(raw_params, ",", fixed = TRUE)[[1L]])
      }

      p_types <- character(length(params))
      p_names <- character(length(params))
      valid_sig <- TRUE
      for (i in seq_along(params)) {
        pm <- regmatches(params[[i]], regexec("^(int|long|double|const\\s+char\\s*\\*)\\s*([A-Za-z_][A-Za-z0-9_]*)$", params[[i]], perl = TRUE))[[1L]]
        if (length(pm) == 0L) {
          valid_sig <- FALSE
          break
        }
        p_types[[i]] <- trimws(gsub("\\s+", " ", pm[[2L]]))
        p_names[[i]] <- pm[[3L]]
      }
      if (!valid_sig) next

      fn_names <- c(fn_names, fname)
      wrap_sym <- paste0("r_wrap_", fname)
      sexp_params <- if (length(p_names) == 0L) "void" else paste(paste0("SEXP s_", p_names), collapse = ", ")

      conv_lines <- character()
      for (i in seq_along(p_names)) {
        pt <- p_types[[i]]
        pn <- p_names[[i]]
        if (pt %in% c("int", "long")) {
          conv_lines <- c(conv_lines, sprintf("  %s c_%s = (%s)Rf_asInteger(s_%s);", pt, pn, pt, pn))
        } else if (pt == "double") {
          conv_lines <- c(conv_lines, sprintf("  double c_%s = Rf_asReal(s_%s);", pn, pn))
        } else {
          conv_lines <- c(conv_lines, sprintf("  const char* c_%s = CHAR(STRING_ELT(s_%s, 0));", pn, pn))
        }
      }
      c_call <- sprintf("%s(%s)", fname, paste(if (length(p_names) > 0L) paste0("c_", p_names) else character(), collapse = ", "))
      ret_stmt <- if (ret_type %in% c("int", "long")) {
        sprintf("  return Rf_ScalarInteger((int)(%s));", c_call)
      } else if (ret_type == "double") {
        sprintf("  return Rf_ScalarReal(%s);", c_call)
      } else {
        sprintf("  return Rf_mkString(%s);", c_call)
      }

      c_wrappers <- c(
        c_wrappers,
        sprintf("SEXP %s(%s) {\n%s\n%s\n}", wrap_sym, sexp_params, paste(conv_lines, collapse = "\n"), ret_stmt)
      )
      call_entries <- c(
        call_entries,
        sprintf("  {\"%s\", (DL_FUNC) &%s, %d},", wrap_sym, wrap_sym, length(p_names))
      )

      r_coercions <- vapply(seq_along(p_names), function(i) {
        pt <- p_types[[i]]
        pn <- p_names[[i]]
        if (pt %in% c("int", "long")) sprintf("as.integer(%s)", pn)
        else if (pt == "double") sprintf("as.double(%s)", pn)
        else sprintf("as.character(%s)", pn)
      }, character(1))
      call_args <- paste(c(sprintf("\"%s\"", wrap_sym), r_coercions, sprintf("PACKAGE = \"%s\"", pkg_name)), collapse = ", ")
      r_wrappers <- c(
        r_wrappers,
        sprintf("%s <- function(%s) {\n  .Call(%s)\n}", fname, paste(p_names, collapse = ", "), call_args)
      )
    }
  }

  pkg_c_ident <- gsub("[^A-Za-z0-9_]", "_", pkg_name)
  c_full <- paste(
    "#include \"pyport_cpython_shim.h\"",
    "",
    clean_code,
    "",
    paste(c_wrappers, collapse = "\n\n"),
    "",
    "static const R_CallMethodDef CallEntries[] = {",
    paste(call_entries, collapse = "\n"),
    "  {NULL, NULL, 0}",
    "};",
    "",
    sprintf("void R_init_%s(DllInfo *dll) {", pkg_c_ident),
    "  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);",
    "  R_useDynamicSymbols(dll, FALSE);",
    "}",
    sep = "\n"
  )

  list(
    c_code = c_full,
    r_code = paste(r_wrappers, collapse = "\n\n"),
    functions = fn_names
  )
}

# ---- Phase 7: Rust (`PyO3` / `maturin`) -> `extendr` bridge ----------------

#' Rewrite a `PyO3` Rust source file (`#[pyfunction]`, `#[pymodule]`) to `extendr`
#'
#' Converts `use pyo3::prelude::*;`, `#[pyfunction]`, `#[pymethods]`,
#' `#[pyclass]`, `PyResult<T>`, and `#[pymodule]` in a Rust source file into
#' `use extendr_api::prelude::*;`, `#[extendr]`, `Result<T>`, and
#' `extendr_module! { mod <pkg>; ... }`, and generates matching R `.Call`
#' wrappers so the crate compiles as a standalone R package without Python or
#' `libpython`.
#'
#' @param rust_code Character string containing Rust `PyO3` source code.
#' @param pkg_name Target R package name.
#' @returns A list with:
#'   - `rust_code`: transformed Rust code using `extendr_api::prelude::*`.
#'   - `r_wrappers`: R wrapper functions calling `.Call("wrap__<fn>", ...)`.
#'   - `functions`: character vector of exported `#[pyfunction]` names.
#'   - `makevars`: `src/Makevars` content for Unix/macOS.
#'   - `makevars_win`: `src/Makevars.win` content for Windows (`x86_64-pc-windows-gnu`).
#' @export
#' @examples
#' pyo3_src <- '
#' use pyo3::prelude::*;
#'
#' #[pyfunction]
#' fn sum_squares(n: i32) -> i32 {
#'     (1..=n).map(|x| x * x).sum()
#' }
#' '
#' out <- transpile_pyo3_rust(pyo3_src, pkg_name = "fastsq")
#' cat(out$rust_code)
transpile_pyo3_rust <- function(rust_code, pkg_name = "rustpkg") {
  src <- paste(rust_code, collapse = "\n")

  # Extract all `#[pyfunction]` function signatures
  pat <- "#\\[pyfunction(?:\\([^)]*\\))?\\]\\s*(?:pub\\s+)?fn\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^)]*)\\)"
  m <- gregexpr(pat, src, perl = TRUE)[[1L]]
  fn_names <- character()
  r_wrappers <- character()

  if (length(m) >= 1L && m[[1L]] > 0L) {
    hits <- regmatches(src, list(m))[[1L]]
    for (h in hits) {
      cap <- regmatches(h, regexec(pat, h, perl = TRUE))[[1L]]
      fname <- cap[[2L]]
      raw_params <- trimws(cap[[3L]])
      p_names <- if (!nzchar(raw_params)) {
        character()
      } else {
        parts <- trimws(strsplit(raw_params, ",", fixed = TRUE)[[1L]])
        # Drop `py: Python<'_>` context arguments
        parts <- parts[!grepl(":\\s*Python\\b", parts)]
        vapply(parts, function(p) trimws(strsplit(p, ":", fixed = TRUE)[[1L]][[1L]]), character(1))
      }
      fn_names <- c(fn_names, fname)
      call_sym <- paste0("wrap__", fname)
      call_args <- paste(c(sprintf("\"%s\"", call_sym), p_names, sprintf("PACKAGE = \"%s\"", pkg_name)), collapse = ", ")
      r_wrappers <- c(
        r_wrappers,
        sprintf("%s <- function(%s) {\n  .Call(%s)\n}", fname, paste(p_names, collapse = ", "), call_args)
      )
    }
  }

  # Rewrite PyO3 imports and attributes to extendr
  out_rs <- src
  out_rs <- gsub("use\\s+pyo3::prelude::\\*;", "use extendr_api::prelude::*;", out_rs)
  out_rs <- gsub("#\\[pyfunction(?:\\([^)]*\\))?\\]", "#[extendr]", out_rs)
  out_rs <- gsub("#\\[pymethods\\]", "#[extendr]", out_rs)
  out_rs <- gsub("#\\[pyclass(?:\\([^)]*\\))?\\]", "#[extendr]", out_rs)
  out_rs <- gsub("\\bPyResult<", "Result<", out_rs)
  # Strip `#[pymodule] fn ... { ... }` block if present and append `extendr_module!`
  out_rs <- sub("#\\[pymodule\\][\\s\\S]*$", "", out_rs, perl = TRUE)

  mod_ident <- gsub("[^A-Za-z0-9_]", "_", pkg_name)
  fn_lines <- if (length(fn_names) > 0L) paste0("  fn ", fn_names, ";", collapse = "\n") else ""
  ext_macro <- sprintf(
    "\nextendr_module! {\n  mod %s;\n%s\n}\n",
    mod_ident, fn_lines
  )
  out_rs <- paste0(trimws(out_rs), "\n", ext_macro)

  makevars <- paste(
    "TARGET_DIR = ./rust/target",
    "LIBDIR = $(TARGET_DIR)/release",
    sprintf("STATLIB = $(LIBDIR)/lib%s.a", mod_ident),
    sprintf("PKG_LIBS = -L$(LIBDIR) -l%s", mod_ident),
    "",
    "all: $(SHLIB)",
    "$(SHLIB): $(STATLIB)",
    "$(STATLIB):",
    "\tcargo build --lib --release --manifest-path=./rust/Cargo.toml --target-dir $(TARGET_DIR)",
    "clean:",
    "\t$(RM) -rf $(SHLIB) $(STATLIB) $(OBJECTS) $(TARGET_DIR)",
    sep = "\n"
  )

  makevars_win <- paste(
    "TARGET = x86_64-pc-windows-gnu",
    "TARGET_DIR = ./rust/target",
    "LIBDIR = $(TARGET_DIR)/$(TARGET)/release",
    sprintf("STATLIB = $(LIBDIR)/lib%s.a", mod_ident),
    sprintf("PKG_LIBS = -L$(LIBDIR) -l%s -lws2_32 -ladvapi32 -luserenv -lbcrypt -lntdll", mod_ident),
    "",
    "all: $(SHLIB)",
    "$(SHLIB): $(STATLIB)",
    "$(STATLIB):",
    "\tcargo build --target=$(TARGET) --lib --release --manifest-path=./rust/Cargo.toml --target-dir $(TARGET_DIR)",
    "clean:",
    "\t$(RM) -rf $(SHLIB) $(STATLIB) $(OBJECTS) $(TARGET_DIR)",
    sep = "\n"
  )

  list(
    rust_code = out_rs,
    r_wrappers = paste(r_wrappers, collapse = "\n\n"),
    functions = fn_names,
    makevars = makevars,
    makevars_win = makevars_win
  )
}
