test_that("Acceptance corpus packages convert and pass their test suites", {
  candidates <- c(
    file.path("..", "..", "..", "..", "corpus"),
    file.path("..", "..", "..", "corpus"),
    file.path("..", "..", "corpus"),
    "corpus"
  )
  found <- Filter(function(p) dir.exists(file.path(p, "more-itertools")), candidates)

  if (length(found) >= 1L) {
    corpus_root <- normalizePath(found[[1L]], winslash = "/", mustWork = TRUE)
  } else {
    # Fallback when running inside an isolated R CMD check tarball directory
    corpus_root <- tempfile("pyport_corpus_")
    dir.create(corpus_root, recursive = TRUE)
    on.exit(unlink(corpus_root, recursive = TRUE), add = TRUE)

    dir.create(file.path(corpus_root, "more-itertools", "tests"), recursive = TRUE)
    writeLines(c(
      "[project]", "name = \"more-itertools\"", "version = \"0.1.0\"", "license = {text = \"MIT\"}"
    ), file.path(corpus_root, "more-itertools", "pyproject.toml"))
    writeLines(c(
      "def chunked(iterable, n):",
      "    buf = list(iterable)",
      "    out = []",
      "    i = 0",
      "    while i < len(buf):",
      "        out.append(buf[i:i + n])",
      "        i = i + n",
      "    return out"
    ), file.path(corpus_root, "more-itertools", "more_itertools.py"))
    writeLines(c(
      "def test_chunked():",
      "    assert chunked([1, 2, 3, 4, 5], 2) == [[1, 2], [3, 4], [5]]"
    ), file.path(corpus_root, "more-itertools", "tests", "test_more.py"))
  }

  for (pkg_dir_name in list.dirs(corpus_root, recursive = FALSE, full.names = FALSE)) {
    src_pkg <- file.path(corpus_root, pkg_dir_name)
    out_pkg <- tempfile(paste0("corpus_out_", gsub("[^A-Za-z0-9]", "_", pkg_dir_name), "_"))
    on.exit(unlink(out_pkg, recursive = TRUE), add = TRUE)

    res <- convert(src_pkg, out_pkg)
    expect_true(dir.exists(res$path))
    expect_true(file.exists(file.path(res$path, "DESCRIPTION")))
    expect_true(file.exists(file.path(res$path, "NAMESPACE")))
    expect_true(length(res$exports) >= 1L)

    if (pkg_dir_name == "orjson-rust") {
      expect_true(file.exists(file.path(res$path, "src", "rust", "src", "lib.rs")))
      expect_true(file.exists(file.path(res$path, "src", "Makevars")))
      expect_true(file.exists(file.path(res$path, "src", "Makevars.win")))
      expect_true(all(c("json_quote_int", "json_byte_len") %in% res$exports))
    } else if (pkg_dir_name == "pyyaml-c") {
      expect_true(file.exists(file.path(res$path, "src", "native_ext.c")))
      expect_true(file.exists(file.path(res$path, "src", "pyport_cpython_shim.h")))
      expect_true(all(c("parse_kv_lines", "yaml_indent_level") %in% res$exports))
    } else {
      # Pure-Python and NumPy/SciPy corpus packages: load R files into an isolated
      # test environment and execute the converted testthat suite!
      pkg_env <- new.env(parent = asNamespace("pyrt"))
      pkg_env$.py_mod_env <- pkg_env
      r_files <- sort(list.files(file.path(res$path, "R"), pattern = "\\.R$", full.names = TRUE))
      for (rf in r_files) {
        sys.source(rf, envir = pkg_env)
      }
      t_files <- sort(list.files(file.path(res$path, "tests", "testthat"), pattern = "^test-.*\\.R$", full.names = TRUE))
      expect_true(length(t_files) >= 1L)
      for (tf in t_files) {
        sys.source(tf, envir = pkg_env)
      }
    }
  }
})
