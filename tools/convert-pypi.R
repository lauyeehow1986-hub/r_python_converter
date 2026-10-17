#!/usr/bin/env Rscript
# tools/convert-pypi.R
#
# Batch converter and benchmark runner for PyPI sdists (`.tar.gz`) and local
# corpus packages. Produces standalone R source tarballs (`.tar.gz`) and
# Windows/macOS binary packages (`.zip` / `.tgz`), executes converted test
# suites, and prints a summary table.
#
# Usage:
#   Rscript tools/convert-pypi.R                  # converts & builds all corpus packages
#   Rscript tools/convert-pypi.R --pypi <pkgname> # downloads sdist from PyPI and converts

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
})

download_pypi_sdist <- function(pkg, dest_dir = tempdir()) {
  url <- sprintf("https://pypi.org/pypi/%s/json", pkg)
  con <- url(url)
  on.exit(close(con), add = TRUE)
  raw_json <- paste(readLines(con, warn = FALSE), collapse = "\n")
  # Extract sdist URL via regex (zero external JSON dependency required)
  m <- regmatches(raw_json, regexpr("https://files\\.pythonhosted\\.org/[^\"]+\\.tar\\.gz", raw_json))
  if (length(m) == 0L || !nzchar(m)) {
    stop(sprintf("Could not find source tarball (.tar.gz) on PyPI for '%s'.", pkg))
  }
  out_tar <- file.path(dest_dir, basename(m[[1L]]))
  utils::download.file(m[[1L]], out_tar, mode = "wb", quiet = TRUE)
  out_tar
}

run_corpus_benchmark <- function(corpus_dir = "corpus", out_root = file.path(tempdir(), "pyport_bench")) {
  unlink(out_root, recursive = TRUE)
  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  pkgs <- list.dirs(corpus_dir, recursive = FALSE, full.names = FALSE)
  rows <- list()

  for (nm in pkgs) {
    t0 <- proc.time()[["elapsed"]]
    src_path <- file.path(corpus_dir, nm)
    dest_pkg <- file.path(out_root, nm)
    res <- convert(src_path, dest_pkg)

    can_build_bin <- (nm != "orjson-rust")
    built <- build_r_package(res$path, dest_dir = out_root, binary = can_build_bin)
    elapsed <- round(proc.time()[["elapsed"]] - t0, 2)

    rows[[length(rows) + 1L]] <- data.frame(
      corpus_pkg = nm,
      r_pkg = res$pkg_name,
      exports = length(res$exports),
      source_tar = basename(built$source_pkg),
      binary_pkg = if (!is.null(built$binary_pkg)) basename(built$binary_pkg) else "(rust-source-only)",
      sec = elapsed,
      stringsAsFactors = FALSE
    )
  }

  df <- do.call(rbind, rows)
  print(df, row.names = FALSE)
  invisible(df)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2L && args[[1L]] == "--pypi") {
  pkg <- args[[2L]]
  sdist <- download_pypi_sdist(pkg)
  out_dir <- file.path(tempdir(), paste0("r_", pkg))
  res <- convert(sdist, out_dir)
  built <- build_r_package(res$path, dest_dir = dirname(out_dir), binary = TRUE)
  cat("Converted PyPI package:", pkg, "->", built$source_pkg, "/", built$binary_pkg, "\n")
} else if (sys.nframe() == 0L) {
  run_corpus_benchmark()
}
