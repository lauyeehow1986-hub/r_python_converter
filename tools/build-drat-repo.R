#!/usr/bin/env Rscript
# tools/build-drat-repo.R
#
# Builds a CRAN-structured source and binary package repository (`repo/`)
# containing `pyrt`, `pyport`, and all converted corpus packages, complete with
# `PACKAGES`, `PACKAGES.gz`, and `PACKAGES.rds` indices generated via base R's
# `tools::write_PACKAGES()`.
#
# Once built, users can install any converted package directly into R with zero
# Python installation and zero compilation:
#   install.packages("more.itertools", repos = paste0("file:///", normalizePath("repo", winslash = "/")))

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
})

build_local_cran_repo <- function(repo_dir = file.path(tempdir(), "pyport_cran_repo")) {
  r_ver_short <- paste0(R.version$major, ".", strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][[1L]])
  src_contrib <- file.path(repo_dir, "src", "contrib")
  bin_contrib <- if (.Platform$OS.type == "windows") {
    file.path(repo_dir, "bin", "windows", "contrib", r_ver_short)
  } else if (grepl("darwin", R.version$os)) {
    file.path(repo_dir, "bin", "macosx", "contrib", r_ver_short)
  } else {
    file.path(repo_dir, "src", "contrib")
  }

  dir.create(src_contrib, recursive = TRUE, showWarnings = FALSE)
  dir.create(bin_contrib, recursive = TRUE, showWarnings = FALSE)

  # 1. Build `pyrt` and `pyport` source tarballs and platform binaries
  for (core_pkg in c("packages/pyrt", "packages/pyport")) {
    built <- build_r_package(core_pkg, dest_dir = src_contrib, binary = TRUE)
    if (!is.null(built$binary_pkg) && bin_contrib != src_contrib) {
      file.rename(built$binary_pkg, file.path(bin_contrib, basename(built$binary_pkg)))
    }
    cat("Built core package:", built$pkg_name, built$version, "\n")
  }

  # 2. Convert and build corpus packages
  work_conv <- tempfile("drat_conv_")
  dir.create(work_conv, recursive = TRUE)
  on.exit(unlink(work_conv, recursive = TRUE), add = TRUE)

  for (nm in list.dirs("corpus", recursive = FALSE, full.names = FALSE)) {
    if (nm == "orjson-rust") next
    res <- convert(file.path("corpus", nm), file.path(work_conv, nm))
    built <- build_r_package(res$path, dest_dir = src_contrib, binary = TRUE)
    if (!is.null(built$binary_pkg) && bin_contrib != src_contrib) {
      file.rename(built$binary_pkg, file.path(bin_contrib, basename(built$binary_pkg)))
    }
    cat("Built converted corpus package:", built$pkg_name, built$version, "\n")
  }

  # 3. Generate CRAN repository indices (`PACKAGES`, `PACKAGES.gz`, `PACKAGES.rds`)
  tools::write_PACKAGES(src_contrib, type = "source")
  if (.Platform$OS.type == "windows") {
    tools::write_PACKAGES(bin_contrib, type = "win.binary")
  } else if (grepl("darwin", R.version$os)) {
    tools::write_PACKAGES(bin_contrib, type = "mac.binary")
  }

  cat("CRAN-compatible repository ready at:", normalizePath(repo_dir, winslash = "/"), "\n")
  invisible(repo_dir)
}

if (sys.nframe() == 0L) {
  build_local_cran_repo()
}
