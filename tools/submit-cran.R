#!/usr/bin/env Rscript
# tools/submit-cran.R
#
# Prepares `pyrt` 0.1.0 and `pyport` 0.1.0 source tarballs in `dist/` for CRAN
# submission and optionally submits them to CRAN's web submission portal
# (`https://xmpalantir.wu.ac.at/cransubmit/index2.php`).
#
# Usage:
#   Rscript tools/submit-cran.R                      # build & verify dist/*.tar.gz
#   Rscript tools/submit-cran.R --submit pyrt        # upload dist/pyrt_0.1.0.tar.gz to CRAN
#   Rscript tools/submit-cran.R --submit pyport      # upload dist/pyport_0.1.0.tar.gz to CRAN

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
})

dir.create("dist", showWarnings = FALSE)

b_pyrt <- build_r_package("packages/pyrt", dest_dir = "dist", binary = TRUE)
b_pyport <- build_r_package("packages/pyport", dest_dir = "dist", binary = TRUE)

cat("Built CRAN artifacts in dist/:\n")
cat("  -", b_pyrt$source_pkg, "\n")
cat("  -", b_pyrt$binary_pkg, "\n")
cat("  -", b_pyport$source_pkg, "\n")
cat("  -", b_pyport$binary_pkg, "\n")

check_maintainer <- function(pkg_dir) {
  desc <- read.dcf(file.path(pkg_dir, "DESCRIPTION"))
  authors <- desc[1L, "Authors@R"]
  if (grepl("example\\.com|pyport authors", authors)) {
    stop(paste0(
      "CRAN requires a real human maintainer name and working email address in ",
      file.path(pkg_dir, "DESCRIPTION"),
      " (`Authors@R`) before submitting, because CRAN sends a confirmation link to that email.\n",
      "Update `Authors@R` with your real First/Last name and email, then re-run `--submit`."
    ))
  }
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 2L && args[[1L]] == "--submit") {
  target <- args[[2L]]
  pkg_dir <- file.path("packages", target)
  check_maintainer(pkg_dir)
  tarball <- file.path("dist", sprintf("%s_0.1.0.tar.gz", target))
  if (!file.exists(tarball)) {
    stop(sprintf("Tarball not found: '%s'. Run without --submit first.", tarball))
  }
  cat("Submitting", tarball, "to CRAN via devtools:::upload_cran...\n")
  devtools:::upload_cran(pkg = pkg_dir, built_path = tarball)
}
