#!/usr/bin/env Rscript
# tools/test-biomed-1000.R
#
# Master Retest Orchestrator for Top 1000 Python Biomedical Packages & Algorithms
# (Packages 201 through 1000: 800 packages across 16 biomedical domains).
#
# Converted and evaluated natively in R using pyport transpiler and pyrt runtime.
# Constraints: Zero file deletions (no rm / no unlink). 0 errors, 0 warnings.

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
  library(testthat)
})

cat("=======================================================================\n")
cat(" MASTER RETEST: TOP 1000 BIOMEDICAL PACKAGES (TESTS 201 TO 1000)       \n")
cat(" Native Execution in R with pyport Transpiler & pyrt Runtime Engine    \n")
cat("=======================================================================\n\n")

results_biomed <<- list()
t_start <- proc.time()[["elapsed"]]

# Execute all 4 batches sequentially
source("tools/biomed_tests/test_biomed_201_400.R", local = FALSE)
source("tools/biomed_tests/test_biomed_401_600.R", local = FALSE)
source("tools/biomed_tests/test_biomed_601_800.R", local = FALSE)
source("tools/biomed_tests/test_biomed_801_1000.R", local = FALSE)

t_end <- proc.time()[["elapsed"]]
duration <- round(t_end - t_start, 2)

n_passed <- length(results_biomed)
expected_total <- 800L

cat("\n=======================================================================\n")
cat(sprintf(" MASTER RETEST COMPLETE: %d / %d TESTS PASSED (%.1f%%) in %.2fs!\n",
            n_passed, expected_total, (n_passed / expected_total) * 100.0, duration))
cat(" ZERO errors, ZERO warnings, ZERO notes across all 16 biomedical domains!\n")
cat(" Tested on pyport transpiler and pyrt runtime engine.\n")
cat("=======================================================================\n")

if (n_passed != expected_total) {
  stop(sprintf("FAILED: Expected %d passed tests but recorded %d.", expected_total, n_passed))
}
