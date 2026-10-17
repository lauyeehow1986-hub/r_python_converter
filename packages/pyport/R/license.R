# License classifier and CRAN license mapper for converted Python packages.

#' Audit a Python package license and map it to a CRAN `License:` specification
#'
#' Classifies an upstream Python license identifier or declaration into
#' `"permissive"`, `"copyleft"` (GPL/LGPL/AGPL, inherited by the generated R
#' package), or `"proprietary"` (refused with a `pyport_license_error`).
#'
#' @param license_str Character string declaring the upstream Python license
#'   (e.g. `"MIT"`, `"BSD-3-Clause"`, `"Apache-2.0"`, `"GPL-3.0-or-later"`,
#'   `"Proprietary"`).
#' @returns A named list with elements:
#'   - `category`: `"permissive"` or `"copyleft"`,
#'   - `cran_license`: valid CRAN `DESCRIPTION` `License:` string,
#'   - `needs_license_file`: logical indicating whether a `LICENSE` template
#'     file (`YEAR:` / `COPYRIGHT HOLDER:`) is required by `R CMD check`,
#'   - `spdx`: normalized identifier string.
#' @export
#' @examples
#' check_python_license("MIT")
#' check_python_license("GPL-3.0-or-later")
check_python_license <- function(license_str) {
  if (is.null(license_str) || !is.character(license_str) || length(license_str) == 0L || !nzchar(trimws(license_str[[1L]]))) {
    license_str <- "MIT"
  }
  raw <- trimws(license_str[[1L]])
  low <- tolower(raw)

  # 1. Refuse proprietary / non-commercial / non-derivative licenses.
  refused_patterns <- c(
    "proprietary", "all rights reserved", "non-commercial", "noncommercial",
    "cc-by-nc", "cc-by-nd", "no derivatives", "do not distribute", "unlicensed"
  )
  for (pat in refused_patterns) {
    if (grepl(pat, low, fixed = TRUE)) {
      cond <- structure(
        list(
          message = sprintf(
            "Cannot convert package with non-free/non-derivative license '%s' (matched '%s').",
            raw, pat
          ),
          call = NULL,
          license = raw
        ),
        class = c("pyport_license_error", "error", "condition")
      )
      stop(cond)
    }
  }

  # 2. Copyleft (LGPL / AGPL / GPL): generated R package inherits compatible license.
  if (grepl("agpl", low, fixed = TRUE)) {
    return(list(
      category = "copyleft",
      cran_license = "AGPL (>= 3)",
      needs_license_file = FALSE,
      spdx = "AGPL-3.0-or-later"
    ))
  }
  if (grepl("lgpl", low, fixed = TRUE)) {
    cran <- if (grepl("2\\.1", low)) "LGPL (>= 2.1)" else if (grepl("2", low)) "LGPL (>= 2)" else "LGPL (>= 3)"
    return(list(
      category = "copyleft",
      cran_license = cran,
      needs_license_file = FALSE,
      spdx = "LGPL"
    ))
  }
  if (grepl("gpl", low, fixed = TRUE)) {
    cran <- if (grepl("2", low) && !grepl("3", low)) "GPL (>= 2)" else "GPL (>= 3)"
    return(list(
      category = "copyleft",
      cran_license = cran,
      needs_license_file = FALSE,
      spdx = "GPL"
    ))
  }

  # 3. Permissive licenses.
  if (grepl("apache", low, fixed = TRUE)) {
    return(list(
      category = "permissive",
      cran_license = "Apache License (>= 2.0)",
      needs_license_file = FALSE,
      spdx = "Apache-2.0"
    ))
  }
  if (grepl("bsd", low, fixed = TRUE)) {
    two_clause <- grepl("2", low, fixed = TRUE)
    return(list(
      category = "permissive",
      cran_license = if (two_clause) "BSD_2_clause + file LICENSE" else "BSD_3_clause + file LICENSE",
      needs_license_file = TRUE,
      spdx = if (two_clause) "BSD-2-Clause" else "BSD-3-Clause"
    ))
  }
  if (grepl("mpl", low, fixed = TRUE) || grepl("mozilla", low, fixed = TRUE)) {
    return(list(
      category = "permissive",
      cran_license = "MPL-2.0",
      needs_license_file = FALSE,
      spdx = "MPL-2.0"
    ))
  }

  # Default permissive (MIT / ISC / PSF):
  list(
    category = "permissive",
    cran_license = "MIT + file LICENSE",
    needs_license_file = TRUE,
    spdx = "MIT"
  )
}
