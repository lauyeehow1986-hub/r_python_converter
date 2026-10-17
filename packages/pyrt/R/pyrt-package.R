#' @keywords internal
#' @useDynLib pyrt, .registration = TRUE
"_PACKAGE"

#' Python `None`, `Ellipsis`, and truthiness helpers
#'
#' Foundational singletons and truthiness rules used by R code generated with
#' \pkg{pyport}. Phase 1 expands \pkg{pyrt} with reference-semantics containers
#' (`py_list`, `py_dict`, `py_set`, `py_tuple`), the Python class/MRO system,
#' exceptions, iteration, slicing, and bit-exact `random`.
#'
#' @param x Any R object.
#' @returns `py_none()` and `py_ellipsis()` return singleton objects;
#'   `is_py_none()`, `is_py_ellipsis()`, and `py_bool()` return a single
#'   `logical` (`TRUE` or `FALSE`).
#' @export
#' @examples
#' is_py_none(py_none())
#' py_bool(0L)
#' py_bool("non-empty")
py_none <- function() {
  NULL
}

#' @rdname py_none
#' @export
is_py_none <- function(x) {
  is.null(x)
}

the_ellipsis <- structure(list(), class = "pyrt_ellipsis")

#' @rdname py_none
#' @export
py_ellipsis <- function() {
  the_ellipsis
}

#' @rdname py_none
#' @export
is_py_ellipsis <- function(x) {
  inherits(x, "pyrt_ellipsis")
}

#' @export
format.pyrt_ellipsis <- function(x, ...) "Ellipsis"

#' @export
print.pyrt_ellipsis <- function(x, ...) {
  cat("Ellipsis\n")
  invisible(x)
}

#' @rdname py_none
#' @export
py_bool <- function(x) {
  if (is.null(x)) return(FALSE)
  if (inherits(x, "pyrt_ellipsis") || R6::is.R6(x)) return(TRUE)
  if (inherits(x, "pyrt_instance")) {
    bfn <- py_getattr_default(x, "__bool__", NULL)
    if (!is.null(bfn)) return(isTRUE(bfn()))
    lfn <- py_getattr_default(x, "__len__", NULL)
    if (!is.null(lfn)) return(as.integer(lfn()) > 0L)
    return(TRUE)
  }
  if (inherits(x, "pyrt_container") || inherits(x, "pyrt_range")) {
    return(py_len(x) > 0L)
  }
  if (gmp::is.bigz(x)) return(!all(x == 0L))
  if (is.logical(x) && length(x) == 1L) return(isTRUE(x))
  if ((is.numeric(x) || is.complex(x)) && length(x) == 1L) return(x != 0)
  if (is.character(x) && length(x) == 1L) return(nzchar(x))
  length(x) > 0L
}
