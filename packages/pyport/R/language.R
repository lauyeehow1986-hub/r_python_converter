#' tree-sitter language for Python
#'
#' `python_language()` returns a `tree_sitter_language` object for Python that
#' can be used with the \pkg{treesitter} package. It wraps the
#' 'tree-sitter-python' grammar vendored inside \pkg{pyport}, so no external
#' grammar package or Python installation is needed.
#'
#' @returns A `tree_sitter_language` object.
#' @export
#' @examples
#' python_language()
python_language <- function() {
  pointer <- .Call(ffi_language)
  new_language(pointer = pointer, abi = grammar_abi(), name = "python")
}

# Mirrors the standalone `new_language()` helper that the treesitter authors
# ask grammar packages to copy (https://unlicense.org).
new_language <- function(pointer, ..., abi = NULL, name = NULL) {
  if (typeof(pointer) != "externalptr") {
    stop("`pointer` must be an external pointer.")
  }
  if (!is.integer(abi) || length(abi) != 1L || is.na(abi)) {
    stop("`abi` must be a single integer.")
  }
  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a string.")
  }
  out <- list(pointer = pointer, abi = abi, name = name)
  class(out) <- "tree_sitter_language"
  out
}

# Cached parser; creating one per call is cheap but avoidable.
the <- new.env(parent = emptyenv())

python_parser <- function() {
  if (is.null(the$parser)) {
    the$parser <- treesitter::parser(python_language())
  }
  the$parser
}
