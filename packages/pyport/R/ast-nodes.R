# Constructor and printing helpers for normalized Python AST nodes.
#
# Every AST node is an S3 list with classes `c("py_ast_<Kind>", "py_ast_node")`
# and a `kind` element matching `<Kind>` (e.g. "Module", "FunctionDef", "BinOp",
# "Constant"). Field names follow Python's `ast` module so downstream passes
# and differential tests against CPython's `ast.dump()` share one vocabulary.
# Source spans are stored on the `loc` attribute (1-based line, 0-based byte
# column, matching CPython's `lineno`/`col_offset`/`end_lineno`/`end_col_offset`)
# rather than as list elements, so structural comparison (`identical()`) ignores
# source positions when desired.

ast_node <- function(kind, ..., .ts_node = NULL, .loc = NULL) {
  fields <- list(...)
  out <- c(list(kind = kind), fields)
  if (!is.null(.ts_node)) {
    .loc <- node_loc(.ts_node)
  }
  if (!is.null(.loc)) {
    attr(out, "loc") <- .loc
  }
  class(out) <- c(paste0("py_ast_", kind), "py_ast_node")
  out
}

# Operator / context singleton token (e.g. Add, Eq, And, Load, Store).
ast_op <- function(kind) {
  structure(list(kind = kind), class = c(paste0("py_ast_", kind), "py_ast_op", "py_ast_node"))
}

node_loc <- function(node) {
  sp <- treesitter::node_start_point(node)
  ep <- treesitter::node_end_point(node)
  list(
    lineno = as.integer(treesitter::point_row(sp)) + 1L,
    col_offset = as.integer(treesitter::point_column(sp)),
    end_lineno = as.integer(treesitter::point_row(ep)) + 1L,
    end_col_offset = as.integer(treesitter::point_column(ep))
  )
}

#' Test whether an object is a Python AST node
#'
#' @param x Any R object.
#' @param kind Optional character vector of node kind names (e.g. `"FunctionDef"`,
#'   `"BinOp"`). If supplied, also checks that `x$kind` is one of `kind`.
#' @returns A single logical value.
#' @export
is_py_ast <- function(x, kind = NULL) {
  if (!inherits(x, "py_ast_node")) {
    return(FALSE)
  }
  if (is.null(kind)) {
    return(TRUE)
  }
  is.character(x$kind) && x$kind %in% kind
}

#' Source location of a Python AST node
#'
#' Returns the 1-based line numbers and 0-based byte column offsets attached to
#' an AST node (matching Python's `lineno`, `col_offset`, `end_lineno`,
#' `end_col_offset`), or `NULL` for nodes that carry no location (such as
#' operator tags).
#'
#' @param x A `py_ast_node`.
#' @returns A named list with elements `lineno`, `col_offset`, `end_lineno`,
#'   `end_col_offset`, or `NULL`.
#' @export
ast_loc <- function(x) {
  attr(x, "loc", exact = TRUE)
}

#' Format a Python AST in `ast.dump()` style
#'
#' Produces a deterministic string representation matching Python's
#' `ast.dump(node, include_attributes = ...)` closely enough to be compared in
#' tests and used for debugging.
#'
#' @param x A `py_ast_node` returned by [parse_python()].
#' @param indent If `NULL` (the default), formats on a single line. If a
#'   non-negative integer, indents nested levels by that many spaces.
#' @param include_attributes Logical; if `TRUE`, appends `lineno`,
#'   `col_offset`, `end_lineno`, and `end_col_offset`.
#' @returns A single character string.
#' @export
#' @examples
#' tree <- parse_python("x = 1 + 2")
#' cat(ast_dump(tree, indent = 2), "\n")
ast_dump <- function(x, indent = NULL, include_attributes = FALSE) {
  if (!inherits(x, "py_ast_node")) {
    stop("`x` must be a `py_ast_node` object.")
  }
  step <- if (is.null(indent)) NULL else as.integer(indent)
  dump_value(x, step = step, depth = 0L, include_attributes = isTRUE(include_attributes))
}

#' @export
format.py_ast_node <- function(x, ...) {
  ast_dump(x, indent = 2L, ...)
}

#' @export
print.py_ast_node <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

dump_value <- function(v, step, depth, include_attributes) {
  if (is.null(v)) {
    return("None")
  }
  if (inherits(v, "py_ast_op")) {
    return(paste0(v$kind, "()"))
  }
  if (inherits(v, "py_ast_node")) {
    fn <- setdiff(names(v), "kind")
    # Constant carries an auxiliary `ctype` tag ("None", "bool", "int", "float",
    # "complex", "str", "bytes", "ellipsis") so R can distinguish None / True /
    # bigints / bytes; CPython's ast.dump only shows `value` and `kind`.
    if (identical(v$kind, "Constant")) {
      return(dump_constant(v, step, depth, include_attributes))
    }
    attrs <- character()
    if (include_attributes && !is.null(loc <- ast_loc(v))) {
      attrs <- c(
        paste0("lineno=", loc$lineno),
        paste0("col_offset=", loc$col_offset),
        paste0("end_lineno=", loc$end_lineno),
        paste0("end_col_offset=", loc$end_col_offset)
      )
    }
    # Python 3.8+ `ast.dump()` omits fields whose value is `None`, except on
    # `MatchSingleton(value=None)` where `value` is the sole required field.
    non_null_fn <- fn[!vapply(fn, function(f) {
      is.null(v[[f]]) && !identical(v$kind, "MatchSingleton")
    }, logical(1))]
    parts <- vapply(non_null_fn, function(f) {
      paste0(f, "=", dump_value(v[[f]], step, depth + 1L, include_attributes))
    }, character(1))
    all_parts <- c(parts, attrs)
    if (length(all_parts) == 0L) {
      return(paste0(v$kind, "()"))
    }
    if (is.null(step)) {
      return(paste0(v$kind, "(", paste(all_parts, collapse = ", "), ")"))
    }
    pad <- strrep(" ", (depth + 1L) * step)
    end_pad <- strrep(" ", depth * step)
    return(paste0(
      v$kind, "(\n",
      pad, paste(all_parts, collapse = paste0(",\n", pad)),
      "\n", end_pad, ")"
    ))
  }
  if (is.list(v)) {
    if (length(v) == 0L) {
      return("[]")
    }
    items <- vapply(v, dump_value, character(1),
                    step = step, depth = depth + 1L,
                    include_attributes = include_attributes)
    if (is.null(step)) {
      return(paste0("[", paste(items, collapse = ", "), "]"))
    }
    pad <- strrep(" ", (depth + 1L) * step)
    end_pad <- strrep(" ", depth * step)
    return(paste0("[\n", pad, paste(items, collapse = paste0(",\n", pad)), "\n", end_pad, "]"))
  }
  if (is.logical(v) && length(v) == 1L) {
    return(if (is.na(v)) "None" else if (v) "True" else "False")
  }
  if (is.integer(v) && length(v) == 1L) {
    return(as.character(v))
  }
  if (is.character(v) && length(v) == 1L) {
    return(py_repr_str(v))
  }
  paste(format(v), collapse = " ")
}

dump_constant <- function(v, step, depth, include_attributes) {
  val_str <- switch(v$ctype,
    None = "None",
    ellipsis = "Ellipsis",
    bool = if (isTRUE(v$value)) "True" else "False",
    int = as.character(v$value),
    float = py_repr_float(v$value),
    complex = {
      im <- Im(v$value)
      im_str <- if (is.finite(im) && im == trunc(im) && abs(im) < 1e16) {
        sprintf("%.0fj", im)
      } else {
        paste0(py_repr_float(im), "j")
      }
      if (Re(v$value) == 0) im_str else paste0(py_repr_float(Re(v$value)), "+", im_str)
    },
    str = py_repr_str(v$value),
    bytes = py_repr_bytes(v$value),
    format(v$value)
  )
  parts <- paste0("value=", val_str)
  if (!is.null(v$s_kind)) {
    parts <- c(parts, paste0("kind=", py_repr_str(v$s_kind)))
  }
  if (include_attributes && !is.null(loc <- ast_loc(v))) {
    parts <- c(
      parts,
      paste0("lineno=", loc$lineno),
      paste0("col_offset=", loc$col_offset),
      paste0("end_lineno=", loc$end_lineno),
      paste0("end_col_offset=", loc$end_col_offset)
    )
  }
  paste0("Constant(", paste(parts, collapse = ", "), ")")
}

py_repr_float <- function(x) {
  if (is.nan(x)) return("nan")
  if (is.infinite(x)) return(if (x > 0) "inf" else "-inf")
  if (x == 0) return(if (1 / x < 0) "-0.0" else "0.0")
  # Shortest decimal that round-trips to the exact IEEE-754 double via strtod().
  s <- NULL
  for (d in 15:17) {
    cand <- format(x, digits = d, scientific = NA, trim = TRUE)
    if (identical(.Call(ffi_strtod, cand), x)) {
      s <- cand
      break
    }
  }
  if (is.null(s)) {
    s <- format(x, digits = 17, scientific = NA, trim = TRUE)
  }
  if (!grepl("[.eE]", s)) paste0(s, ".0") else s
}

py_repr_str <- function(s) {
  if (is.null(s) || length(s) == 0L) return("''")
  s <- enc2utf8(s)
  # Match Python's quote rule: single quotes unless the string contains `'`
  # and no `"`.
  use_double <- grepl("'", s, fixed = TRUE) && !grepl('"', s, fixed = TRUE)
  q <- if (use_double) '"' else "'"
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  if (!use_double) {
    s <- gsub("'", "\\'", s, fixed = TRUE)
  } else {
    s <- gsub('"', '\\"', s, fixed = TRUE)
  }
  paste0(q, s, q)
}

py_repr_bytes <- function(b) {
  if (length(b) == 0L) return("b''")
  ints <- as.integer(b)
  chars <- vapply(ints, function(byte) {
    if (byte == 0x5cL) "\\\\"
    else if (byte == 0x27L) "\\'"
    else if (byte == 0x0aL) "\\n"
    else if (byte == 0x0dL) "\\r"
    else if (byte == 0x09L) "\\t"
    else if (byte >= 0x20L && byte <= 0x7eL) rawToChar(as.raw(byte))
    else sprintf("\\x%02x", byte)
  }, character(1))
  paste0("b'", paste(chars, collapse = ""), "'")
}
