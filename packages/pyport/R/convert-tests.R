# Translator from Python `pytest` and `unittest` test suites into `testthat` R
# test files.

#' Convert a Python `pytest` / `unittest` test file into `testthat` R code
#'
#' Parses Python test code (`test_*.py`) via [parse_python()] and lowers
#' `def test_*():` functions and `class Test*(unittest.TestCase):` methods into
#' `testthat::test_that(...)` blocks, rewriting `assert`, `pytest.raises()`, and
#' `self.assert*()` calls into `testthat` expectations (`expect_true()`,
#' `expect_false()`, `expect_error()`).
#'
#' @param source Character string of Python test code or a `py_ast_node`.
#' @param pkg_name Optional name of the R package under test.
#' @returns A character string containing valid `testthat` R code.
#' @export
#' @examples
#' py_test <- "
#' def test_addition():
#'     assert 1 + 2 == 3
#'     with pytest.raises(ZeroDivisionError):
#'         _ = 1 // 0
#' "
#' cat(convert_pytest_code(py_test), "\n")
convert_pytest_code <- function(source, pkg_name = NULL) {
  tree <- if (is_py_ast(source)) {
    source
  } else if (is.character(source)) {
    parse_python(paste(source, collapse = "\n"))
  } else {
    stop("`source` must be a Python source string or a `py_ast_node`.")
  }

  rewrite_test_stmt <- function(st) {
    if (!is_py_ast(st)) return(st)
    # 1. `with pytest.raises(Exc): body` or `with self.assertRaises(Exc): body`
    if (st$kind == "With" && length(st$items) == 1L) {
      ce <- st$items[[1L]]$context_expr
      exc_name <- extract_raises_exc(ce)
      if (!is.null(exc_name)) {
        return(ast_node(
          "Expr",
          value = ast_node(
            "Call",
            func = ast_node("Name", id = paste0(".pyport_expect_raises__", exc_name), ctx = ast_op("Load")),
            args = list(ast_node("Lambda",
              args = ast_node("arguments",
                posonlyargs = list(), args = list(), vararg = NULL,
                kwonlyargs = list(), kw_defaults = list(), kwarg = NULL, defaults = list()
              ),
              body = if (length(st$body) == 1L && is_py_ast(st$body[[1L]], "Expr")) {
                st$body[[1L]]$value
              } else {
                # Wrap multi-statement body in a helper block via transpile_python
                ast_node("Constant", value = paste(vapply(st$body, function(b) {
                  as.character(transpile_python(ast_node("Module", body = list(b), type_ignores = list())))
                }, character(1)), collapse = "\n"), ctype = "r_raw_block")
              }
            )),
            keywords = list()
          )
        ))
      }
    }
    # 2. `self.assertEqual(a, b)` / `self.assertTrue(x)` / `self.assertFalse(x)`
    if (st$kind == "Expr" && is_py_ast(st$value, "Call") &&
        is_py_ast(st$value$func, "Attribute") &&
        is_py_ast(st$value$func$value, "Name") &&
        identical(st$value$func$value$id, "self")) {
      m <- st$value$func$attr
      args <- st$value$args
      if (m == "assertEqual" && length(args) >= 2L) {
        return(ast_node("Assert",
          test = ast_node("Compare", left = args[[1L]], ops = list(ast_op("Eq")), comparators = list(args[[2L]])),
          msg = NULL
        ))
      }
      if (m == "assertNotEqual" && length(args) >= 2L) {
        return(ast_node("Assert",
          test = ast_node("Compare", left = args[[1L]], ops = list(ast_op("NotEq")), comparators = list(args[[2L]])),
          msg = NULL
        ))
      }
      if (m == "assertTrue" && length(args) >= 1L) {
        return(ast_node("Assert", test = args[[1L]], msg = NULL))
      }
      if (m == "assertFalse" && length(args) >= 1L) {
        return(ast_node("Assert",
          test = ast_node("UnaryOp", op = ast_op("Not"), operand = args[[1L]]),
          msg = NULL
        ))
      }
      if (m == "assertIsNone" && length(args) >= 1L) {
        return(ast_node("Assert",
          test = ast_node("Compare",
            left = args[[1L]],
            ops = list(ast_op("Is")),
            comparators = list(ast_node("Constant", value = NULL, ctype = "None"))
          ),
          msg = NULL
        ))
      }
    }
    st
  }

  extract_raises_exc <- function(call_node) {
    if (!is_py_ast(call_node, "Call")) return(NULL)
    fn <- call_node$func
    is_raises <- (is_py_ast(fn, "Attribute") && fn$attr %in% c("raises", "assertRaises")) ||
                 (is_py_ast(fn, "Name") && fn$id %in% c("raises", "assertRaises"))
    if (!is_raises || length(call_node$args) == 0L) return(NULL)
    a1 <- call_node$args[[1L]]
    if (is_py_ast(a1, "Name")) return(a1$id)
    if (is_py_ast(a1, "Attribute")) return(a1$attr)
    "Exception"
  }

  emit_test_body <- function(stmts) {
    out_lines <- character()
    for (st in stmts) {
      # Skip docstring
      if (is_py_ast(st, "Expr") && is_py_ast(st$value, "Constant") && identical(st$value$ctype, "str")) {
        next
      }
      # Handle `with pytest.raises(Exc): ...` directly for clean testthat output
      if (st$kind == "With" && length(st$items) == 1L) {
        exc_name <- extract_raises_exc(st$items[[1L]]$context_expr)
        if (!is.null(exc_name)) {
          sub_mod <- ast_node("Module", body = st$body, type_ignores = list())
          sub_r <- strip_mod_header(as.character(transpile_python(sub_mod)))
          out_lines <- c(
            out_lines,
            sprintf("testthat::expect_error({\n%s\n}, class = %s)",
                    indent_block(sub_r, 2L),
                    paste0('"', exc_name, '"'))
          )
          next
        }
      }
      st2 <- rewrite_test_stmt(st)
      if (st2$kind == "Assert") {
        cond_mod <- ast_node("Module", body = list(ast_node("Expr", value = st2$test)), type_ignores = list())
        cond_r <- strip_mod_header(as.character(transpile_python(cond_mod)))
        out_lines <- c(out_lines, sprintf("testthat::expect_true(pyrt::py_bool(%s))", cond_r))
      } else {
        sub_mod <- ast_node("Module", body = list(st2), type_ignores = list())
        sub_r <- strip_mod_header(as.character(transpile_python(sub_mod)))
        out_lines <- c(out_lines, sub_r)
      }
    }
    paste(out_lines[nzchar(out_lines)], collapse = "\n")
  }

  strip_mod_header <- function(code) {
    lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    keep <- !grepl("^# Transpiled", lines) &
            !grepl("^if \\(!exists\\(\"\\.py_mod_env\"", lines)
    paste(lines[keep], collapse = "\n")
  }

  indent_block <- function(code, spaces = 2L) {
    if (!nzchar(code)) return("")
    pad <- strrep(" ", spaces)
    lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    paste(paste0(pad, lines), collapse = "\n")
  }

  setup_stmts <- list()
  test_blocks <- character()

  for (st in tree$body) {
    # Skip `import pytest` / `import unittest`
    if (st$kind == "Import") {
      nms <- vapply(st$names, `[[`, character(1), "name")
      if (all(nms %in% c("pytest", "unittest"))) next
    }
    if (st$kind == "ImportFrom" && !is.null(st$module) && st$module %in% c("pytest", "unittest")) {
      next
    }
    # If `from <pkg> import ...` imports from the package under test, skip since
    # `testthat` already attaches the package namespace!
    if (st$kind == "ImportFrom" && !is.null(pkg_name) && !is.null(st$module)) {
      norm <- function(x) tolower(gsub("[-_.]", "", x))
      mod_top <- strsplit(st$module, ".", fixed = TRUE)[[1L]][[1L]]
      if (norm(mod_top) %in% norm(pkg_name)) next
    }

    if (st$kind == "FunctionDef" && grepl("^test_", st$name)) {
      tbody <- emit_test_body(st$body)
      test_blocks <- c(
        test_blocks,
        sprintf("test_that(%s, {\n%s\n})",
                paste0('"', st$name, '"'),
                indent_block(tbody, 2L))
      )
    } else if (st$kind == "ClassDef" && grepl("^Test", st$name)) {
      for (mst in st$body) {
        if (mst$kind == "FunctionDef" && grepl("^test_", mst$name)) {
          tbody <- emit_test_body(mst$body)
          test_blocks <- c(
            test_blocks,
            sprintf("test_that(%s, {\n%s\n})",
                    paste0('"', st$name, "::", mst$name, '"'),
                    indent_block(tbody, 2L))
          )
        }
      }
    } else {
      setup_stmts[[length(setup_stmts) + 1L]] <- st
    }
  }

  header_r <- if (length(setup_stmts) > 0L) {
    strip_mod_header(as.character(transpile_python(ast_node("Module", body = setup_stmts, type_ignores = list()))))
  } else {
    character()
  }

  paste(c(header_r[nzchar(header_r)], test_blocks), collapse = "\n\n")
}
