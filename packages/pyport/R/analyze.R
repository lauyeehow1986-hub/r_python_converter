# Scope analysis, generator detection, and unsupported-feature diagnostics
# for normalized Python ASTs (`py_ast_node`).

#' Analyze scopes, bindings, generators, and diagnostics in a Python AST
#'
#' Walks a `py_ast_node` returned by [parse_python()] and computes lexical
#' scope information (parameters, local assignments, `global` and `nonlocal`
#' declarations, generator status for functions containing `yield` /
#' `yield from`, imported modules) together with structured diagnostics for
#' dynamic Python constructs (`eval()`, `exec()`, custom `metaclass=`, `async`).
#'
#' @param tree A `py_ast_node` (typically a `"Module"` node from [parse_python()]).
#' @returns An S3 list of class `"pyport_analysis"` with elements:
#'   - `scope`: root scope summary (`kind`, `locals`, `globals`, `nonlocals`,
#'     `params`, `is_generator`, `children`),
#'   - `imports`: character vector of top-level imported module names,
#'   - `diagnostics`: list of structured diagnostic records (`code`, `level`,
#'     `message`, `lineno`, `col_offset`).
#' @export
#' @examples
#' tree <- parse_python("def gen(n):\n    for i in range(n):\n        yield i * 2\n")
#' info <- analyze_python(tree)
#' info$scope$children[[1]]$is_generator
analyze_python <- function(tree) {
  if (!is_py_ast(tree)) {
    stop("`tree` must be a `py_ast_node` returned by `parse_python()`.")
  }
  diags <- list()
  imports <- character()

  add_diag <- function(code, level, message, node) {
    loc <- ast_loc(node)
    diags[[length(diags) + 1L]] <<- list(
      code = code,
      level = level,
      message = message,
      lineno = if (!is.null(loc)) loc$lineno else NA_integer_,
      col_offset = if (!is.null(loc)) loc$col_offset else NA_integer_
    )
  }

  analyze_scope <- function(node, kind, name = "<module>") {
    params <- character()
    if (kind == "function" && !is.null(node$args)) {
      a <- node$args
      for (arg_list in list(a$posonlyargs, a$args, a$kwonlyargs)) {
        for (p in arg_list) {
          if (!is.null(p$arg)) params <- c(params, p$arg)
        }
      }
      if (!is.null(a$vararg) && !is.null(a$vararg$arg)) params <- c(params, a$vararg$arg)
      if (!is.null(a$kwarg) && !is.null(a$kwarg$arg)) params <- c(params, a$kwarg$arg)
    }

    globals <- character()
    nonlocals <- character()
    assigned <- character()
    is_gen <- FALSE
    children <- list()

    record_target_names <- function(tgt) {
      if (is.null(tgt) || !is_py_ast(tgt)) return()
      switch(tgt$kind,
        Name = {
          assigned <<- c(assigned, tgt$id)
        },
        Tuple = ,
        List = {
          for (el in tgt$elts) record_target_names(el)
        },
        Starred = {
          record_target_names(tgt$value)
        },
        MatchAs = {
          if (!is.null(tgt$name)) assigned <<- c(assigned, tgt$name)
          if (!is.null(tgt$pattern)) record_target_names(tgt$pattern)
        },
        MatchStar = {
          if (!is.null(tgt$name)) assigned <<- c(assigned, tgt$name)
        },
        MatchMapping = {
          for (p in tgt$patterns) record_target_names(p)
          if (!is.null(tgt$rest)) assigned <<- c(assigned, tgt$rest)
        },
        MatchSequence = {
          for (p in tgt$patterns) record_target_names(p)
        },
        MatchClass = {
          for (p in tgt$patterns) record_target_names(p)
          for (p in tgt$kwd_patterns) record_target_names(p)
        },
        MatchOr = {
          for (p in tgt$patterns) record_target_names(p)
        },
        NULL
      )
    }

    walk <- function(n) {
      if (is.null(n)) return()
      if (is.list(n) && !is_py_ast(n)) {
        for (item in n) walk(item)
        return()
      }
      if (!is_py_ast(n)) return()

      nk <- n$kind
      if (nk %in% c("FunctionDef", "AsyncFunctionDef")) {
        if (nk == "AsyncFunctionDef") {
          add_diag("PYPORT_ASYNC_LOWERED", "note",
                   sprintf("Async function '%s' is lowered synchronously.", n$name), n)
        }
        assigned <<- c(assigned, n$name)
        # Walk default expressions and decorators in the enclosing scope.
        if (!is.null(n$args)) {
          walk(n$args$defaults)
          walk(n$args$kw_defaults)
        }
        walk(n$decorator_list)
        children[[length(children) + 1L]] <<- analyze_scope(n, "function", n$name)
        return()
      }
      if (nk == "Lambda") {
        if (!is.null(n$args)) {
          walk(n$args$defaults)
          walk(n$args$kw_defaults)
        }
        children[[length(children) + 1L]] <<- analyze_scope(n, "function", "<lambda>")
        return()
      }
      if (nk == "ClassDef") {
        assigned <<- c(assigned, n$name)
        walk(n$bases)
        for (kw in n$keywords) {
          if (identical(kw$arg, "metaclass")) {
            add_diag("PYPORT_METACLASS", "warning",
                     sprintf("Class '%s' specifies a custom metaclass; lowered to standard py_class.", n$name),
                     kw)
          }
          walk(kw$value)
        }
        walk(n$decorator_list)
        children[[length(children) + 1L]] <<- analyze_scope(n, "class", n$name)
        return()
      }
      if (nk %in% c("Yield", "YieldFrom")) {
        if (kind == "function") is_gen <<- TRUE
      }
      if (nk %in% c("Await", "AsyncFor", "AsyncWith")) {
        add_diag("PYPORT_ASYNC_LOWERED", "note",
                 sprintf("'%s' construct is lowered synchronously.", nk), n)
      }
      if (nk == "Global") {
        globals <<- c(globals, unlist(n$names))
        return()
      }
      if (nk == "Nonlocal") {
        nonlocals <<- c(nonlocals, unlist(n$names))
        return()
      }
      if (nk == "Assign") {
        for (t in n$targets) record_target_names(t)
      } else if (nk == "AnnAssign") {
        if (!is.null(n$value)) record_target_names(n$target)
      } else if (nk == "AugAssign") {
        record_target_names(n$target)
      } else if (nk == "NamedExpr") {
        record_target_names(n$target)
      } else if (nk %in% c("For", "AsyncFor")) {
        record_target_names(n$target)
      } else if (nk == "withitem") {
        if (!is.null(n$optional_vars)) record_target_names(n$optional_vars)
      } else if (nk == "ExceptHandler") {
        if (!is.null(n$name)) assigned <<- c(assigned, n$name)
      } else if (nk == "match_case") {
        record_target_names(n$pattern)
      } else if (nk == "Import") {
        for (al in n$names) {
          top_mod <- strsplit(al$name, ".", fixed = TRUE)[[1L]][[1L]]
          imports <<- unique(c(imports, top_mod))
          bind_name <- if (!is.null(al$asname)) al$asname else top_mod
          assigned <<- c(assigned, bind_name)
        }
      } else if (nk == "ImportFrom") {
        if (!is.null(n$module)) {
          top_mod <- strsplit(n$module, ".", fixed = TRUE)[[1L]][[1L]]
          imports <<- unique(c(imports, top_mod))
        }
        for (al in n$names) {
          bind_name <- if (!is.null(al$asname)) al$asname else al$name
          if (!identical(bind_name, "*")) assigned <<- c(assigned, bind_name)
        }
      } else if (nk == "Call") {
        if (is_py_ast(n$func, "Name") && n$func$id %in% c("eval", "exec")) {
          add_diag("PYPORT_DYNAMIC_EXEC", "warning",
                   sprintf("Dynamic '%s()' call cannot be statically transpiled without a runtime hook.", n$func$id),
                   n)
        }
      }

      # Recurse into child fields (excluding already-handled nested scopes).
      for (fn in setdiff(names(n), "kind")) {
        walk(n[[fn]])
      }
    }

    if (kind == "function") {
      walk(node$body)
    } else if (kind == "class") {
      walk(node$body)
    } else {
      for (fn in setdiff(names(node), "kind")) {
        walk(node[[fn]])
      }
    }

    globals <- unique(globals)
    nonlocals <- unique(nonlocals)
    params <- unique(params)
    locals <- setdiff(unique(c(params, assigned)), c(globals, nonlocals))

    list(
      kind = kind,
      name = name,
      params = params,
      locals = locals,
      globals = globals,
      nonlocals = nonlocals,
      is_generator = is_gen,
      children = children
    )
  }

  root_scope <- if (tree$kind %in% c("FunctionDef", "AsyncFunctionDef", "Lambda")) {
    analyze_scope(tree, "function", if (!is.null(tree$name)) tree$name else "<lambda>")
  } else if (identical(tree$kind, "ClassDef")) {
    analyze_scope(tree, "class", tree$name)
  } else {
    analyze_scope(tree, "module", "<module>")
  }
  structure(
    list(
      scope = root_scope,
      imports = imports,
      diagnostics = diags
    ),
    class = "pyport_analysis"
  )
}

#' Escape analysis for container bindings in a Python function AST
#'
#' Determines which local container variables (`x = [...]`, `d = {...}`,
#' `s = {...}`) in a Python function AST are strictly non-escaping (never
#' returned, yielded, passed as arguments to external calls, stored as attributes
#' or subscripts of other objects, or captured by inner closures/nonlocals) and
#' therefore safe for stack/value-semantics optimizations.
#'
#' @param fn_node A `"FunctionDef"` or `"AsyncFunctionDef"` `py_ast_node`.
#' @returns A list with character vectors `container_locals`, `escaping`, and
#'   `non_escaping`.
#' @export
#' @examples
#' tree <- parse_python("
#' def compute(n):
#'     scratch = [1, 2, 3]
#'     exported = [4, 5]
#'     return exported
#' ")
#' analyze_escape(tree$body[[1]])
analyze_escape <- function(fn_node) {
  if (!is_py_ast(fn_node, c("FunctionDef", "AsyncFunctionDef"))) {
    stop("`fn_node` must be a FunctionDef or AsyncFunctionDef AST node.")
  }
  scope_info <- analyze_python(fn_node)$scope
  container_locals <- character()
  escaping <- c(scope_info$globals, scope_info$nonlocals)

  # Any variable referenced inside a nested function/lambda escapes into closure
  for (ch in scope_info$children) {
    escaping <- c(escaping, ch$nonlocals)
  }

  collect_names <- function(node) {
    out <- character()
    walk_n <- function(x) {
      if (is.null(x)) return()
      if (is.list(x) && !is_py_ast(x)) {
        for (el in x) walk_n(el)
        return()
      }
      if (!is_py_ast(x)) return()
      if (identical(x$kind, "Name")) out <<- c(out, x$id)
      for (f in setdiff(names(x), "kind")) walk_n(x[[f]])
    }
    walk_n(node)
    unique(out)
  }

  walk_stmt <- function(n) {
    if (is.null(n)) return()
    if (is.list(n) && !is_py_ast(n)) {
      for (item in n) walk_stmt(item)
      return()
    }
    if (!is_py_ast(n)) return()
    nk <- n$kind
    if (nk %in% c("FunctionDef", "AsyncFunctionDef", "Lambda", "ClassDef")) {
      # Names used inside nested scope escape
      escaping <<- c(escaping, collect_names(n$body))
      return()
    }
    if (nk == "Assign" && length(n$targets) == 1L && is_py_ast(n$targets[[1L]], "Name")) {
      lhs <- n$targets[[1L]]$id
      if (is_py_ast(n$value, c("List", "Dict", "Set", "ListComp", "DictComp", "SetComp"))) {
        container_locals <<- unique(c(container_locals, lhs))
      } else if (is_py_ast(n$value, "Name")) {
        # Aliasing `y = x` causes both to escape
        escaping <<- c(escaping, lhs, n$value$id)
      }
    } else if (nk %in% c("Return", "Yield", "YieldFrom")) {
      escaping <<- c(escaping, collect_names(n$value))
    } else if (nk == "Call") {
      # Arguments to non-method function calls may escape
      for (a in n$args) escaping <<- c(escaping, collect_names(a))
      for (kw in n$keywords) escaping <<- c(escaping, collect_names(kw$value))
    }
    for (fn in setdiff(names(n), "kind")) walk_stmt(n[[fn]])
  }

  walk_stmt(fn_node$body)
  escaping <- unique(escaping)
  list(
    container_locals = container_locals,
    escaping = intersect(container_locals, escaping),
    non_escaping = setdiff(container_locals, escaping)
  )
}

