# AST-to-R transpiler for the pure-Python subset.
#
# Lowers normalized `py_ast_node` trees (from `parse_python()`) into standalone,
# human-readable R code that executes against `pyrt` without Python.

r_reserved_words <- c(
  "if", "else", "repeat", "while", "function", "for", "in", "next", "break",
  "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA", "NA_integer_", "NA_real_",
  "NA_complex_", "NA_character_"
)

r_ident <- function(name) {
  if (name %in% r_reserved_words || !grepl("^[A-Za-z.][A-Za-z0-9._]*$", name)) {
    paste0("`", gsub("`", "\\\\`", name), "`")
  } else {
    name
  }
}

deparse_r_str <- function(s) {
  if (is.null(s) || length(s) == 0L) return('""')
  s <- enc2utf8(s)
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub('"', '\\"', s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  paste0('"', s, '"')
}

ast_has_yield <- function(node) {
  if (is.null(node)) return(FALSE)
  if (is.list(node) && !is_py_ast(node)) {
    return(any(vapply(node, ast_has_yield, logical(1))))
  }
  if (!is_py_ast(node)) return(FALSE)
  if (node$kind %in% c("Yield", "YieldFrom")) return(TRUE)
  if (node$kind %in% c("FunctionDef", "AsyncFunctionDef", "ClassDef", "Lambda")) {
    return(FALSE)
  }
  for (fn in setdiff(names(node), "kind")) {
    if (ast_has_yield(node[[fn]])) return(TRUE)
  }
  FALSE
}

#' Transpile Python source code or an AST into R code
#'
#' Converts Python 3 source code (or a `py_ast_node` returned by
#' [parse_python()]) into idiomatic R source code backed by `pyrt`.
#'
#' @param source Either a character string of Python source code or a
#'   `py_ast_node` returned by [parse_python()].
#' @param module_name Optional module name string included in comments/metadata.
#' @returns A character string of class `"pyport_r_code"` with attributes
#'   `"analysis"` (from [analyze_python()]) and `"diagnostics"`.
#' @export
#' @examples
#' r_src <- transpile_python("
#' def fib(n):
#'     a, b = 0, 1
#'     for _ in range(n):
#'         a, b = b, a + b
#'     return a
#' ")
#' cat(r_src, "\n")
transpile_python <- function(source, module_name = NULL) {
  tree <- if (is_py_ast(source)) {
    source
  } else if (is.character(source)) {
    parse_python(paste(source, collapse = "\n"))
  } else {
    stop("`source` must be a Python source string or a `py_ast_node`.")
  }
  analysis <- analyze_python(tree)

  counter <- 0L
  gensym <- function(prefix = "tmp") {
    counter <<- counter + 1L
    paste0(".py_", prefix, "_", counter)
  }

  make_ctx <- function(scope, parent_ctx = NULL,
                       in_loop = FALSE, loop_flag = NULL,
                       in_try_or_with = FALSE, in_cps = FALSE,
                       current_class = NULL, self_param = NULL) {
    known <- scope$locals
    if (!is.null(parent_ctx)) {
      known <- unique(c(known, parent_ctx$known_vars))
    }
    list(
      scope = scope,
      parent = parent_ctx,
      known_vars = known,
      globals = scope$globals,
      nonlocals = scope$nonlocals,
      in_loop = in_loop,
      loop_flag = loop_flag,
      in_try_or_with = in_try_or_with,
      in_cps = in_cps,
      current_class = current_class,
      self_param = self_param
    )
  }

  indent_lines <- function(code, spaces = 2L) {
    if (!nzchar(code)) return("")
    pad <- strrep(" ", spaces)
    lines <- strsplit(code, "\n", fixed = TRUE)[[1L]]
    paste(paste0(pad, lines), collapse = "\n")
  }

  # ---- Expression lowering --------------------------------------------------

  emit_expr <- function(node, ctx) {
    if (is.null(node)) return("pyrt::py_none()")
    if (!is_py_ast(node)) stop("Expected AST node in emit_expr().")

    switch(node$kind,
      Constant = emit_constant(node),
      Name = emit_name_load(node$id, ctx),
      BinOp = emit_binop(node, ctx),
      UnaryOp = emit_unaryop(node, ctx),
      BoolOp = emit_boolop(node, ctx),
      Compare = emit_compare(node, ctx),
      IfExp = sprintf(
        "(if (pyrt::py_bool(%s)) %s else %s)",
        emit_expr(node$test, ctx),
        emit_expr(node$body, ctx),
        emit_expr(node$orelse, ctx)
      ),
      NamedExpr = sprintf(
        "(%s)",
        emit_assign_target(node$target, emit_expr(node$value, ctx), ctx)
      ),
      JoinedStr = emit_joined_str(node, ctx),
      FormattedValue = emit_formatted_value(node, ctx),
      List = emit_seq_literal("pyrt::py_list", node$elts, ctx),
      Tuple = emit_seq_literal("pyrt::py_tuple", node$elts, ctx),
      Set = emit_seq_literal("pyrt::py_set", node$elts, ctx),
      Dict = emit_dict_literal(node, ctx),
      Attribute = sprintf(
        "pyrt::py_getattr(%s, %s)",
        emit_expr(node$value, ctx),
        deparse_r_str(node$attr)
      ),
      Subscript = emit_subscript_load(node, ctx),
      Slice = emit_slice(node, ctx),
      Call = emit_call(node, ctx),
      Lambda = emit_lambda(node, ctx),
      ListComp = emit_comp(node, "list", ctx),
      SetComp = emit_comp(node, "set", ctx),
      DictComp = emit_comp(node, "dict", ctx),
      GeneratorExp = emit_genexp(node, ctx),
      Await = emit_expr(node$value, ctx),
      Starred = emit_expr(node$value, ctx),
      stop(sprintf("Unsupported expression node kind: '%s'", node$kind))
    )
  }

  emit_constant <- function(node) {
    val <- node$value
    switch(node$ctype,
      None = "pyrt::py_none()",
      ellipsis = "pyrt::py_ellipsis()",
      bool = if (isTRUE(val)) "TRUE" else "FALSE",
      int = {
        if (gmp::is.bigz(val)) {
          sprintf("gmp::as.bigz(%s)", deparse_r_str(as.character(val)))
        } else {
          paste0(as.character(val), "L")
        }
      },
      float = {
        if (is.nan(val)) "NaN"
        else if (is.infinite(val)) (if (val > 0) "Inf" else "-Inf")
        else py_repr_float(val)
      },
      complex = sprintf("complex(real = %s, imaginary = %s)", py_repr_float(Re(val)), py_repr_float(Im(val))),
      str = deparse_r_str(val),
      bytes = {
        if (length(val) == 0L) "raw(0L)"
        else sprintf("as.raw(c(%s))", paste0("0x", as.character(val), "L", collapse = ", "))
      },
      deparse_r_str(as.character(val))
    )
  }

  emit_name_load <- function(id, ctx) {
    if (ctx$scope$kind != "module" && id %in% ctx$globals) {
      return(sprintf("get(%s, envir = .py_mod_env, inherits = TRUE)", deparse_r_str(id)))
    }
    if (!(id %in% ctx$known_vars) && id %in% pyrt::py_builtin_names()) {
      return(sprintf("pyrt::py_builtin(%s)", deparse_r_str(id)))
    }
    r_ident(id)
  }

  emit_binop <- function(node, ctx) {
    l <- emit_expr(node$left, ctx)
    r <- emit_expr(node$right, ctx)
    fn <- switch(node$op$kind,
      Add = "pyrt::py_add",
      Sub = "pyrt::py_sub",
      Mult = "pyrt::py_mul",
      Div = "pyrt::py_truediv",
      FloorDiv = "pyrt::py_floordiv",
      Mod = "pyrt::py_mod",
      Pow = "pyrt::py_pow",
      BitAnd = "pyrt::py_bitand",
      BitOr = "pyrt::py_bitor",
      BitXor = "pyrt::py_bitxor",
      LShift = "pyrt::py_lshift",
      RShift = "pyrt::py_rshift",
      MatMult = "pyrt::py_matmul"
    )
    sprintf("%s(%s, %s)", fn, l, r)
  }

  emit_unaryop <- function(node, ctx) {
    x <- emit_expr(node$operand, ctx)
    switch(node$op$kind,
      UAdd = sprintf("pyrt::py_pos(%s)", x),
      USub = sprintf("pyrt::py_neg(%s)", x),
      Not = sprintf("(!pyrt::py_bool(%s))", x),
      Invert = sprintf("pyrt::py_invert(%s)", x)
    )
  }

  emit_boolop <- function(node, ctx) {
    vals <- node$values
    n <- length(vals)
    is_and <- identical(node$op$kind, "And")
    expr <- emit_expr(vals[[n]], ctx)
    for (i in seq(n - 1L, 1L, by = -1L)) {
      tmp <- gensym("bool")
      lhs <- emit_expr(vals[[i]], ctx)
      cond <- if (is_and) sprintf("!pyrt::py_bool(%s)", tmp) else sprintf("pyrt::py_bool(%s)", tmp)
      expr <- sprintf("{ %s <- %s; if (%s) %s else %s }", tmp, lhs, cond, tmp, expr)
    }
    sprintf("(%s)", expr)
  }

  emit_compare_single <- function(op_kind, l, r) {
    switch(op_kind,
      Eq = sprintf("pyrt::py_eq(%s, %s)", l, r),
      NotEq = sprintf("pyrt::py_ne(%s, %s)", l, r),
      Lt = sprintf("pyrt::py_lt(%s, %s)", l, r),
      LtE = sprintf("pyrt::py_le(%s, %s)", l, r),
      Gt = sprintf("pyrt::py_gt(%s, %s)", l, r),
      GtE = sprintf("pyrt::py_ge(%s, %s)", l, r),
      Is = sprintf("pyrt::py_is(%s, %s)", l, r),
      IsNot = sprintf("(!pyrt::py_is(%s, %s))", l, r),
      In = sprintf("pyrt::py_in(%s, %s)", l, r),
      NotIn = sprintf("(!pyrt::py_in(%s, %s))", l, r)
    )
  }

  emit_compare <- function(node, ctx) {
    n <- length(node$ops)
    if (n == 1L) {
      return(emit_compare_single(
        node$ops[[1L]]$kind,
        emit_expr(node$left, ctx),
        emit_expr(node$comparators[[1L]], ctx)
      ))
    }
    # Chained comparison `a < b < c`: evaluate interior operands once.
    prev_var <- gensym("cmp")
    stmts <- sprintf("%s <- %s", prev_var, emit_expr(node$left, ctx))
    checks <- list()
    for (i in seq_len(n)) {
      cur_var <- gensym("cmp")
      stmts <- c(stmts, sprintf("%s <- %s", cur_var, emit_expr(node$comparators[[i]], ctx)))
      checks[[i]] <- list(prev = prev_var, cur = cur_var, op = node$ops[[i]]$kind)
      prev_var <- cur_var
    }
    chain <- emit_compare_single(checks[[n]]$op, checks[[n]]$prev, checks[[n]]$cur)
    for (i in seq(n - 1L, 1L, by = -1L)) {
      step_cmp <- emit_compare_single(checks[[i]]$op, checks[[i]]$prev, checks[[i]]$cur)
      chain <- sprintf("if (%s) { %s; %s } else FALSE", step_cmp, stmts[[i + 2L]], chain)
    }
    sprintf("({ %s; %s; %s })", stmts[[1L]], stmts[[2L]], chain)
  }

  emit_joined_str <- function(node, ctx) {
    if (length(node$values) == 0L) return('""')
    parts <- vapply(node$values, function(v) {
      if (is_py_ast(v, "Constant")) deparse_r_str(v$value)
      else emit_formatted_value(v, ctx)
    }, character(1))
    sprintf("pyrt::py_fstring(%s)", paste(parts, collapse = ", "))
  }

  emit_formatted_value <- function(node, ctx) {
    val_code <- emit_expr(node$value, ctx)
    spec_code <- if (is.null(node$format_spec)) {
      '""'
    } else if (is_py_ast(node$format_spec, "JoinedStr") &&
               length(node$format_spec$values) == 1L &&
               is_py_ast(node$format_spec$values[[1L]], "Constant")) {
      deparse_r_str(node$format_spec$values[[1L]]$value)
    } else {
      emit_expr(node$format_spec, ctx)
    }
    conv_code <- if (is.null(node$conversion) || node$conversion < 0L) {
      "-1L"
    } else {
      paste0(as.integer(node$conversion), "L")
    }
    sprintf("pyrt::py_format(%s, %s, %s)", val_code, spec_code, conv_code)
  }

  emit_seq_literal <- function(ctor, elts, ctx) {
    if (length(elts) == 0L) return(sprintf("%s()", ctor))
    has_star <- any(vapply(elts, function(e) is_py_ast(e, "Starred"), logical(1)))
    if (!has_star) {
      items <- vapply(elts, emit_expr, character(1), ctx = ctx)
      return(sprintf("%s(list(%s))", ctor, paste(items, collapse = ", ")))
    }
    chunks <- vapply(elts, function(e) {
      if (is_py_ast(e, "Starred")) {
        sprintf("pyrt::py_collect(%s)", emit_expr(e$value, ctx))
      } else {
        sprintf("list(%s)", emit_expr(e, ctx))
      }
    }, character(1))
    sprintf("%s(c(%s))", ctor, paste(chunks, collapse = ", "))
  }

  emit_dict_literal <- function(node, ctx) {
    n <- length(node$keys)
    if (n == 0L) return("pyrt::py_dict()")
    has_unpack <- any(vapply(node$keys, is.null, logical(1)))
    if (!has_unpack) {
      pairs <- vapply(seq_len(n), function(i) {
        sprintf("list(%s, %s)",
                emit_expr(node$keys[[i]], ctx),
                emit_expr(node$values[[i]], ctx))
      }, character(1))
      return(sprintf("pyrt::py_dict(list(%s))", paste(pairs, collapse = ", ")))
    }
    dvar <- gensym("dict")
    ops <- sprintf("%s <- pyrt::py_dict()", dvar)
    for (i in seq_len(n)) {
      if (is.null(node$keys[[i]])) {
        ops <- c(ops, sprintf("%s$update(%s)", dvar, emit_expr(node$values[[i]], ctx)))
      } else {
        ops <- c(ops, sprintf("%s$set(%s, %s)",
                              dvar,
                              emit_expr(node$keys[[i]], ctx),
                              emit_expr(node$values[[i]], ctx)))
      }
    }
    ops <- c(ops, dvar)
    sprintf("({ %s })", paste(ops, collapse = "; "))
  }

  emit_slice <- function(node, ctx) {
    lo <- if (is.null(node$lower)) "NULL" else emit_expr(node$lower, ctx)
    hi <- if (is.null(node$upper)) "NULL" else emit_expr(node$upper, ctx)
    st <- if (is.null(node$step)) "NULL" else emit_expr(node$step, ctx)
    sprintf("pyrt::py_slice(%s, %s, %s)", lo, hi, st)
  }

  emit_subscript_load <- function(node, ctx) {
    val <- emit_expr(node$value, ctx)
    sl <- emit_expr(node$slice, ctx)
    sprintf("pyrt::py_getitem(%s, %s)", val, sl)
  }

  emit_call <- function(node, ctx) {
    # 0-argument `super()` inside a class method -> `pyrt::py_super(Cls, self)`.
    if (is_py_ast(node$func, "Name") && identical(node$func$id, "super") &&
        length(node$args) == 0L && length(node$keywords) == 0L &&
        !is.null(ctx$current_class) && !is.null(ctx$self_param)) {
      return(sprintf("pyrt::py_super(%s, %s)", r_ident(ctx$current_class), r_ident(ctx$self_param)))
    }

    fn_code <- emit_expr(node$func, ctx)
    has_star <- any(vapply(node$args, function(a) is_py_ast(a, "Starred"), logical(1)))
    has_kwstar <- any(vapply(node$keywords, function(k) is.null(k$arg), logical(1)))

    if (!has_star && !has_kwstar) {
      pos_str <- vapply(node$args, emit_expr, character(1), ctx = ctx)
      kw_str <- vapply(node$keywords, function(kw) {
        paste0(r_ident(kw$arg), " = ", emit_expr(kw$value, ctx))
      }, character(1))
      all_str <- c(pos_str, kw_str)
      return(sprintf("%s(%s)", fn_code, paste(all_str, collapse = ", ")))
    }

    pos_chunks <- vapply(node$args, function(a) {
      if (is_py_ast(a, "Starred")) {
        sprintf("pyrt::py_collect(%s)", emit_expr(a$value, ctx))
      } else {
        sprintf("list(%s)", emit_expr(a, ctx))
      }
    }, character(1))
    pos_code <- if (length(pos_chunks) == 0L) "list()" else sprintf("c(%s)", paste(pos_chunks, collapse = ", "))

    kw_chunks <- vapply(node$keywords, function(kw) {
      if (is.null(kw$arg)) {
        d_expr <- emit_expr(kw$value, ctx)
        sprintf("(function(.d) if (inherits(.d, 'pyrt_dict')) stats::setNames(.d$.vals, vapply(.d$.keys, as.character, character(1))) else as.list(.d))(%s)", d_expr)
      } else {
        sprintf("list(%s = %s)", r_ident(kw$arg), emit_expr(kw$value, ctx))
      }
    }, character(1))
    kw_code <- if (length(kw_chunks) == 0L) "list()" else sprintf("c(%s)", paste(kw_chunks, collapse = ", "))

    sprintf("pyrt::py_call(%s, %s, %s)", fn_code, pos_code, kw_code)
  }

  # ---- Function signature & Lambda lowering ---------------------------------

  build_signature <- function(args_node, ctx) {
    pos_args <- c(args_node$posonlyargs, args_node$args)
    n_pos <- length(pos_args)
    n_def <- length(args_node$defaults)
    def_offset <- n_pos - n_def

    sig_parts <- character()
    for (i in seq_len(n_pos)) {
      p_name <- r_ident(pos_args[[i]]$arg)
      if (i > def_offset) {
        d_expr <- emit_expr(args_node$defaults[[i - def_offset]], ctx)
        sig_parts <- c(sig_parts, paste0(p_name, " = ", d_expr))
      } else {
        sig_parts <- c(sig_parts, p_name)
      }
    }

    has_vararg <- !is.null(args_node$vararg)
    has_kwarg <- !is.null(args_node$kwarg)

    if (has_vararg) {
      sig_parts <- c(sig_parts, "...")
    }

    for (j in seq_along(args_node$kwonlyargs)) {
      kw_name <- r_ident(args_node$kwonlyargs[[j]]$arg)
      kw_def <- args_node$kw_defaults[[j]]
      if (!is.null(kw_def)) {
        sig_parts <- c(sig_parts, paste0(kw_name, " = ", emit_expr(kw_def, ctx)))
      } else {
        sig_parts <- c(sig_parts, kw_name)
      }
    }

    if (!has_vararg && has_kwarg) {
      sig_parts <- c(sig_parts, "...")
    }

    prologue <- character()
    if (has_vararg || has_kwarg) {
      va_sym <- gensym("va")
      prologue <- c(
        prologue,
        sprintf(
          "%s <- pyrt::py_split_varargs(list(...), vararg = %s, kwarg = %s)",
          va_sym,
          if (has_vararg) "TRUE" else "FALSE",
          if (has_kwarg) "TRUE" else "FALSE"
        )
      )
      if (has_vararg) {
        prologue <- c(prologue, sprintf("%s <- %s$args", r_ident(args_node$vararg$arg), va_sym))
      }
      if (has_kwarg) {
        prologue <- c(prologue, sprintf("%s <- %s$kwargs", r_ident(args_node$kwarg$arg), va_sym))
      }
    }

    list(
      sig = paste(sig_parts, collapse = ", "),
      prologue = prologue
    )
  }

  emit_lambda <- function(node, ctx) {
    fn_scope <- analyze_python(node)$scope
    sig_info <- build_signature(node$args, ctx)
    fn_ctx <- make_ctx(fn_scope, parent_ctx = ctx,
                       current_class = ctx$current_class, self_param = ctx$self_param)
    body_code <- emit_expr(node$body, fn_ctx)
    if (length(sig_info$prologue) == 0L) {
      sprintf("function(%s) %s", sig_info$sig, body_code)
    } else {
      sprintf("function(%s) { %s; %s }", sig_info$sig, paste(sig_info$prologue, collapse = "; "), body_code)
    }
  }

  # ---- Comprehensions & Generator Expressions -------------------------------

  emit_comp <- function(node, kind, ctx) {
    out_var <- gensym("comp")
    init_code <- if (kind == "dict") {
      sprintf("%s <- pyrt::py_dict()", out_var)
    } else {
      sprintf("%s <- list()", out_var)
    }
    for (g_idx in rev(seq_along(node$generators))) {
      gen <- node$generators[[g_idx]]
      tgt_names <- collect_target_ids(gen$target)
      ctx$known_vars <- unique(c(ctx$known_vars, tgt_names))
    }
    inner <- if (kind == "dict") {
      sprintf("%s$set(%s, %s)", out_var,
              emit_expr(node$key, ctx),
              emit_expr(node$value, ctx))
    } else {
      sprintf("%s[length(%s) + 1L] <- list(%s)", out_var, out_var,
              emit_expr(node$elt, ctx))
    }

    body <- inner
    for (g_idx in rev(seq_along(node$generators))) {
      gen <- node$generators[[g_idx]]
      for (if_node in rev(gen$ifs)) {
        body <- sprintf("if (pyrt::py_bool(%s)) {\n%s\n}",
                        emit_expr(if_node, ctx),
                        indent_lines(body, 2L))
      }
      it_var <- gensym("it")
      step_var <- gensym("step")
      bind_code <- emit_assign_target(gen$target, paste0(step_var, "$value"), ctx)
      body <- paste(
        sprintf("%s <- pyrt::py_iter(%s)", it_var, emit_expr(gen$iter, ctx)),
        "repeat {",
        sprintf("  %s <- pyrt::py_iter_step(%s)", step_var, it_var),
        sprintf("  if (%s$stopped) break", step_var),
        indent_lines(bind_code, 2L),
        indent_lines(body, 2L),
        "}",
        sep = "\n"
      )
    }

    wrap_ret <- switch(kind,
      list = sprintf("pyrt::py_list(%s)", out_var),
      set = sprintf("pyrt::py_set(%s)", out_var),
      dict = out_var
    )
    sprintf("(function() {\n%s\n%s\n%s\n})()",
            indent_lines(init_code, 2L),
            indent_lines(body, 2L),
            indent_lines(wrap_ret, 2L))
  }

  emit_genexp <- function(node, ctx) {
    inner_stmt <- ast_node("Expr", value = ast_node("Yield", value = node$elt))
    for (g_idx in rev(seq_along(node$generators))) {
      gen <- node$generators[[g_idx]]
      tgt_names <- collect_target_ids(gen$target)
      ctx$known_vars <- unique(c(ctx$known_vars, tgt_names))
      for (if_node in rev(gen$ifs)) {
        inner_stmt <- ast_node("If", test = if_node, body = list(inner_stmt), orelse = list())
      }
      inner_stmt <- ast_node("For", target = gen$target, iter = gen$iter,
                             body = list(inner_stmt), orelse = list())
    }
    cps_ctx <- ctx
    cps_ctx$in_cps <- TRUE
    cps_body <- emit_cps_stmts(list(inner_stmt), "function() pyrt::py_stop_iteration()",
                               NULL, NULL, cps_ctx)
    sprintf("(function() {\n  .py_gen_env <- environment()\n  pyrt::py_generator(function(.py_sent = NULL) {\n%s\n  })\n})()",
            indent_lines(cps_body, 4L))
  }

  collect_target_ids <- function(tgt) {
    if (is.null(tgt) || !is_py_ast(tgt)) return(character())
    switch(tgt$kind,
      Name = tgt$id,
      Tuple = ,
      List = unlist(lapply(tgt$elts, collect_target_ids)),
      Starred = collect_target_ids(tgt$value),
      character()
    )
  }

  # ---- Assignment & Target Lowering -----------------------------------------

  emit_name_store <- function(id, rhs_code, ctx) {
    if (ctx$scope$kind != "module" && id %in% ctx$globals) {
      return(sprintf("assign(%s, %s, envir = .py_mod_env)", deparse_r_str(id), rhs_code))
    }
    if (ctx$scope$kind != "module" && id %in% ctx$nonlocals) {
      return(sprintf("%s <<- %s", r_ident(id), rhs_code))
    }
    if (isTRUE(ctx$in_cps)) {
      return(sprintf("assign(%s, %s, envir = .py_gen_env)", deparse_r_str(id), rhs_code))
    }
    sprintf("%s <- %s", r_ident(id), rhs_code)
  }

  emit_assign_target <- function(target, rhs_code, ctx) {
    switch(target$kind,
      Name = emit_name_store(target$id, rhs_code, ctx),
      Attribute = sprintf(
        "pyrt::py_setattr(%s, %s, %s)",
        emit_expr(target$value, ctx),
        deparse_r_str(target$attr),
        rhs_code
      ),
      Subscript = sprintf(
        "pyrt::py_setitem(%s, %s, %s)",
        emit_expr(target$value, ctx),
        emit_expr(target$slice, ctx),
        rhs_code
      ),
      Tuple = ,
      List = {
        elts <- target$elts
        star_idx <- which(vapply(elts, function(e) is_py_ast(e, "Starred"), logical(1)))
        uvar <- gensym("unp")
        if (length(star_idx) == 0L) {
          lines <- sprintf("%s <- pyrt::py_unpack(%s, %dL)", uvar, rhs_code, length(elts))
          for (i in seq_along(elts)) {
            lines <- c(lines, emit_assign_target(elts[[i]], sprintf("%s[[%dL]]", uvar, i), ctx))
          }
          paste(lines, collapse = "\n")
        } else {
          si <- star_idx[[1L]]
          n_before <- si - 1L
          n_after <- length(elts) - si
          lines <- sprintf("%s <- pyrt::py_unpack(%s, %dL, %dL, TRUE)", uvar, rhs_code, n_before, n_after)
          for (i in seq_along(elts)) {
            tgt_i <- if (i == si) elts[[i]]$value else elts[[i]]
            lines <- c(lines, emit_assign_target(tgt_i, sprintf("%s[[%dL]]", uvar, i), ctx))
          }
          paste(lines, collapse = "\n")
        }
      },
      stop(sprintf("Unsupported assignment target kind: '%s'", target$kind))
    )
  }

  # ---- CPS Generator Compiler (`yield` / `yield from`) ----------------------

  emit_cps_stmts <- function(stmts, k_code, break_k, cont_k, ctx) {
    if (length(stmts) == 0L) {
      return(sprintf("pyrt::py_bounce(%s)", k_code))
    }
    cur_k <- k_code
    for (i in rev(seq_along(stmts))) {
      st <- stmts[[i]]
      if (i == length(stmts)) {
        body_i <- emit_cps_stmt(st, cur_k, break_k, cont_k, ctx)
      } else {
        k_sym <- gensym("k")
        next_body <- cur_k
        body_i <- paste(
          sprintf("%s <- function(.py_sent = NULL) {", k_sym),
          indent_lines(next_body, 2L),
          "}",
          emit_cps_stmt(st, k_sym, break_k, cont_k, ctx),
          sep = "\n"
        )
      }
      cur_k <- body_i
    }
    cur_k
  }

  emit_cps_stmt <- function(stmt, k_code, break_k, cont_k, ctx) {
    if (!ast_has_yield(stmt) && !ast_has_control_jump(stmt)) {
      normal <- emit_stmt(stmt, ctx)
      return(paste(normal, sprintf("pyrt::py_bounce(%s)", k_code), sep = "\n"))
    }

    switch(stmt$kind,
      Expr = {
        v <- stmt$value
        if (is_py_ast(v, "Yield")) {
          val_code <- if (is.null(v$value)) "pyrt::py_none()" else emit_expr(v$value, ctx)
          return(sprintf("pyrt::py_yield(%s, cont = %s)", val_code, k_code))
        }
        if (is_py_ast(v, "YieldFrom")) {
          sub_it <- gensym("yfit")
          step_fn <- gensym("yfstep")
          cell_var <- gensym("yfcell")
          return(paste(
            sprintf("%s <- pyrt::py_iter(%s)", sub_it, emit_expr(v$value, ctx)),
            sprintf("%s <- function(.py_sent = NULL) {", step_fn),
            sprintf("  %s <- pyrt::py_iter_step(%s)", cell_var, sub_it),
            sprintf("  if (%s$stopped) return(pyrt::py_bounce(%s))", cell_var, k_code),
            sprintf("  pyrt::py_yield(%s$value, cont = %s)", cell_var, step_fn),
            "}",
            sprintf("pyrt::py_bounce(%s)", step_fn),
            sep = "\n"
          ))
        }
        paste(emit_expr(v, ctx), sprintf("pyrt::py_bounce(%s)", k_code), sep = "\n")
      },
      Assign = {
        if (length(stmt$targets) == 1L && is_py_ast(stmt$value, "Yield")) {
          val_code <- if (is.null(stmt$value$value)) "pyrt::py_none()" else emit_expr(stmt$value$value, ctx)
          recv_fn <- gensym("krecv")
          bind_code <- emit_assign_target(stmt$targets[[1L]], ".py_sent", ctx)
          return(paste(
            sprintf("%s <- function(.py_sent = pyrt::py_none()) {", recv_fn),
            indent_lines(bind_code, 2L),
            sprintf("  pyrt::py_bounce(%s)", k_code),
            "}",
            sprintf("pyrt::py_yield(%s, cont = %s)", val_code, recv_fn),
            sep = "\n"
          ))
        }
        paste(emit_stmt(stmt, ctx), sprintf("pyrt::py_bounce(%s)", k_code), sep = "\n")
      },
      Return = {
        val_code <- if (is.null(stmt$value)) "pyrt::py_none()" else emit_expr(stmt$value, ctx)
        sprintf("pyrt::py_stop_iteration(%s)", val_code)
      },
      Break = {
        sprintf("pyrt::py_bounce(%s)", break_k)
      },
      Continue = {
        sprintf("pyrt::py_bounce(%s)", cont_k)
      },
      If = {
        cond_code <- emit_expr(stmt$test, ctx)
        then_code <- emit_cps_stmts(stmt$body, k_code, break_k, cont_k, ctx)
        else_code <- emit_cps_stmts(stmt$orelse, k_code, break_k, cont_k, ctx)
        paste(
          sprintf("if (pyrt::py_bool(%s)) {", cond_code),
          indent_lines(then_code, 2L),
          "} else {",
          indent_lines(else_code, 2L),
          "}",
          sep = "\n"
        )
      },
      While = {
        loop_fn <- gensym("wloop")
        after_orelse <- if (length(stmt$orelse) == 0L) {
          k_code
        } else {
          or_body <- emit_cps_stmts(stmt$orelse, k_code, break_k, cont_k, ctx)
          paste0("function() {\n", indent_lines(or_body, 2L), "\n}")
        }
        body_cps <- emit_cps_stmts(stmt$body, loop_fn, k_code, loop_fn, ctx)
        paste(
          sprintf("%s <- function(.py_sent = NULL) {", loop_fn),
          sprintf("  if (pyrt::py_bool(%s)) {", emit_expr(stmt$test, ctx)),
          indent_lines(body_cps, 4L),
          "  } else {",
          sprintf("    pyrt::py_bounce(%s)", after_orelse),
          "  }",
          "}",
          sprintf("pyrt::py_bounce(%s)", loop_fn),
          sep = "\n"
        )
      },
      For = ,
      AsyncFor = {
        it_var <- gensym("forit")
        loop_fn <- gensym("forloop")
        step_var <- gensym("forstep")
        after_orelse <- if (length(stmt$orelse) == 0L) {
          k_code
        } else {
          or_body <- emit_cps_stmts(stmt$orelse, k_code, break_k, cont_k, ctx)
          paste0("function() {\n", indent_lines(or_body, 2L), "\n}")
        }
        bind_code <- emit_assign_target(stmt$target, paste0(step_var, "$value"), ctx)
        body_cps <- emit_cps_stmts(stmt$body, loop_fn, k_code, loop_fn, ctx)
        paste(
          sprintf("assign(%s, pyrt::py_iter(%s), envir = .py_gen_env)", deparse_r_str(it_var), emit_expr(stmt$iter, ctx)),
          sprintf("%s <- function(.py_sent = NULL) {", loop_fn),
          sprintf("  %s <- pyrt::py_iter_step(%s)", step_var, it_var),
          sprintf("  if (%s$stopped) return(pyrt::py_bounce(%s))", step_var, after_orelse),
          indent_lines(bind_code, 2L),
          indent_lines(body_cps, 2L),
          "}",
          sprintf("pyrt::py_bounce(%s)", loop_fn),
          sep = "\n"
        )
      },
      Try = ,
      TryStar = {
        body_cps <- emit_cps_stmts(stmt$body, k_code, break_k, cont_k, ctx)
        handler_entries <- character()
        for (h in stmt$handlers) {
          types <- extract_handler_types(h$type)
          p_var <- if (!is.null(h$name)) r_ident(h$name) else ".py_exc"
          h_body <- emit_cps_stmts(h$body, k_code, break_k, cont_k, ctx)
          h_fn <- paste(
            sprintf("function(%s) {", p_var),
            indent_lines(h_body, 2L),
            "}",
            sep = "\n"
          )
          for (tp in types) {
            handler_entries <- c(handler_entries, sprintf("%s = %s", r_ident(tp), h_fn))
          }
        }
        handlers_code <- if (length(handler_entries) == 0L) {
          "list()"
        } else {
          paste("list(", indent_lines(paste(handler_entries, collapse = ",\n"), 2L), ")", sep = "\n")
        }
        final_code <- if (length(stmt$finalbody) == 0L) {
          "NULL"
        } else {
          paste("function() {", indent_lines(emit_stmts(stmt$finalbody, ctx), 2L), "}", sep = "\n")
        }
        paste(
          "pyrt::py_try(",
          indent_lines(paste("(function() {\n", indent_lines(body_cps, 2L), "\n})()", sep = ""), 2L), ",",
          indent_lines(sprintf("handlers = %s,", handlers_code), 2L),
          indent_lines(sprintf("finalbody = %s", final_code), 2L),
          ")",
          sep = "\n"
        )
      },
      paste(emit_stmt(stmt, ctx), sprintf("pyrt::py_bounce(%s)", k_code), sep = "\n")
    )
  }

  ast_has_control_jump <- function(node) {
    if (is.null(node)) return(FALSE)
    if (is.list(node) && !is_py_ast(node)) {
      return(any(vapply(node, ast_has_control_jump, logical(1))))
    }
    if (!is_py_ast(node)) return(FALSE)
    if (node$kind %in% c("Return", "Break", "Continue")) return(TRUE)
    if (node$kind %in% c("FunctionDef", "AsyncFunctionDef", "ClassDef", "Lambda")) return(FALSE)
    for (fn in setdiff(names(node), "kind")) {
      if (ast_has_control_jump(node[[fn]])) return(TRUE)
    }
    FALSE
  }

  # ---- Statement Lowering ---------------------------------------------------

  emit_stmts <- function(stmts, ctx) {
    if (length(stmts) == 0L) return("invisible(NULL)")
    lines <- vapply(stmts, emit_stmt, character(1), ctx = ctx)
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0L) "invisible(NULL)" else paste(lines, collapse = "\n")
  }

  emit_stmt <- function(node, ctx) {
    switch(node$kind,
      Expr = {
        emit_expr(node$value, ctx)
      },
      Assign = {
        rhs <- emit_expr(node$value, ctx)
        if (length(node$targets) == 1L) {
          emit_assign_target(node$targets[[1L]], rhs, ctx)
        } else {
          tmp <- gensym("rhs")
          assigns <- vapply(node$targets, function(t) {
            emit_assign_target(t, tmp, ctx)
          }, character(1))
          paste(c(sprintf("%s <- %s", tmp, rhs), assigns), collapse = "\n")
        }
      },
      AnnAssign = {
        if (is.null(node$value)) {
          ""
        } else {
          emit_assign_target(node$target, emit_expr(node$value, ctx), ctx)
        }
      },
      AugAssign = emit_aug_assign(node, ctx),
      Delete = {
        ops <- vapply(node$targets, function(t) {
          switch(t$kind,
            Subscript = sprintf("pyrt::py_delitem(%s, %s)",
                                emit_expr(t$value, ctx),
                                emit_expr(t$slice, ctx)),
            Attribute = sprintf("pyrt::py_delattr(%s, %s)",
                                emit_expr(t$value, ctx),
                                deparse_r_str(t$attr)),
            Name = sprintf("%s <- NULL", r_ident(t$id)),
            "invisible(NULL)"
          )
        }, character(1))
        paste(ops, collapse = "\n")
      },
      Pass = "invisible(NULL)",
      Global = "",
      Nonlocal = "",
      TypeAlias = "",
      Return = {
        val <- if (is.null(node$value)) "pyrt::py_none()" else emit_expr(node$value, ctx)
        if (ctx$in_try_or_with) {
          sprintf("return(pyrt::py_return_signal(%s))", val)
        } else {
          sprintf("return(%s)", val)
        }
      },
      Break = {
        if (ctx$in_try_or_with) {
          "return(pyrt::py_break_signal())"
        } else if (!is.null(ctx$loop_flag)) {
          sprintf("{ %s <- TRUE; break }", ctx$loop_flag)
        } else {
          "break"
        }
      },
      Continue = {
        if (ctx$in_try_or_with) {
          "return(pyrt::py_continue_signal())"
        } else {
          "next"
        }
      },
      If = emit_if(node, ctx),
      While = emit_while(node, ctx),
      For = ,
      AsyncFor = emit_for(node, ctx),
      FunctionDef = ,
      AsyncFunctionDef = emit_function_def(node, ctx),
      ClassDef = emit_class_def(node, ctx),
      Raise = emit_raise(node, ctx),
      Assert = {
        test_code <- emit_expr(node$test, ctx)
        msg_code <- if (is.null(node$msg)) "NULL" else emit_expr(node$msg, ctx)
        sprintf("pyrt::py_assert(%s, %s)", test_code, msg_code)
      },
      Try = ,
      TryStar = emit_try(node, ctx),
      With = ,
      AsyncWith = emit_with(node, ctx),
      Match = emit_match(node, ctx),
      Import = emit_import(node, ctx),
      ImportFrom = emit_import_from(node, ctx),
      stop(sprintf("Unsupported statement node kind: '%s'", node$kind))
    )
  }

  emit_aug_assign <- function(node, ctx) {
    op_Node <- ast_node("BinOp", left = node$target, op = node$op, right = node$value)
    rhs <- emit_binop(op_Node, ctx)
    emit_assign_target(node$target, rhs, ctx)
  }

  emit_if <- function(node, ctx) {
    cond <- emit_expr(node$test, ctx)
    body <- emit_stmts(node$body, ctx)
    if (length(node$orelse) == 0L) {
      paste(
        sprintf("if (pyrt::py_bool(%s)) {", cond),
        indent_lines(body, 2L),
        "}",
        sep = "\n"
      )
    } else if (length(node$orelse) == 1L && is_py_ast(node$orelse[[1L]], "If")) {
      paste(
        sprintf("if (pyrt::py_bool(%s)) {", cond),
        indent_lines(body, 2L),
        paste0("} else ", emit_if(node$orelse[[1L]], ctx)),
        sep = "\n"
      )
    } else {
      orelse <- emit_stmts(node$orelse, ctx)
      paste(
        sprintf("if (pyrt::py_bool(%s)) {", cond),
        indent_lines(body, 2L),
        "} else {",
        indent_lines(orelse, 2L),
        "}",
        sep = "\n"
      )
    }
  }

  emit_while <- function(node, ctx) {
    has_orelse <- length(node$orelse) > 0L
    flag <- if (has_orelse) gensym("broke") else NULL
    loop_ctx <- ctx
    loop_ctx$in_loop <- TRUE
    loop_ctx$loop_flag <- flag
    body <- emit_stmts(node$body, loop_ctx)
    loop_code <- paste(
      sprintf("while (pyrt::py_bool(%s)) {", emit_expr(node$test, ctx)),
      indent_lines(body, 2L),
      "}",
      sep = "\n"
    )
    if (!has_orelse) return(loop_code)
    orelse_code <- emit_stmts(node$orelse, ctx)
    paste(
      sprintf("%s <- FALSE", flag),
      loop_code,
      sprintf("if (!%s) {", flag),
      indent_lines(orelse_code, 2L),
      "}",
      sep = "\n"
    )
  }

  emit_for <- function(node, ctx) {
    has_orelse <- length(node$orelse) > 0L
    flag <- if (has_orelse) gensym("broke") else NULL
    loop_ctx <- ctx
    loop_ctx$in_loop <- TRUE
    loop_ctx$loop_flag <- flag

    # Fast-path optimization (Phase 8): lower `for <id> in range(...)` without
    # `else` clause directly to a native R `for` loop over `pyrt::py_range_seq(...)`
    # when `<id>` is a local variable.
    if (!has_orelse &&
        is_py_ast(node$target, "Name") &&
        !(node$target$id %in% ctx$globals) &&
        !(node$target$id %in% ctx$nonlocals) &&
        is_py_ast(node$iter, "Call") &&
        is_py_ast(node$iter$func, "Name") &&
        identical(node$iter$func$id, "range") &&
        !("range" %in% ctx$known_vars) &&
        length(node$iter$keywords) == 0L &&
        length(node$iter$args) %in% 1:3 &&
        all(vapply(node$iter$args, function(a) !is_py_ast(a, "Starred"), logical(1)))) {
      r_args <- paste(vapply(node$iter$args, emit_expr, character(1), ctx = ctx), collapse = ", ")
      body_code <- emit_stmts(node$body, loop_ctx)
      return(paste(
        sprintf("for (%s in pyrt::py_range_seq(%s)) {", r_ident(node$target$id), r_args),
        indent_lines(body_code, 2L),
        "}",
        sep = "\n"
      ))
    }

    it_var <- gensym("it")
    step_var <- gensym("step")
    bind_code <- emit_assign_target(node$target, paste0(step_var, "$value"), loop_ctx)
    body_code <- emit_stmts(node$body, loop_ctx)
    loop_code <- paste(
      sprintf("%s <- pyrt::py_iter(%s)", it_var, emit_expr(node$iter, ctx)),
      "repeat {",
      sprintf("  %s <- pyrt::py_iter_step(%s)", step_var, it_var),
      sprintf("  if (%s$stopped) break", step_var),
      indent_lines(bind_code, 2L),
      indent_lines(body_code, 2L),
      "}",
      sep = "\n"
    )
    if (!has_orelse) return(loop_code)
    orelse_code <- emit_stmts(node$orelse, ctx)
    paste(
      sprintf("%s <- FALSE", flag),
      loop_code,
      sprintf("if (!%s) {", flag),
      indent_lines(orelse_code, 2L),
      "}",
      sep = "\n"
    )
  }

  emit_function_value <- function(node, ctx, is_method = FALSE) {
    fn_scope <- analyze_python(node)$scope
    esc_info <- analyze_escape(node)
    sig_info <- build_signature(node$args, ctx)
    all_pos <- c(node$args$posonlyargs, node$args$args)
    self_p <- if (is_method && length(all_pos) >= 1L) all_pos[[1L]]$arg else ctx$self_param
    fn_ctx <- make_ctx(
      fn_scope,
      parent_ctx = ctx,
      in_cps = isTRUE(fn_scope$is_generator),
      current_class = ctx$current_class,
      self_param = self_p
    )
    fn_ctx$escape_info <- esc_info

    body_stmts <- node$body
    if (length(body_stmts) >= 1L &&
        is_py_ast(body_stmts[[1L]], "Expr") &&
        is_py_ast(body_stmts[[1L]]$value, "Constant") &&
        identical(body_stmts[[1L]]$value$ctype, "str")) {
      body_stmts <- body_stmts[-1L]
    }

    if (isTRUE(fn_scope$is_generator)) {
      cps_code <- emit_cps_stmts(
        body_stmts,
        "function() pyrt::py_stop_iteration()",
        NULL, NULL, fn_ctx
      )
      lines <- c(
        sig_info$prologue,
        ".py_gen_env <- environment()",
        sprintf("pyrt::py_generator(function(.py_sent = NULL) {\n%s\n})", indent_lines(cps_code, 2L))
      )
    } else {
      body_code <- emit_stmts(body_stmts, fn_ctx)
      last_is_return <- length(body_stmts) > 0L &&
        body_stmts[[length(body_stmts)]]$kind %in% c("Return", "Raise")
      lines <- c(
        sig_info$prologue,
        body_code,
        if (!last_is_return) "pyrt::py_none()" else character()
      )
    }

    paste(
      sprintf("function(%s) {", sig_info$sig),
      indent_lines(paste(lines[nzchar(lines)], collapse = "\n"), 2L),
      "}",
      sep = "\n"
    )
  }

  emit_function_def <- function(node, ctx) {
    fn_code <- emit_function_value(node, ctx, is_method = FALSE)
    for (dec in rev(node$decorator_list)) {
      dec_code <- emit_expr(dec, ctx)
      fn_code <- sprintf("%s(%s)", dec_code, fn_code)
    }
    emit_name_store(node$name, fn_code, ctx)
  }

  emit_class_def <- function(node, ctx) {
    cls_scope <- analyze_python(node)$scope
    cls_ctx <- make_ctx(cls_scope, parent_ctx = ctx, current_class = node$name)

    bases_code <- vapply(node$bases, emit_expr, character(1), ctx = ctx)
    bases_str <- if (length(bases_code) == 0L) "list()" else sprintf("list(%s)", paste(bases_code, collapse = ", "))

    dict_var <- gensym("cdict")
    body_lines <- sprintf("%s <- list()", dict_var)

    for (stmt in node$body) {
      if (stmt$kind %in% c("FunctionDef", "AsyncFunctionDef")) {
        m_name <- stmt$name
        decs <- stmt$decorator_list
        prop_mod <- NULL
        if (length(decs) == 1L && is_py_ast(decs[[1L]], "Attribute") &&
            is_py_ast(decs[[1L]]$value, "Name") &&
            decs[[1L]]$attr %in% c("setter", "deleter")) {
          prop_mod <- list(prop = decs[[1L]]$value$id, action = decs[[1L]]$attr)
        }
        fn_val <- emit_function_value(stmt, cls_ctx, is_method = TRUE)
        if (!is.null(prop_mod)) {
          body_lines <- c(
            body_lines,
            sprintf("%s[[%s]] <- %s[[%s]]$%s(%s)",
                    dict_var, deparse_r_str(prop_mod$prop),
                    dict_var, deparse_r_str(prop_mod$prop),
                    prop_mod$action, fn_val)
          )
        } else {
          for (dec in rev(decs)) {
            if (is_py_ast(dec, "Name") && dec$id %in% c("property", "staticmethod", "classmethod")) {
              fn_val <- sprintf("pyrt::py_%s(%s)", dec$id, fn_val)
            } else {
              fn_val <- sprintf("%s(%s)", emit_expr(dec, cls_ctx), fn_val)
            }
          }
          body_lines <- c(body_lines, sprintf("%s[[%s]] <- %s", dict_var, deparse_r_str(m_name), fn_val))
        }
      } else if (stmt$kind == "Assign") {
        rhs <- emit_expr(stmt$value, cls_ctx)
        for (t in stmt$targets) {
          if (is_py_ast(t, "Name")) {
            body_lines <- c(
              body_lines,
              sprintf("%s <- %s", r_ident(t$id), rhs),
              sprintf("%s[[%s]] <- %s", dict_var, deparse_r_str(t$id), r_ident(t$id))
            )
          }
        }
      } else if (stmt$kind == "AnnAssign") {
        if (!is.null(stmt$value) && is_py_ast(stmt$target, "Name")) {
          rhs <- emit_expr(stmt$value, cls_ctx)
          t_id <- stmt$target$id
          body_lines <- c(
            body_lines,
            sprintf("%s <- %s", r_ident(t_id), rhs),
            sprintf("%s[[%s]] <- %s", dict_var, deparse_r_str(t_id), r_ident(t_id))
          )
        } else if (is.null(stmt$value) && is_py_ast(stmt$target, "Name")) {
          t_id <- stmt$target$id
          body_lines <- c(
            body_lines,
            sprintf("%s[[%s]] <- pyrt::py_none()", dict_var, deparse_r_str(t_id))
          )
        }
      } else if (stmt$kind == "Pass" ||
                 (stmt$kind == "Expr" && is_py_ast(stmt$value, "Constant") && identical(stmt$value$ctype, "str"))) {
        # Ignore `pass` and class docstring
      } else {
        body_lines <- c(body_lines, emit_stmt(stmt, cls_ctx))
      }
    }

    body_lines <- c(body_lines, dict_var)
    dict_iife <- paste(
      "(function() {",
      indent_lines(paste(body_lines, collapse = "\n"), 2L),
      "})()",
      sep = "\n"
    )

    cls_expr <- sprintf("pyrt::py_class(%s, bases = %s, dict = %s)",
                        deparse_r_str(node$name), bases_str, dict_iife)
    for (dec in rev(node$decorator_list)) {
      cls_expr <- sprintf("%s(%s)", emit_expr(dec, ctx), cls_expr)
    }
    emit_name_store(node$name, cls_expr, ctx)
  }

  # ---- Exceptions (`raise`, `try / except / else / finally`, `with`) --------

  emit_raise <- function(node, ctx) {
    if (is.null(node$exc)) {
      return("pyrt::py_raise()")
    }
    exc_Node <- node$exc
    cause_code <- if (is.null(node$cause)) "NULL" else emit_expr(node$cause, ctx)
    if (is_py_ast(exc_Node, "Call") && is_py_ast(exc_Node$func, "Name") &&
        !(exc_Node$func$id %in% ctx$known_vars)) {
      msg_code <- if (length(exc_Node$args) >= 1L) emit_expr(exc_Node$args[[1L]], ctx) else '""'
      return(sprintf("pyrt::py_raise(%s, %s, cause = %s)",
                     deparse_r_str(exc_Node$func$id), msg_code, cause_code))
    }
    if (is_py_ast(exc_Node, "Name") && !(exc_Node$id %in% ctx$known_vars)) {
      return(sprintf("pyrt::py_raise(%s, cause = %s)", deparse_r_str(exc_Node$id), cause_code))
    }
    sprintf("pyrt::py_raise(%s, cause = %s)", emit_expr(exc_Node, ctx), cause_code)
  }

  emit_try <- function(node, ctx) {
    try_ctx <- ctx
    try_ctx$in_try_or_with <- TRUE

    body_code <- emit_stmts(node$body, try_ctx)
    handler_entries <- character()
    for (h in node$handlers) {
      types <- extract_handler_types(h$type)
      p_var <- if (!is.null(h$name)) r_ident(h$name) else ".py_exc"
      h_body <- emit_stmts(h$body, try_ctx)
      h_fn <- paste(
        sprintf("function(%s) {", p_var),
        indent_lines(h_body, 2L),
        "}",
        sep = "\n"
      )
      for (tp in types) {
        handler_entries <- c(handler_entries, sprintf("%s = %s", r_ident(tp), h_fn))
      }
    }
    handlers_code <- if (length(handler_entries) == 0L) {
      "list()"
    } else {
      paste("list(", indent_lines(paste(handler_entries, collapse = ",\n"), 2L), ")", sep = "\n")
    }

    orelse_code <- if (length(node$orelse) == 0L) {
      "NULL"
    } else {
      paste("function() {", indent_lines(emit_stmts(node$orelse, try_ctx), 2L), "}", sep = "\n")
    }

    final_code <- if (length(node$finalbody) == 0L) {
      "NULL"
    } else {
      paste("function() {", indent_lines(emit_stmts(node$finalbody, try_ctx), 2L), "}", sep = "\n")
    }

    call_code <- paste(
      "pyrt::py_try(",
      indent_lines(paste("(function() {\n", indent_lines(body_code, 2L), "\n})()", sep = ""), 2L), ",",
      indent_lines(sprintf("handlers = %s,", handlers_code), 2L),
      indent_lines(sprintf("orelse = %s,", orelse_code), 2L),
      indent_lines(sprintf("finalbody = %s", final_code), 2L),
      ")",
      sep = "\n"
    )
    wrap_signal_propagation(call_code, ctx)
  }

  extract_handler_types <- function(type_node) {
    if (is.null(type_node)) return("BaseException")
    if (is_py_ast(type_node, "Name")) return(type_node$id)
    if (is_py_ast(type_node, "Attribute")) return(type_node$attr)
    if (is_py_ast(type_node, "Tuple")) {
      return(unlist(lapply(type_node$elts, extract_handler_types)))
    }
    "Exception"
  }

  emit_with <- function(node, ctx) {
    with_ctx <- ctx
    with_ctx$in_try_or_with <- TRUE
    inner <- emit_stmts(node$body, with_ctx)
    for (item in rev(node$items)) {
      mgr_code <- emit_expr(item$context_expr, ctx)
      if (is.null(item$optional_vars)) {
        fn_code <- paste("function() {", indent_lines(inner, 2L), "}", sep = "\n")
      } else if (is_py_ast(item$optional_vars, "Name")) {
        fn_code <- paste(
          sprintf("function(%s) {", r_ident(item$optional_vars$id)),
          indent_lines(inner, 2L),
          "}",
          sep = "\n"
        )
      } else {
        v_sym <- gensym("withv")
        bind_code <- emit_assign_target(item$optional_vars, v_sym, with_ctx)
        fn_code <- paste(
          sprintf("function(%s) {", v_sym),
          indent_lines(bind_code, 2L),
          indent_lines(inner, 2L),
          "}",
          sep = "\n"
        )
      }
      inner <- sprintf("pyrt::py_with(%s, %s)", mgr_code, fn_code)
    }
    wrap_signal_propagation(inner, ctx)
  }

  wrap_signal_propagation <- function(call_code, ctx) {
    if (ctx$scope$kind == "module" && !ctx$in_loop) return(call_code)
    res_var <- gensym("sig")
    checks <- sprintf("%s <- %s", res_var, call_code)
    if (ctx$in_try_or_with) {
      checks <- c(
        checks,
        sprintf("if (pyrt::is_py_return(%s) || pyrt::is_py_break(%s) || pyrt::is_py_continue(%s)) return(%s)",
                res_var, res_var, res_var, res_var)
      )
      return(paste(checks, collapse = "\n"))
    }
    if (ctx$scope$kind == "function") {
      checks <- c(checks, sprintf("if (pyrt::is_py_return(%s)) return(%s$value)", res_var, res_var))
    }
    if (ctx$in_loop) {
      if (!is.null(ctx$loop_flag)) {
        checks <- c(checks, sprintf("if (pyrt::is_py_break(%s)) { %s <- TRUE; break }", res_var, ctx$loop_flag))
      } else {
        checks <- c(checks, sprintf("if (pyrt::is_py_break(%s)) break", res_var))
      }
      checks <- c(checks, sprintf("if (pyrt::is_py_continue(%s)) next", res_var))
    }
    paste(checks, collapse = "\n")
  }

  # ---- Structural Pattern Matching (`match / case`) -------------------------

  emit_match <- function(node, ctx) {
    subj_var <- gensym("subj")
    done_var <- gensym("matched")
    lines <- c(
      sprintf("%s <- %s", subj_var, emit_expr(node$subject, ctx)),
      sprintf("%s <- FALSE", done_var)
    )
    for (mc in node$cases) {
      pat <- emit_pattern(mc$pattern, subj_var, ctx)
      guard_cond <- if (is.null(mc$guard)) "TRUE" else sprintf("pyrt::py_bool(%s)", emit_expr(mc$guard, ctx))
      case_body <- emit_stmts(mc$body, ctx)
      block <- paste(
        sprintf("if (!%s && (%s)) {", done_var, pat$cond),
        if (nzchar(pat$binds)) indent_lines(pat$binds, 2L) else NULL,
        sprintf("  if (%s) {", guard_cond),
        sprintf("    %s <- TRUE", done_var),
        indent_lines(case_body, 4L),
        "  }",
        "}",
        sep = "\n"
      )
      lines <- c(lines, block)
    }
    paste(lines, collapse = "\n")
  }

  emit_pattern <- function(pat, val_expr, ctx) {
    switch(pat$kind,
      MatchValue = list(
        cond = sprintf("pyrt::py_eq(%s, %s)", val_expr, emit_expr(pat$value, ctx)),
        binds = ""
      ),
      MatchSingleton = {
        cond <- if (is.null(pat$value)) {
          sprintf("pyrt::is_py_none(%s)", val_expr)
        } else {
          sprintf("pyrt::py_is(%s, %s)", val_expr, if (isTRUE(pat$value)) "TRUE" else "FALSE")
        }
        list(cond = cond, binds = "")
      },
      MatchAs = {
        if (is.null(pat$pattern)) {
          bind <- if (is.null(pat$name)) "" else emit_name_store(pat$name, val_expr, ctx)
          list(cond = "TRUE", binds = bind)
        } else {
          sub <- emit_pattern(pat$pattern, val_expr, ctx)
          bind <- if (is.null(pat$name)) "" else emit_name_store(pat$name, val_expr, ctx)
          list(
            cond = sub$cond,
            binds = paste(c(sub$binds[nzchar(sub$binds)], bind[nzchar(bind)]), collapse = "\n")
          )
        }
      },
      MatchOr = {
        subs <- lapply(pat$patterns, emit_pattern, val_expr = val_expr, ctx = ctx)
        conds <- vapply(subs, `[[`, character(1), "cond")
        all_binds <- unlist(lapply(subs, function(s) s$binds[nzchar(s$binds)]))
        list(
          cond = paste0("(", paste(conds, collapse = " || "), ")"),
          binds = paste(all_binds, collapse = "\n")
        )
      },
      MatchSequence = {
        pats <- pat$patterns
        n <- length(pats)
        conds <- sprintf("(inherits(%s, 'pyrt_list') || inherits(%s, 'pyrt_tuple') || is.list(%s)) && pyrt::py_len(%s) == %dL",
                         val_expr, val_expr, val_expr, val_expr, n)
        binds <- character()
        for (i in seq_len(n)) {
          item_expr <- sprintf("pyrt::py_getitem(%s, %dL)", val_expr, i - 1L)
          sub <- emit_pattern(pats[[i]], item_expr, ctx)
          conds <- c(conds, sub$cond)
          if (nzchar(sub$binds)) binds <- c(binds, sub$binds)
        }
        list(
          cond = paste(conds, collapse = " && "),
          binds = paste(binds, collapse = "\n")
        )
      },
      MatchMapping = {
        n <- length(pat$keys)
        conds <- sprintf("inherits(%s, 'pyrt_dict')", val_expr)
        binds <- character()
        for (i in seq_len(n)) {
          k_code <- emit_expr(pat$keys[[i]], ctx)
          conds <- c(conds, sprintf("pyrt::py_in(%s, %s)", k_code, val_expr))
          sub <- emit_pattern(pat$patterns[[i]], sprintf("pyrt::py_getitem(%s, %s)", val_expr, k_code), ctx)
          conds <- c(conds, sub$cond)
          if (nzchar(sub$binds)) binds <- c(binds, sub$binds)
        }
        list(
          cond = paste(conds, collapse = " && "),
          binds = paste(binds, collapse = "\n")
        )
      },
      MatchClass = {
        cls_code <- emit_expr(pat$cls, ctx)
        conds <- sprintf("pyrt::py_isinstance(%s, %s)", val_expr, cls_code)
        binds <- character()
        for (i in seq_along(pat$kwd_attrs)) {
          attr_name <- pat$kwd_attrs[[i]]
          conds <- c(conds, sprintf("pyrt::py_hasattr(%s, %s)", val_expr, deparse_r_str(attr_name)))
          sub <- emit_pattern(
            pat$kwd_patterns[[i]],
            sprintf("pyrt::py_getattr(%s, %s)", val_expr, deparse_r_str(attr_name)),
            ctx
          )
          conds <- c(conds, sub$cond)
          if (nzchar(sub$binds)) binds <- c(binds, sub$binds)
        }
        list(
          cond = paste(conds, collapse = " && "),
          binds = paste(binds, collapse = "\n")
        )
      },
      list(cond = "TRUE", binds = "")
    )
  }

  # ---- Imports (`import`, `from ... import ...`) ----------------------------

  emit_import <- function(node, ctx) {
    lines <- character()
    for (al in node$names) {
      mod_name <- al$name
      bind_name <- if (!is.null(al$asname)) al$asname else strsplit(mod_name, ".", fixed = TRUE)[[1L]][[1L]]
      rhs <- resolve_module_expr(mod_name)
      lines <- c(lines, emit_name_store(bind_name, rhs, ctx))
    }
    paste(lines, collapse = "\n")
  }

  emit_import_from <- function(node, ctx) {
    mod_name <- if (is.null(node$module)) "" else node$module
    if (identical(mod_name, "__future__")) return("invisible(NULL)")
    mod_expr <- resolve_module_expr(mod_name)
    lines <- character()
    for (al in node$names) {
      if (identical(al$name, "*")) next
      bind_name <- if (!is.null(al$asname)) al$asname else al$name
      rhs <- sprintf("pyrt::py_getattr(%s, %s)", mod_expr, deparse_r_str(al$name))
      lines <- c(lines, emit_name_store(bind_name, rhs, ctx))
    }
    paste(lines, collapse = "\n")
  }

  resolve_module_expr <- function(mod_name) {
    if (identical(mod_name, "random")) {
      return("pyrt::py_random")
    }
    top_mod <- strsplit(mod_name, ".", fixed = TRUE)[[1L]][[1L]]
    if (top_mod %in% pyrt::py_stdlib_names()) {
      return(sprintf("pyrt::py_stdlib(%s)", deparse_r_str(mod_name)))
    }
    sprintf("get0(%s, envir = .py_mod_env, inherits = TRUE, ifnotfound = pyrt::py_stdlib(%s))",
            deparse_r_str(mod_name), deparse_r_str(mod_name))
  }

  # ---- Top-level Module Lowering --------------------------------------------

  root_ctx <- make_ctx(analysis$scope)

  header <- c(
    if (!is.null(module_name)) sprintf("# Transpiled from Python module: %s", module_name) else "# Transpiled by pyport",
    "if (!exists(\".py_mod_env\", inherits = FALSE)) .py_mod_env <- environment()"
  )
  body_r <- emit_stmts(tree$body, root_ctx)
  out_code <- paste(c(header, body_r), collapse = "\n")

  structure(
    out_code,
    analysis = analysis,
    diagnostics = analysis$diagnostics,
    class = c("pyport_r_code", "character")
  )
}

#' Transpile and evaluate Python code in an R environment
#'
#' Convenience function that transpiles Python 3 source code via
#' [transpile_python()] and evaluates the generated R code inside an environment
#' `envir`, returning `envir` so defined functions, classes, and variables can
#' be accessed directly with `$` (`mod$func(...)`, `mod$MyClass(...)`).
#'
#' @param source Python source code string or `py_ast_node`.
#' @param envir Optional environment in which to evaluate the transpiled R code.
#'   Defaults to a new environment inheriting from `globalenv()`.
#' @returns The environment `envir` populated with the module's bindings.
#' @export
#' @examples
#' mod <- eval_python("
#' def greet(name='world'):
#'     return f'hello, {name}!'
#' ")
#' mod$greet("R")
eval_python <- function(source, envir = NULL) {
  if (is.null(envir)) {
    envir <- new.env(parent = globalenv())
  }
  r_code <- transpile_python(source)
  envir$.py_mod_env <- envir
  exprs <- parse(text = as.character(r_code), keep.source = FALSE)
  eval(exprs, envir = envir)
  envir
}
