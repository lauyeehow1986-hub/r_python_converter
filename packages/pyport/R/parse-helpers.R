# Helpers for compound statements, definitions, expressions, and pattern
# matching called by `cst_to_ast()` in `R/parse.R`.

parse_expression_statement <- function(node, env) {
  ch <- named_non_extra(node)
  if (length(ch) == 0L) return(NULL)
  if (length(ch) == 1L) {
    tp <- treesitter::node_type(ch[[1L]])
    if (tp %in% c("assignment", "augmented_assignment")) {
      return(cst_to_ast(ch[[1L]], env))
    }
    return(ast_node("Expr", value = cst_to_ast(ch[[1L]], env), .ts_node = node))
  }
  # Top-level comma-separated expressions (`1, 2`) are parsed as multiple
  # children of `expression_statement` by tree-sitter-python.
  elts <- lapply(ch, cst_to_ast, env = env)
  tup <- ast_node("Tuple", elts = elts, ctx = ast_op("Load"), .ts_node = node)
  ast_node("Expr", value = tup, .ts_node = node)
}

parse_assignment <- function(node, env) {
  type_node <- treesitter::node_child_by_field_name(node, "type")
  left_node <- treesitter::node_child_by_field_name(node, "left")
  right_node <- treesitter::node_child_by_field_name(node, "right")

  if (!is.null(type_node)) {
    target <- set_ctx(cst_to_ast(left_node, env), "Store")
    is_paren <- treesitter::node_type(left_node) %in% c("parenthesized_expression", "tuple_pattern")
    simple <- if (identical(target$kind, "Name") && !is_paren) 1L else 0L
    val <- if (is.null(right_node)) NULL else cst_to_ast(right_node, env)
    return(ast_node("AnnAssign", target = target,
                    annotation = cst_to_ast(type_node, env),
                    value = val, simple = simple, .ts_node = node))
  }

  # Chained assignment `a = b = 1` nests `assignment` on the right in
  # tree-sitter, whereas Python's `Assign` flattens `targets = [a, b]`.
  targets <- list(set_ctx(cst_to_ast(left_node, env), "Store"))
  cur <- right_node
  while (!is.null(cur) && identical(treesitter::node_type(cur), "assignment")) {
    targets[[length(targets) + 1L]] <- set_ctx(
      cst_to_ast(treesitter::node_child_by_field_name(cur, "left"), env), "Store"
    )
    cur <- treesitter::node_child_by_field_name(cur, "right")
  }
  ast_node("Assign", targets = targets, value = cst_to_ast(cur, env),
           type_comment = NULL, .ts_node = node)
}

parse_aug_assign <- function(node, env) {
  target <- set_ctx(cst_to_ast(treesitter::node_child_by_field_name(node, "left"), env), "Store")
  op_tok <- treesitter::node_type(treesitter::node_child_by_field_name(node, "operator"))
  value <- cst_to_ast(treesitter::node_child_by_field_name(node, "right"), env)
  ast_node("AugAssign", target = target, op = ast_op(augop_map[[op_tok]]),
           value = value, .ts_node = node)
}

parse_raise <- function(node, env) {
  cause_node <- treesitter::node_child_by_field_name(node, "cause")
  ch <- named_non_extra(node)
  exc_node <- NULL
  if (length(ch) >= 1L) {
    if (is.null(cause_node) || length(ch) >= 2L) {
      exc_node <- ch[[1L]]
    }
  }
  ast_node(
    "Raise",
    exc = if (is.null(exc_node)) NULL else cst_to_ast(exc_node, env),
    cause = if (is.null(cause_node)) NULL else cst_to_ast(cause_node, env),
    .ts_node = node
  )
}

parse_import_alias <- function(node) {
  tp <- treesitter::node_type(node)
  if (tp == "aliased_import") {
    nm <- treesitter::node_text(treesitter::node_child_by_field_name(node, "name"))
    asname <- treesitter::node_text(treesitter::node_child_by_field_name(node, "alias"))
    ast_node("alias", name = nm, asname = asname, .ts_node = node)
  } else if (tp == "wildcard_import") {
    ast_node("alias", name = "*", asname = NULL, .ts_node = node)
  } else {
    ast_node("alias", name = treesitter::node_text(node), asname = NULL, .ts_node = node)
  }
}

parse_import_from <- function(node) {
  mod_node <- treesitter::node_child_by_field_name(node, "module_name")
  level <- 0L
  mod_name <- NULL
  if (!is.null(mod_node)) {
    if (identical(treesitter::node_type(mod_node), "relative_import")) {
      for (ch in named_non_extra(mod_node)) {
        if (identical(treesitter::node_type(ch), "import_prefix")) {
          level <- nchar(treesitter::node_text(ch))
        } else if (identical(treesitter::node_type(ch), "dotted_name")) {
          mod_name <- treesitter::node_text(ch)
        }
      }
    } else {
      mod_name <- treesitter::node_text(mod_node)
    }
  }
  name_nodes <- children_by_field(node, "name")
  if (length(name_nodes) == 0L) {
    # `from m import *` has a `wildcard_import` child without a field name.
    for (ch in named_non_extra(node)) {
      if (identical(treesitter::node_type(ch), "wildcard_import")) {
        name_nodes <- list(ch)
      }
    }
  }
  ast_node("ImportFrom", module = mod_name,
           names = lapply(name_nodes, parse_import_alias),
           level = level, .ts_node = node)
}

parse_if_stmt <- function(node, env) {
  cond <- cst_to_ast(treesitter::node_child_by_field_name(node, "condition"), env)
  body <- convert_body(treesitter::node_child_by_field_name(node, "consequence"), env)
  alts <- children_by_field(node, "alternative")
  orelse <- build_elif_chain(alts, env)
  ast_node("If", test = cond, body = body, orelse = orelse, .ts_node = node)
}

build_elif_chain <- function(alts, env) {
  if (length(alts) == 0L) return(list())
  first <- alts[[1L]]
  tp <- treesitter::node_type(first)
  if (tp == "else_clause") {
    return(convert_body(treesitter::node_child_by_field_name(first, "body"), env))
  }
  # `elif_clause` becomes a single `If` node inside the parent's `orelse`.
  cond <- cst_to_ast(treesitter::node_child_by_field_name(first, "condition"), env)
  body <- convert_body(treesitter::node_child_by_field_name(first, "consequence"), env)
  rest <- build_elif_chain(alts[-1L], env)
  list(ast_node("If", test = cond, body = body, orelse = rest, .ts_node = first))
}

parse_with_stmt <- function(node, env) {
  is_async <- has_unnamed_token(node, "async")
  ch <- named_non_extra(node)
  clause <- ch[[1L]]
  item_nodes <- named_non_extra(clause)
  items <- lapply(item_nodes, function(it) {
    val_node <- treesitter::node_child_by_field_name(it, "value")
    if (identical(treesitter::node_type(val_node), "as_pattern")) {
      sub_ch <- named_non_extra(val_node)
      ctx_expr <- cst_to_ast(sub_ch[[1L]], env)
      alias_node <- treesitter::node_child_by_field_name(val_node, "alias")
      opt_vars <- set_ctx(cst_to_ast(named_non_extra(alias_node)[[1L]], env), "Store")
    } else {
      ctx_expr <- cst_to_ast(val_node, env)
      opt_vars <- NULL
    }
    ast_node("withitem", context_expr = ctx_expr, optional_vars = opt_vars, .ts_node = it)
  })
  body <- convert_body(treesitter::node_child_by_field_name(node, "body"), env)
  kind <- if (is_async) "AsyncWith" else "With"
  ast_node(kind, items = items, body = body, type_comment = NULL, .ts_node = node)
}

parse_try_stmt <- function(node, env) {
  body <- convert_body(treesitter::node_child_by_field_name(node, "body"), env)
  handlers <- list()
  orelse <- list()
  finalbody <- list()
  is_star <- FALSE

  for (ch in named_non_extra(node)) {
    tp <- treesitter::node_type(ch)
    if (tp %in% c("except_clause", "except_group_clause")) {
      h <- parse_except_clause(ch, env)
      if (isTRUE(h$is_star)) is_star <- TRUE
      handlers[[length(handlers) + 1L]] <- h$node
    } else if (tp == "else_clause") {
      orelse <- convert_body(treesitter::node_child_by_field_name(ch, "body"), env)
    } else if (tp == "finally_clause") {
      bnode <- named_non_extra(ch)[[1L]]
      finalbody <- convert_body(bnode, env)
    }
  }
  kind <- if (is_star) "TryStar" else "Try"
  ast_node(kind, body = body, handlers = handlers, orelse = orelse,
           finalbody = finalbody, .ts_node = node)
}

parse_except_clause <- function(node, env) {
  is_star <- identical(treesitter::node_type(node), "except_group_clause")
  ch <- named_non_extra(node)
  val_node <- treesitter::node_child_by_field_name(node, "value")
  # `except_group_clause` does not assign the `value` field name to its first child.
  if (is.null(val_node) && length(ch) >= 2L) {
    val_node <- ch[[1L]]
  }
  exc_type <- NULL
  exc_name <- NULL

  if (!is.null(val_node)) {
    if (identical(treesitter::node_type(val_node), "as_pattern")) {
      first_ch <- named_non_extra(val_node)[[1L]]
      if (identical(treesitter::node_type(first_ch), "list_splat")) {
        is_star <- TRUE
        first_ch <- named_non_extra(first_ch)[[1L]]
      }
      exc_type <- cst_to_ast(first_ch, env)
      alias_node <- treesitter::node_child_by_field_name(val_node, "alias")
      exc_name <- treesitter::node_text(alias_node)
    } else if (identical(treesitter::node_type(val_node), "list_splat")) {
      is_star <- TRUE
      exc_type <- cst_to_ast(named_non_extra(val_node)[[1L]], env)
    } else {
      exc_type <- cst_to_ast(val_node, env)
    }
  }
  # The handler body is the `block` child.
  block_node <- ch[[length(ch)]]
  list(
    is_star = is_star,
    node = ast_node("ExceptHandler", type = exc_type, name = exc_name,
                    body = convert_body(block_node, env), .ts_node = node)
  )
}

# ---- Functions, lambdas, classes, parameters --------------------------------

parse_funcdef <- function(node, env, decorators = list()) {
  is_async <- has_unnamed_token(node, "async")
  name <- treesitter::node_text(treesitter::node_child_by_field_name(node, "name"))
  params_node <- treesitter::node_child_by_field_name(node, "parameters")
  args <- parse_parameters(params_node, env)
  ret_node <- treesitter::node_child_by_field_name(node, "return_type")
  returns <- if (is.null(ret_node)) NULL else cst_to_ast(ret_node, env)
  tp_node <- treesitter::node_child_by_field_name(node, "type_parameters")
  type_params <- parse_type_params(tp_node, env)
  body <- convert_body(treesitter::node_child_by_field_name(node, "body"), env)
  kind <- if (is_async) "AsyncFunctionDef" else "FunctionDef"
  ast_node(
    kind,
    name = name,
    args = args,
    body = body,
    decorator_list = decorators,
    returns = returns,
    type_comment = NULL,
    type_params = type_params,
    .ts_node = node
  )
}

parse_classdef <- function(node, env, decorators = list()) {
  name <- treesitter::node_text(treesitter::node_child_by_field_name(node, "name"))
  sup_node <- treesitter::node_child_by_field_name(node, "superclasses")
  call_parts <- parse_arg_list(sup_node, env)
  tp_node <- treesitter::node_child_by_field_name(node, "type_parameters")
  type_params <- parse_type_params(tp_node, env)
  body <- convert_body(treesitter::node_child_by_field_name(node, "body"), env)
  ast_node(
    "ClassDef",
    name = name,
    bases = call_parts$args,
    keywords = call_parts$keywords,
    body = body,
    decorator_list = decorators,
    type_params = type_params,
    .ts_node = node
  )
}

parse_parameters <- function(params_node, env) {
  posonlyargs <- list()
  args <- list()
  vararg <- NULL
  kwonlyargs <- list()
  kw_defaults <- list()
  kwarg <- NULL
  defaults <- list()

  if (is.null(params_node)) {
    return(ast_node("arguments", posonlyargs = posonlyargs, args = args,
                    vararg = vararg, kwonlyargs = kwonlyargs,
                    kw_defaults = kw_defaults, kwarg = kwarg, defaults = defaults))
  }

  # Parameter entries before `/` move to `posonlyargs`; entries after `*` or
  # `*args` go to `kwonlyargs` (whose defaults list aligns 1-to-1 and pads missing
  # defaults with `NULL` / `None`, matching Python's `ast.arguments`).
  in_kwonly <- FALSE
  pos_defaults <- list()

  for (ch in named_non_extra(params_node)) {
    tp <- treesitter::node_type(ch)
    if (tp == "positional_separator") {
      posonlyargs <- c(posonlyargs, args)
      args <- list()
      next
    }
    if (tp == "keyword_separator") {
      in_kwonly <- TRUE
      next
    }
    p <- dissect_param(ch, env)
    if (p$kind == "vararg") {
      vararg <- p$arg
      in_kwonly <- TRUE
    } else if (p$kind == "kwarg") {
      kwarg <- p$arg
    } else if (in_kwonly) {
      kwonlyargs[[length(kwonlyargs) + 1L]] <- p$arg
      kw_defaults[length(kw_defaults) + 1L] <- list(p$default)
    } else {
      args[[length(args) + 1L]] <- p$arg
      if (!is.null(p$default)) {
        pos_defaults[[length(pos_defaults) + 1L]] <- p$default
      }
    }
  }

  ast_node(
    "arguments",
    posonlyargs = posonlyargs,
    args = args,
    vararg = vararg,
    kwonlyargs = kwonlyargs,
    kw_defaults = kw_defaults,
    kwarg = kwarg,
    defaults = pos_defaults,
    .ts_node = params_node
  )
}

dissect_param <- function(node, env) {
  tp <- treesitter::node_type(node)
  switch(tp,
    identifier = list(
      kind = "normal",
      arg = ast_node("arg", arg = treesitter::node_text(node),
                     annotation = NULL, type_comment = NULL, .ts_node = node),
      default = NULL
    ),
    default_parameter = {
      nm <- treesitter::node_text(treesitter::node_child_by_field_name(node, "name"))
      val <- cst_to_ast(treesitter::node_child_by_field_name(node, "value"), env)
      list(
        kind = "normal",
        arg = ast_node("arg", arg = nm, annotation = NULL,
                       type_comment = NULL, .ts_node = node),
        default = val
      )
    },
    typed_parameter = {
      ch <- named_non_extra(node)
      first <- ch[[1L]]
      ftp <- treesitter::node_type(first)
      ann <- cst_to_ast(treesitter::node_child_by_field_name(node, "type"), env)
      if (ftp == "list_splat_pattern") {
        nm <- treesitter::node_text(named_non_extra(first)[[1L]])
        list(kind = "vararg",
             arg = ast_node("arg", arg = nm, annotation = ann,
                            type_comment = NULL, .ts_node = node),
             default = NULL)
      } else if (ftp == "dictionary_splat_pattern") {
        nm <- treesitter::node_text(named_non_extra(first)[[1L]])
        list(kind = "kwarg",
             arg = ast_node("arg", arg = nm, annotation = ann,
                            type_comment = NULL, .ts_node = node),
             default = NULL)
      } else {
        nm <- treesitter::node_text(first)
        list(kind = "normal",
             arg = ast_node("arg", arg = nm, annotation = ann,
                            type_comment = NULL, .ts_node = node),
             default = NULL)
      }
    },
    typed_default_parameter = {
      nm <- treesitter::node_text(treesitter::node_child_by_field_name(node, "name"))
      ann <- cst_to_ast(treesitter::node_child_by_field_name(node, "type"), env)
      val <- cst_to_ast(treesitter::node_child_by_field_name(node, "value"), env)
      list(
        kind = "normal",
        arg = ast_node("arg", arg = nm, annotation = ann,
                       type_comment = NULL, .ts_node = node),
        default = val
      )
    },
    list_splat_pattern = {
      nm <- treesitter::node_text(named_non_extra(node)[[1L]])
      list(kind = "vararg",
           arg = ast_node("arg", arg = nm, annotation = NULL,
                          type_comment = NULL, .ts_node = node),
           default = NULL)
    },
    dictionary_splat_pattern = {
      nm <- treesitter::node_text(named_non_extra(node)[[1L]])
      list(kind = "kwarg",
           arg = ast_node("arg", arg = nm, annotation = NULL,
                          type_comment = NULL, .ts_node = node),
           default = NULL)
    }
  )
}

parse_type_params <- function(tp_node, env) {
  if (is.null(tp_node)) return(list())
  lapply(named_non_extra(tp_node), function(t) {
    inner <- named_non_extra(t)[[1L]]
    itp <- treesitter::node_type(inner)
    if (itp == "identifier") {
      ast_node("TypeVar", name = treesitter::node_text(inner),
               bound = NULL, .ts_node = t)
    } else if (itp == "constrained_type") {
      ch <- named_non_extra(inner)
      nm <- treesitter::node_text(named_non_extra(ch[[1L]])[[1L]])
      ast_node("TypeVar", name = nm, bound = cst_to_ast(ch[[2L]], env), .ts_node = t)
    } else if (itp == "splat_type") {
      nm <- treesitter::node_text(named_non_extra(inner)[[1L]])
      if (has_unnamed_token(inner, "**")) {
        ast_node("ParamSpec", name = nm, .ts_node = t)
      } else {
        ast_node("TypeVarTuple", name = nm, .ts_node = t)
      }
    } else {
      cst_to_ast(inner, env)
    }
  })
}

parse_type_alias <- function(node, env) {
  left <- treesitter::node_child_by_field_name(node, "left")
  right <- treesitter::node_child_by_field_name(node, "right")
  inner <- named_non_extra(left)[[1L]]
  if (identical(treesitter::node_type(inner), "generic_type")) {
    ch <- named_non_extra(inner)
    name_node <- set_ctx(cst_to_ast(ch[[1L]], env), "Store")
    type_params <- parse_type_params(ch[[2L]], env)
  } else {
    name_node <- set_ctx(cst_to_ast(inner, env), "Store")
    type_params <- list()
  }
  ast_node("TypeAlias", name = name_node, type_params = type_params,
           value = cst_to_ast(right, env), .ts_node = node)
}

# ---- Expression helpers -----------------------------------------------------

parse_boolop <- function(node, env) {
  op_tok <- treesitter::node_type(treesitter::node_child_by_field_name(node, "operator"))
  op_name <- if (op_tok == "and") "And" else "Or"
  # Flatten chained `a and b and c` into `BoolOp(And(), [a, b, c])` like Python.
  values <- collect_boolop_values(node, op_tok, env)
  ast_node("BoolOp", op = ast_op(op_name), values = values, .ts_node = node)
}

collect_boolop_values <- function(node, op_tok, env) {
  if (identical(treesitter::node_type(node), "boolean_operator") &&
      identical(treesitter::node_type(treesitter::node_child_by_field_name(node, "operator")), op_tok)) {
    left <- treesitter::node_child_by_field_name(node, "left")
    right <- treesitter::node_child_by_field_name(node, "right")
    c(collect_boolop_values(left, op_tok, env),
      collect_boolop_values(right, op_tok, env))
  } else {
    list(cst_to_ast(node, env))
  }
}

parse_compare <- function(node, env) {
  n <- treesitter::node_child_count(node)
  ops <- list()
  operands <- list()
  for (i in seq_len(n)) {
    ch <- treesitter::node_child(node, i)
    fn <- treesitter::node_field_name_for_child(node, i)
    if (!is.na(fn) && identical(fn, "operators")) {
      op_tok <- treesitter::node_type(ch)
      ops[[length(ops) + 1L]] <- ast_op(cmpop_map[[op_tok]])
    } else if (treesitter::node_is_named(ch) &&
               !treesitter::node_is_extra(ch) &&
               !identical(treesitter::node_type(ch), "comment")) {
      operands[[length(operands) + 1L]] <- cst_to_ast(ch, env)
    }
  }
  ast_node("Compare", left = operands[[1L]], ops = ops,
           comparators = operands[-1L], .ts_node = node)
}

parse_dict <- function(node, env) {
  keys <- list()
  values <- list()
  for (ch in named_non_extra(node)) {
    tp <- treesitter::node_type(ch)
    if (tp == "pair") {
      keys[[length(keys) + 1L]] <- cst_to_ast(treesitter::node_child_by_field_name(ch, "key"), env)
      values[[length(values) + 1L]] <- cst_to_ast(treesitter::node_child_by_field_name(ch, "value"), env)
    } else if (tp == "dictionary_splat") {
      # `**d` inside a dict literal uses `key = NULL` (`None` in Python's AST).
      keys[length(keys) + 1L] <- list(NULL)
      values[[length(values) + 1L]] <- cst_to_ast(named_non_extra(ch)[[1L]], env)
    }
  }
  ast_node("Dict", keys = keys, values = values, .ts_node = node)
}

parse_call <- function(node, env) {
  fn <- cst_to_ast(treesitter::node_child_by_field_name(node, "function"), env)
  args_node <- treesitter::node_child_by_field_name(node, "arguments")
  parts <- parse_arg_list(args_node, env)
  ast_node("Call", func = fn, args = parts$args, keywords = parts$keywords,
           .ts_node = node)
}

parse_arg_list <- function(args_node, env) {
  args <- list()
  keywords <- list()
  if (is.null(args_node)) return(list(args = args, keywords = keywords))
  if (identical(treesitter::node_type(args_node), "generator_expression")) {
    return(list(args = list(cst_to_ast(args_node, env)), keywords = keywords))
  }
  for (ch in named_non_extra(args_node)) {
    tp <- treesitter::node_type(ch)
    if (tp == "keyword_argument") {
      nm <- treesitter::node_text(treesitter::node_child_by_field_name(ch, "name"))
      val <- cst_to_ast(treesitter::node_child_by_field_name(ch, "value"), env)
      keywords[[length(keywords) + 1L]] <- ast_node("keyword", arg = nm, value = val, .ts_node = ch)
    } else if (tp == "dictionary_splat") {
      val <- cst_to_ast(named_non_extra(ch)[[1L]], env)
      keywords[[length(keywords) + 1L]] <- ast_node("keyword", arg = NULL, value = val, .ts_node = ch)
    } else {
      args[[length(args) + 1L]] <- cst_to_ast(ch, env)
    }
  }
  list(args = args, keywords = keywords)
}

parse_subscript <- function(node, env) {
  val <- cst_to_ast(treesitter::node_child_by_field_name(node, "value"), env)
  sub_nodes <- children_by_field(node, "subscript")
  subs <- lapply(sub_nodes, cst_to_ast, env = env)
  # Multiple comma-separated subscripts (`x[a, b]`) or trailing comma (`x[a,]`)
  # form a Tuple slice in Python 3.9+.
  has_comma <- has_unnamed_token(node, ",")
  sl <- if (length(subs) == 1L && !has_comma) {
    subs[[1L]]
  } else {
    ast_node("Tuple", elts = subs, ctx = ast_op("Load"), .ts_node = node)
  }
  ast_node("Subscript", value = val, slice = sl, ctx = ast_op("Load"), .ts_node = node)
}

parse_slice <- function(node, env) {
  # Walk all children (including unnamed `:` tokens) to distinguish
  # `a:b`, `:b`, `a::c`, `::`, etc.
  n <- treesitter::node_child_count(node)
  parts <- list(NULL, NULL, NULL)
  slot <- 1L
  for (i in seq_len(n)) {
    ch <- treesitter::node_child(node, i)
    if (!treesitter::node_is_named(ch) && identical(treesitter::node_type(ch), ":")) {
      slot <- slot + 1L
    } else if (treesitter::node_is_named(ch) &&
               !treesitter::node_is_extra(ch) &&
               !identical(treesitter::node_type(ch), "comment")) {
      parts[[slot]] <- cst_to_ast(ch, env)
    }
  }
  ast_node("Slice", lower = parts[[1L]], upper = parts[[2L]],
           step = parts[[3L]], .ts_node = node)
}

parse_comprehension_clauses <- function(node, env) {
  clauses <- list()
  cur <- NULL
  for (ch in named_non_extra(node)) {
    tp <- treesitter::node_type(ch)
    if (tp == "for_in_clause") {
      if (!is.null(cur)) clauses[[length(clauses) + 1L]] <- cur
      is_async <- if (has_unnamed_token(ch, "async")) 1L else 0L
      target <- set_ctx(cst_to_ast(treesitter::node_child_by_field_name(ch, "left"), env), "Store")
      iter <- cst_to_ast(treesitter::node_child_by_field_name(ch, "right"), env)
      cur <- ast_node("comprehension", target = target, iter = iter,
                      ifs = list(), is_async = is_async, .ts_node = ch)
    } else if (tp == "if_clause") {
      cond <- cst_to_ast(named_non_extra(ch)[[1L]], env)
      cur$ifs[[length(cur$ifs) + 1L]] <- cond
    }
  }
  if (!is.null(cur)) clauses[[length(clauses) + 1L]] <- cur
  clauses
}

# ---- Structural pattern matching (`match` / `case`) -------------------------

parse_match_stmt <- function(node, env) {
  subj_nodes <- children_by_field(node, "subject")
  subject <- if (length(subj_nodes) == 1L) {
    cst_to_ast(subj_nodes[[1L]], env)
  } else {
    ast_node("Tuple", elts = lapply(subj_nodes, cst_to_ast, env = env),
             ctx = ast_op("Load"), .ts_node = node)
  }
  body_node <- treesitter::node_child_by_field_name(node, "body")
  case_nodes <- children_by_field(body_node, "alternative")
  cases <- lapply(case_nodes, function(cn) {
    guard_node <- treesitter::node_child_by_field_name(cn, "guard")
    guard <- if (is.null(guard_node)) NULL else cst_to_ast(named_non_extra(guard_node)[[1L]], env)
    conseq <- convert_body(treesitter::node_child_by_field_name(cn, "consequence"), env)
    pat_nodes <- list()
    for (ch in named_non_extra(cn)) {
      if (identical(treesitter::node_type(ch), "case_pattern")) {
        pat_nodes[[length(pat_nodes) + 1L]] <- parse_case_pattern(ch, env)
      }
    }
    pat <- if (length(pat_nodes) == 1L) pat_nodes[[1L]] else ast_node("MatchSequence", patterns = pat_nodes, .ts_node = cn)
    ast_node("match_case", pattern = pat, guard = guard, body = conseq, .ts_node = cn)
  })
  ast_node("Match", subject = subject, cases = cases, .ts_node = node)
}

parse_case_pattern <- function(node, env) {
  tp <- treesitter::node_type(node)
  if (tp == "case_pattern") {
    ch <- named_non_extra(node)
    if (length(ch) == 0L) {
      # Wildcard `_` is an unnamed token inside `case_pattern`.
      return(ast_node("MatchAs", pattern = NULL, name = NULL, .ts_node = node))
    }
    return(parse_case_pattern(ch[[1L]], env))
  }
  switch(tp,
    dotted_name = {
      ids <- named_non_extra(node)
      if (length(ids) == 1L) {
        nm <- treesitter::node_text(ids[[1L]])
        if (nm == "_") {
          ast_node("MatchAs", pattern = NULL, name = NULL, .ts_node = node)
        } else {
          ast_node("MatchAs", pattern = NULL, name = nm, .ts_node = node)
        }
      } else {
        # Dotted name like `Color.RED` is a value pattern.
        acc <- cst_to_ast(ids[[1L]], env)
        for (i in seq_len(length(ids) - 1L) + 1L) {
          acc <- ast_node("Attribute", value = acc,
                          attr = treesitter::node_text(ids[[i]]),
                          ctx = ast_op("Load"), .ts_node = node)
        }
        ast_node("MatchValue", value = acc, .ts_node = node)
      }
    },
    true = ast_node("MatchSingleton", value = TRUE, .ts_node = node),
    false = ast_node("MatchSingleton", value = FALSE, .ts_node = node),
    none = ast_node("MatchSingleton", value = NULL, .ts_node = node),
    integer = ,
    float = ,
    string = ,
    concatenated_string = ast_node("MatchValue", value = cst_to_ast(node, env), .ts_node = node),
    as_pattern = {
      ch <- named_non_extra(node)
      pat <- parse_case_pattern(ch[[1L]], env)
      nm <- treesitter::node_text(ch[[2L]])
      ast_node("MatchAs", pattern = pat, name = nm, .ts_node = node)
    },
    union_pattern = {
      pats <- lapply(named_non_extra(node), parse_case_pattern, env = env)
      ast_node("MatchOr", patterns = pats, .ts_node = node)
    },
    list_pattern = ,
    tuple_pattern = {
      pats <- lapply(named_non_extra(node), parse_case_pattern, env = env)
      ast_node("MatchSequence", patterns = pats, .ts_node = node)
    },
    splat_pattern = {
      ch <- named_non_extra(node)
      nm <- if (length(ch) == 0L || identical(treesitter::node_text(ch[[1L]]), "_")) NULL else treesitter::node_text(ch[[1L]])
      ast_node("MatchStar", name = nm, .ts_node = node)
    },
    dict_pattern = {
      key_nodes <- children_by_field(node, "key")
      val_nodes <- children_by_field(node, "value")
      keys <- lapply(key_nodes, cst_to_ast, env = env)
      patterns <- lapply(val_nodes, parse_case_pattern, env = env)
      rest <- NULL
      for (ch in named_non_extra(node)) {
        if (identical(treesitter::node_type(ch), "splat_pattern")) {
          sch <- named_non_extra(ch)
          rest <- if (length(sch) == 0L) NULL else treesitter::node_text(sch[[1L]])
        }
      }
      ast_node("MatchMapping", keys = keys, patterns = patterns,
               rest = rest, .ts_node = node)
    },
    class_pattern = {
      ch <- named_non_extra(node)
      cls_node <- ch[[1L]]
      cls_ids <- named_non_extra(cls_node)
      cls_expr <- cst_to_ast(cls_ids[[1L]], env)
      if (length(cls_ids) > 1L) {
        for (i in seq_len(length(cls_ids) - 1L) + 1L) {
          cls_expr <- ast_node("Attribute", value = cls_expr,
                               attr = treesitter::node_text(cls_ids[[i]]),
                               ctx = ast_op("Load"), .ts_node = cls_node)
        }
      }
      pos_pats <- list()
      kwd_attrs <- list()
      kwd_pats <- list()
      for (arg in ch[-1L]) {
        inner <- if (identical(treesitter::node_type(arg), "case_pattern")) named_non_extra(arg)[[1L]] else arg
        if (!is.null(inner) && identical(treesitter::node_type(inner), "keyword_pattern")) {
          kch <- named_non_extra(inner)
          kwd_attrs[[length(kwd_attrs) + 1L]] <- treesitter::node_text(kch[[1L]])
          kwd_pats[[length(kwd_pats) + 1L]] <- parse_case_pattern(kch[[2L]], env)
        } else {
          pos_pats[[length(pos_pats) + 1L]] <- parse_case_pattern(arg, env)
        }
      }
      ast_node("MatchClass", cls = cls_expr, patterns = pos_pats,
               kwd_attrs = kwd_attrs, kwd_patterns = kwd_pats, .ts_node = node)
    },
    ast_node("Unsupported", cst_type = tp,
             text = treesitter::node_text(node), .ts_node = node)
  )
}
