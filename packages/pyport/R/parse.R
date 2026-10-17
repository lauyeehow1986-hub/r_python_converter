#' Parse Python source code into a normalized AST
#'
#' Parses a character string (or a file via [parse_python_file()]) using the
#' vendored 'tree-sitter-python' grammar and normalizes the resulting concrete
#' syntax tree into a Python abstract syntax tree whose node kinds and fields
#' follow Python's `ast` module. No Python installation is required.
#'
#' @param code A character vector of Python source lines (collapsed with `"\n"`).
#' @param filename Optional file path used in syntax-error messages.
#' @param mode Parsing mode: `"exec"` (the default) returns a `Module` node;
#'   `"eval"` parses a single expression and wraps it in an `Expression` node.
#' @param error_on_syntax Logical; if `TRUE` (the default), raises a classed
#'   error (`pyport_syntax_error`) when the source contains a syntax error.
#' @returns An S3 object of class `c("py_ast_<Kind>", "py_ast_node")`. Print it
#'   or pass it to [ast_dump()] to inspect the tree.
#' @export
#' @examples
#' mod <- parse_python("def add(a, b = 1):\n    return a + b\n")
#' mod
parse_python <- function(code,
                         filename = "<string>",
                         mode = c("exec", "eval"),
                         error_on_syntax = TRUE) {
  mode <- match.arg(mode)
  if (!is.character(code)) {
    stop("`code` must be a character vector.")
  }
  text <- enc2utf8(paste(code, collapse = "\n"))
  tree <- treesitter::parser_parse(python_parser(), text)
  root <- treesitter::tree_root_node(tree)

  if (isTRUE(error_on_syntax) && treesitter::node_has_error(root)) {
    err <- find_first_error(root)
    loc <- if (!is.null(err)) node_loc(err) else node_loc(root)
    msg <- sprintf(
      "Python syntax error in %s at line %d, column %d: near `%s`",
      filename, loc$lineno, loc$col_offset + 1L,
      if (!is.null(err)) truncate_snippet(treesitter::node_text(err)) else ""
    )
    cond <- structure(
      list(message = msg, call = NULL, filename = filename, loc = loc),
      class = c("pyport_syntax_error", "error", "condition")
    )
    stop(cond)
  }

  env <- list(filename = filename, source_bytes_str = text)
  mod <- cst_to_ast(root, env)
  if (mode == "eval") {
    if (length(mod$body) != 1L || !identical(mod$body[[1L]]$kind, "Expr")) {
      stop("`mode = 'eval'` requires a single Python expression.")
    }
    return(ast_node("Expression", body = mod$body[[1L]]$value, .ts_node = root))
  }
  mod
}

#' @rdname parse_python
#' @param path Path to a `.py` file (read as UTF-8).
#' @export
parse_python_file <- function(path, error_on_syntax = TRUE) {
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  parse_python(lines, filename = path, error_on_syntax = error_on_syntax)
}

find_first_error <- function(node) {
  if (treesitter::node_is_error(node) || treesitter::node_is_missing(node)) {
    return(node)
  }
  if (!treesitter::node_has_error(node)) {
    return(NULL)
  }
  n <- treesitter::node_child_count(node)
  for (i in seq_len(n)) {
    found <- find_first_error(treesitter::node_child(node, i))
    if (!is.null(found)) return(found)
  }
  node
}

truncate_snippet <- function(s) {
  s <- gsub("[\r\n]+", " ", s)
  if (nchar(s) > 40L) paste0(substr(s, 1L, 37L), "...") else s
}

# ---- Tree-sitter child helpers ----------------------------------------------

# Non-comment named children, in order.
named_non_extra <- function(node) {
  ch <- treesitter::node_named_children(node)
  if (length(ch) == 0L) return(list())
  keep <- !vapply(ch, function(c) {
    treesitter::node_is_extra(c) || identical(treesitter::node_type(c), "comment")
  }, logical(1))
  ch[keep]
}

has_unnamed_token <- function(node, tok) {
  !is.null(find_unnamed_child(node, tok))
}

find_unnamed_child <- function(node, tok) {
  n <- treesitter::node_child_count(node)
  for (i in seq_len(n)) {
    ch <- treesitter::node_child(node, i)
    if (!treesitter::node_is_named(ch) && identical(treesitter::node_type(ch), tok)) {
      return(ch)
    }
  }
  NULL
}

# Collect all children assigned to a repeatable field name.
children_by_field <- function(node, field_name) {
  n <- treesitter::node_child_count(node)
  out <- list()
  for (i in seq_len(n)) {
    fn <- treesitter::node_field_name_for_child(node, i)
    if (!is.na(fn) && identical(fn, field_name)) {
      out[[length(out) + 1L]] <- treesitter::node_child(node, i)
    }
  }
  out
}

# Convert a block or statement list into a flat list of AST statement nodes.
convert_body <- function(block_node, env) {
  if (is.null(block_node)) return(list())
  stmts <- named_non_extra(block_node)
  out <- list()
  for (s in stmts) {
    res <- cst_to_ast(s, env)
    if (inherits(res, "py_ast_stmt_seq")) {
      out <- c(out, res$stmts)
    } else if (!is.null(res)) {
      out[[length(out) + 1L]] <- res
    }
  }
  out
}

# ---- Operator mappings ------------------------------------------------------

binop_map <- c(
  "+" = "Add", "-" = "Sub", "*" = "Mult", "@" = "MatMult",
  "/" = "Div", "//" = "FloorDiv", "%" = "Mod", "**" = "Pow",
  "<<" = "LShift", ">>" = "RShift", "|" = "BitOr", "^" = "BitXor", "&" = "BitAnd"
)

augop_map <- c(
  "+=" = "Add", "-=" = "Sub", "*=" = "Mult", "@=" = "MatMult",
  "/=" = "Div", "//=" = "FloorDiv", "%=" = "Mod", "**=" = "Pow",
  "<<=" = "LShift", ">>=" = "RShift", "|=" = "BitOr", "^=" = "BitXor", "&=" = "BitAnd"
)

unaryop_map <- c("+" = "UAdd", "-" = "USub", "~" = "Invert", "not" = "Not")

cmpop_map <- c(
  "==" = "Eq", "!=" = "NotEq", "<>" = "NotEq",
  "<" = "Lt", "<=" = "LtE", ">" = "Gt", ">=" = "GtE",
  "is" = "Is", "is not" = "IsNot", "in" = "In", "not in" = "NotIn"
)

# Set target context (`Store()` or `Del()`) recursively on an assignable
# expression (`Name`, `Attribute`, `Subscript`, `Starred`, `Tuple`, `List`).
set_ctx <- function(expr, ctx_name) {
  if (is.null(expr) || !inherits(expr, "py_ast_node")) return(expr)
  ctx <- ast_op(ctx_name)
  if (expr$kind %in% c("Name", "Attribute", "Subscript")) {
    expr$ctx <- ctx
  } else if (expr$kind == "Starred") {
    expr$value <- set_ctx(expr$value, ctx_name = ctx_name)
    expr$ctx <- ctx
  } else if (expr$kind %in% c("Tuple", "List")) {
    expr$elts <- lapply(expr$elts, set_ctx, ctx_name = ctx_name)
    expr$ctx <- ctx
  }
  expr
}

# ---- Main CST -> AST dispatcher ---------------------------------------------

cst_to_ast <- function(node, env, decorators = list()) {
  if (is.null(node)) return(NULL)
  tp <- treesitter::node_type(node)

  switch(tp,
    module = ast_node("Module", body = convert_body(node, env),
                      type_ignores = list(), .ts_node = node),

    # ---- Statements ---------------------------------------------------------
    expression_statement = parse_expression_statement(node, env),
    assignment = parse_assignment(node, env),
    augmented_assignment = parse_aug_assign(node, env),
    return_statement = {
      ch <- named_non_extra(node)
      val <- if (length(ch) == 0L) NULL else cst_to_ast(ch[[1L]], env)
      ast_node("Return", value = val, .ts_node = node)
    },
    pass_statement = ast_node("Pass", .ts_node = node),
    break_statement = ast_node("Break", .ts_node = node),
    continue_statement = ast_node("Continue", .ts_node = node),
    global_statement = {
      ids <- vapply(named_non_extra(node), treesitter::node_text, character(1))
      ast_node("Global", names = as.list(ids), .ts_node = node)
    },
    nonlocal_statement = {
      ids <- vapply(named_non_extra(node), treesitter::node_text, character(1))
      ast_node("Nonlocal", names = as.list(ids), .ts_node = node)
    },
    delete_statement = {
      ch <- named_non_extra(node)
      targets <- if (length(ch) == 1L && identical(treesitter::node_type(ch[[1L]]), "expression_list")) {
        lapply(named_non_extra(ch[[1L]]), function(x) set_ctx(cst_to_ast(x, env), "Del"))
      } else {
        lapply(ch, function(x) set_ctx(cst_to_ast(x, env), "Del"))
      }
      ast_node("Delete", targets = targets, .ts_node = node)
    },
    assert_statement = {
      ch <- named_non_extra(node)
      test <- cst_to_ast(ch[[1L]], env)
      msg <- if (length(ch) >= 2L) cst_to_ast(ch[[2L]], env) else NULL
      ast_node("Assert", test = test, msg = msg, .ts_node = node)
    },
    raise_statement = parse_raise(node, env),
    import_statement = {
      names <- lapply(children_by_field(node, "name"), parse_import_alias)
      ast_node("Import", names = names, .ts_node = node)
    },
    import_from_statement = parse_import_from(node),
    future_import_statement = {
      names <- lapply(children_by_field(node, "name"), parse_import_alias)
      ast_node("ImportFrom", module = "__future__", names = names,
               level = 0L, .ts_node = node)
    },
    if_statement = parse_if_stmt(node, env),
    while_statement = {
      cond <- cst_to_ast(treesitter::node_child_by_field_name(node, "condition"), env)
      body <- convert_body(treesitter::node_child_by_field_name(node, "body"), env)
      alt <- treesitter::node_child_by_field_name(node, "alternative")
      orelse <- if (is.null(alt)) list() else convert_body(treesitter::node_child_by_field_name(alt, "body"), env)
      ast_node("While", test = cond, body = body, orelse = orelse, .ts_node = node)
    },
    for_statement = {
      is_async <- has_unnamed_token(node, "async")
      target <- set_ctx(cst_to_ast(treesitter::node_child_by_field_name(node, "left"), env), "Store")
      iter <- cst_to_ast(treesitter::node_child_by_field_name(node, "right"), env)
      body <- convert_body(treesitter::node_child_by_field_name(node, "body"), env)
      alt <- treesitter::node_child_by_field_name(node, "alternative")
      orelse <- if (is.null(alt)) list() else convert_body(treesitter::node_child_by_field_name(alt, "body"), env)
      kind <- if (is_async) "AsyncFor" else "For"
      ast_node(kind, target = target, iter = iter, body = body, orelse = orelse,
               type_comment = NULL, .ts_node = node)
    },
    with_statement = parse_with_stmt(node, env),
    try_statement = parse_try_stmt(node, env),
    function_definition = parse_funcdef(node, env, decorators = decorators),
    class_definition = parse_classdef(node, env, decorators = decorators),
    decorated_definition = {
      def_node <- treesitter::node_child_by_field_name(node, "definition")
      decs <- list()
      for (ch in named_non_extra(node)) {
        if (identical(treesitter::node_type(ch), "decorator")) {
          inner <- named_non_extra(ch)[[1L]]
          decs[[length(decs) + 1L]] <- cst_to_ast(inner, env)
        }
      }
      res <- cst_to_ast(def_node, env, decorators = decs)
      attr(res, "loc") <- node_loc(node)
      res
    },
    match_statement = parse_match_stmt(node, env),
    type_alias_statement = parse_type_alias(node, env),

    # ---- Expressions --------------------------------------------------------
    identifier = ast_node("Name", id = treesitter::node_text(node),
                          ctx = ast_op("Load"), .ts_node = node),
    true = ast_node("Constant", value = TRUE, ctype = "bool",
                    s_kind = NULL, .ts_node = node),
    false = ast_node("Constant", value = FALSE, ctype = "bool",
                     s_kind = NULL, .ts_node = node),
    none = ast_node("Constant", value = NULL, ctype = "None",
                    s_kind = NULL, .ts_node = node),
    ellipsis = ast_node("Constant", value = quote(...), ctype = "ellipsis",
                        s_kind = NULL, .ts_node = node),
    integer = parse_int_literal(node),
    float = parse_float_literal(node),
    string = parse_string_node(node, env),
    concatenated_string = parse_concatenated_string(node, env),

    parenthesized_expression = {
      inner <- named_non_extra(node)[[1L]]
      cst_to_ast(inner, env)
    },
    tuple_pattern = {
      ch <- named_non_extra(node)
      # `(z)` on the left of an assignment is parsed as a 1-child
      # `tuple_pattern` without a comma by tree-sitter; in Python it is a
      # parenthesized target, not a 1-tuple.
      if (length(ch) == 1L && !has_unnamed_token(node, ",")) {
        cst_to_ast(ch[[1L]], env)
      } else {
        elts <- lapply(ch, cst_to_ast, env = env)
        ast_node("Tuple", elts = elts, ctx = ast_op("Load"), .ts_node = node)
      }
    },
    tuple = ,
    expression_list = ,
    pattern_list = {
      elts <- lapply(named_non_extra(node), cst_to_ast, env = env)
      ast_node("Tuple", elts = elts, ctx = ast_op("Load"), .ts_node = node)
    },
    list = ,
    list_pattern = {
      elts <- lapply(named_non_extra(node), cst_to_ast, env = env)
      ast_node("List", elts = elts, ctx = ast_op("Load"), .ts_node = node)
    },
    set = {
      elts <- lapply(named_non_extra(node), cst_to_ast, env = env)
      ast_node("Set", elts = elts, .ts_node = node)
    },
    dictionary = parse_dict(node, env),

    list_splat = ,
    list_splat_pattern = ,
    parenthesized_list_splat = {
      inner <- named_non_extra(node)[[1L]]
      ast_node("Starred", value = cst_to_ast(inner, env),
               ctx = ast_op("Load"), .ts_node = node)
    },

    binary_operator = {
      left <- cst_to_ast(treesitter::node_child_by_field_name(node, "left"), env)
      right <- cst_to_ast(treesitter::node_child_by_field_name(node, "right"), env)
      op_tok <- treesitter::node_type(treesitter::node_child_by_field_name(node, "operator"))
      ast_node("BinOp", left = left, op = ast_op(binop_map[[op_tok]]),
               right = right, .ts_node = node)
    },
    unary_operator = {
      operand <- cst_to_ast(treesitter::node_child_by_field_name(node, "argument"), env)
      op_tok <- treesitter::node_type(treesitter::node_child_by_field_name(node, "operator"))
      ast_node("UnaryOp", op = ast_op(unaryop_map[[op_tok]]),
               operand = operand, .ts_node = node)
    },
    not_operator = {
      operand <- cst_to_ast(treesitter::node_child_by_field_name(node, "argument"), env)
      ast_node("UnaryOp", op = ast_op("Not"), operand = operand, .ts_node = node)
    },
    boolean_operator = parse_boolop(node, env),
    comparison_operator = parse_compare(node, env),
    conditional_expression = {
      ch <- named_non_extra(node)
      ast_node("IfExp", test = cst_to_ast(ch[[2L]], env),
               body = cst_to_ast(ch[[1L]], env),
               orelse = cst_to_ast(ch[[3L]], env), .ts_node = node)
    },
    named_expression = {
      target <- set_ctx(cst_to_ast(treesitter::node_child_by_field_name(node, "name"), env), "Store")
      value <- cst_to_ast(treesitter::node_child_by_field_name(node, "value"), env)
      ast_node("NamedExpr", target = target, value = value, .ts_node = node)
    },
    lambda = {
      p_node <- treesitter::node_child_by_field_name(node, "parameters")
      args <- parse_parameters(p_node, env)
      body <- cst_to_ast(treesitter::node_child_by_field_name(node, "body"), env)
      ast_node("Lambda", args = args, body = body, .ts_node = node)
    },
    call = parse_call(node, env),
    attribute = {
      val <- cst_to_ast(treesitter::node_child_by_field_name(node, "object"), env)
      attr_name <- treesitter::node_text(treesitter::node_child_by_field_name(node, "attribute"))
      ast_node("Attribute", value = val, attr = attr_name,
               ctx = ast_op("Load"), .ts_node = node)
    },
    subscript = parse_subscript(node, env),
    slice = parse_slice(node, env),
    list_comprehension = ast_node(
      "ListComp",
      elt = cst_to_ast(treesitter::node_child_by_field_name(node, "body"), env),
      generators = parse_comprehension_clauses(node, env),
      .ts_node = node
    ),
    set_comprehension = ast_node(
      "SetComp",
      elt = cst_to_ast(treesitter::node_child_by_field_name(node, "body"), env),
      generators = parse_comprehension_clauses(node, env),
      .ts_node = node
    ),
    generator_expression = ast_node(
      "GeneratorExp",
      elt = cst_to_ast(treesitter::node_child_by_field_name(node, "body"), env),
      generators = parse_comprehension_clauses(node, env),
      .ts_node = node
    ),
    dictionary_comprehension = {
      pair_node <- treesitter::node_child_by_field_name(node, "body")
      key <- cst_to_ast(treesitter::node_child_by_field_name(pair_node, "key"), env)
      val <- cst_to_ast(treesitter::node_child_by_field_name(pair_node, "value"), env)
      ast_node("DictComp", key = key, value = val,
               generators = parse_comprehension_clauses(node, env), .ts_node = node)
    },
    yield = {
      ch <- named_non_extra(node)
      val <- if (length(ch) == 0L) NULL else cst_to_ast(ch[[1L]], env)
      if (has_unnamed_token(node, "from")) {
        ast_node("YieldFrom", value = val, .ts_node = node)
      } else {
        ast_node("Yield", value = val, .ts_node = node)
      }
    },
    await = {
      inner <- named_non_extra(node)[[1L]]
      ast_node("Await", value = cst_to_ast(inner, env), .ts_node = node)
    },
    type = {
      inner <- named_non_extra(node)[[1L]]
      cst_to_ast(inner, env)
    },
    generic_type = {
      ch <- named_non_extra(node)
      base <- cst_to_ast(ch[[1L]], env)
      tp_node <- ch[[2L]]
      subs <- lapply(named_non_extra(tp_node), cst_to_ast, env = env)
      sl <- if (length(subs) == 1L) subs[[1L]] else ast_node("Tuple", elts = subs, ctx = ast_op("Load"), .ts_node = tp_node)
      ast_node("Subscript", value = base, slice = sl, ctx = ast_op("Load"), .ts_node = node)
    },
    union_type = {
      ch <- named_non_extra(node)
      acc <- cst_to_ast(ch[[1L]], env)
      for (i in seq_len(length(ch) - 1L) + 1L) {
        acc <- ast_node("BinOp", left = acc, op = ast_op("BitOr"),
                        right = cst_to_ast(ch[[i]], env), .ts_node = node)
      }
      acc
    },
    member_type = {
      ch <- named_non_extra(node)
      ast_node("Attribute", value = cst_to_ast(ch[[1L]], env),
               attr = treesitter::node_text(ch[[2L]]),
               ctx = ast_op("Load"), .ts_node = node)
    },
    splat_type = {
      inner <- named_non_extra(node)[[1L]]
      ast_node("Starred", value = cst_to_ast(inner, env),
               ctx = ast_op("Load"), .ts_node = node)
    },

    # Fallback: preserve source location and node type as an `Unsupported`
    # AST node rather than silently dropping anything.
    ast_node("Unsupported", cst_type = tp,
             text = treesitter::node_text(node), .ts_node = node)
  )
}
