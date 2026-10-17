# Parsing of Python numeric, string, bytes, and f-string literals.
#
# Numeric literals:
# - Underscore separators (`1_000_000`, `0xdead_beef`) are stripped first.
# - Imaginary literals (`1j`, `3.5J`) become R `complex(real = 0, imaginary = b)`
#   wrapped in a Constant node with `ctype = "complex"`.
# - Floating-point literals go through C's `strtod()` (`ffi_strtod`) so rounding
#   matches CPython bit-for-bit.
# - Integer literals in bases 2, 8, 10, 16 stay as R `integer` when they fit in
#   signed 32-bit (`[-2^31 + 1, 2^31 - 1]`, avoiding R's `NA_integer_` at
#   `-2^31`), and otherwise become `gmp::bigz` (exact arbitrary-precision). Both
#   carry `ctype = "int"` and format identically via `as.character()`.

parse_int_literal <- function(node) {
  raw <- gsub("_", "", treesitter::node_text(node), fixed = TRUE)
  if (grepl("[jJ]$", raw)) {
    num <- sub("[jJ]$", "", raw)
    im <- .Call(ffi_strtod, num)
    return(ast_node("Constant", value = complex(real = 0, imaginary = im),
                    ctype = "complex", s_kind = NULL, .ts_node = node))
  }
  prefix <- tolower(substr(raw, 1L, 2L))
  val <- if (prefix %in% c("0x", "0b")) {
    gmp::as.bigz(raw)
  } else if (prefix == "0o") {
    # `gmp::as.bigz()` reads a leading `0` (without `o`) as octal.
    gmp::as.bigz(paste0("0", substring(raw, 3L)))
  } else {
    # Strip any redundant leading zeros (`00_0` is legal for zero in Python) so
    # `gmp::as.bigz()` does not treat a decimal zero as octal.
    cleaned <- sub("^0+([0-9])", "\\1", raw)
    gmp::as.bigz(cleaned)
  }
  # Keep small integers as R integer for fast downstream handling; fall back to
  # bigz outside 32-bit range.
  if (val >= -2147483647L && val <= 2147483647L) {
    val <- as.integer(val)
  }
  ast_node("Constant", value = val, ctype = "int", s_kind = NULL, .ts_node = node)
}

parse_float_literal <- function(node) {
  raw <- gsub("_", "", treesitter::node_text(node), fixed = TRUE)
  if (grepl("[jJ]$", raw)) {
    num <- sub("[jJ]$", "", raw)
    im <- .Call(ffi_strtod, num)
    return(ast_node("Constant", value = complex(real = 0, imaginary = im),
                    ctype = "complex", s_kind = NULL, .ts_node = node))
  }
  val <- .Call(ffi_strtod, raw)
  ast_node("Constant", value = val, ctype = "float", s_kind = NULL, .ts_node = node)
}

# Parse a single `string` CST node into either a Constant(str/bytes) or a
# JoinedStr (for f-strings).
parse_string_node <- function(node, env) {
  n <- treesitter::node_child_count(node)
  start_tok <- treesitter::node_text(treesitter::node_child(node, 1L))
  prefix <- tolower(sub("['\"]+$", "", start_tok))
  raw_prefix <- sub("['\"]+$", "", start_tok)
  is_raw <- grepl("r", prefix, fixed = TRUE)
  is_bytes <- grepl("b", prefix, fixed = TRUE)
  is_f <- grepl("f", prefix, fixed = TRUE)
  # Python's ast sets kind='u' only when an explicit lowercase/uppercase u
  # prefix was written.
  s_kind <- if (grepl("u", raw_prefix, ignore.case = TRUE)) raw_prefix else NULL

  if (is_f) {
    return(parse_fstring_parts(node, is_raw = is_raw, env = env))
  }

  # Plain or raw string/bytes literal: children between string_start and
  # string_end are a single `string_content` (or none, for the empty string).
  content <- ""
  if (n >= 3L) {
    parts <- character(n - 2L)
    for (i in 2:(n - 1L)) {
      ch <- treesitter::node_child(node, i)
      parts[[i - 1L]] <- treesitter::node_text(ch)
    }
    content <- paste0(parts, collapse = "")
  }

  if (is_bytes) {
    bytes_val <- if (is_raw) charToRaw(content) else unescape_py_bytes(content)
    return(ast_node("Constant", value = bytes_val, ctype = "bytes",
                    s_kind = NULL, .ts_node = node))
  }

  str_val <- if (is_raw) content else unescape_py_string(content)
  ast_node("Constant", value = str_val, ctype = "str",
           s_kind = s_kind, .ts_node = node)
}

# Implicit string concatenation (`"a" "b"` or `f"a{x}" "b"`).
parse_concatenated_string <- function(node, env) {
  children <- treesitter::node_named_children(node)
  parts <- lapply(children, parse_string_node, env = env)
  has_f <- any(vapply(parts, function(p) identical(p$kind, "JoinedStr"), logical(1)))
  if (has_f) {
    values <- list()
    for (p in parts) {
      if (identical(p$kind, "JoinedStr")) {
        values <- c(values, p$values)
      } else {
        values <- c(values, list(p))
      }
    }
    return(ast_node("JoinedStr", values = merge_adjacent_str_constants(values),
                    .ts_node = node))
  }
  first <- parts[[1L]]
  if (identical(first$ctype, "bytes")) {
    raw_all <- unlist(lapply(parts, `[[`, "value"))
    return(ast_node("Constant", value = as.raw(raw_all), ctype = "bytes",
                    s_kind = NULL, .ts_node = node))
  }
  str_all <- paste0(vapply(parts, `[[`, "", "value"), collapse = "")
  ast_node("Constant", value = str_all, ctype = "str",
           s_kind = first$s_kind, .ts_node = node)
}

parse_fstring_parts <- function(node, is_raw, env) {
  n <- treesitter::node_child_count(node)
  values <- list()
  if (n >= 3L) {
    for (i in 2:(n - 1L)) {
      ch <- treesitter::node_child(node, i)
      tp <- treesitter::node_type(ch)
      if (tp == "string_content") {
        txt <- extract_fstring_text(ch, is_raw = is_raw)
        if (nzchar(txt)) {
          values[[length(values) + 1L]] <- ast_node(
            "Constant", value = txt, ctype = "str", s_kind = NULL, .ts_node = ch
          )
        }
      } else if (tp %in% c("interpolation", "format_expression")) {
        values <- c(values, parse_interpolation(ch, is_raw = is_raw, env = env))
      }
    }
  }
  ast_node("JoinedStr", values = merge_adjacent_str_constants(values), .ts_node = node)
}

extract_fstring_text <- function(content_node, is_raw) {
  raw_txt <- treesitter::node_text(content_node)
  # Replace `{{` and `}}` after escape processing so `\\{{` works correctly.
  # Use sentinel bytes \x01 / \x02 temporarily if not raw.
  txt <- gsub("{{", "\x01", raw_txt, fixed = TRUE)
  txt <- gsub("}}", "\x02", txt, fixed = TRUE)
  if (!is_raw) {
    txt <- unescape_py_string(txt)
  }
  txt <- gsub("\x01", "{", txt, fixed = TRUE)
  gsub("\x02", "}", txt, fixed = TRUE)
}

parse_interpolation <- function(node, is_raw, env) {
  expr_node <- treesitter::node_child_by_field_name(node, "expression")
  conv_node <- treesitter::node_child_by_field_name(node, "type_conversion")
  spec_node <- treesitter::node_child_by_field_name(node, "format_specifier")

  # Self-documenting debug expression `f"{x=}"`: an unnamed `=` child appears
  # before the conversion/format_specifier. Python expands this to a Constant
  # containing the source text up to and including `=` followed by the
  # FormattedValue (defaulting to !r when neither conversion nor spec is given).
  eq_node <- find_unnamed_child(node, "=")
  prefix_const <- NULL
  if (!is.null(eq_node)) {
    # Extract verbatim text between `{` and the end of `=`.
    open_brace <- treesitter::node_child(node, 1L)
    start_b <- treesitter::node_end_byte(open_brace) + 1L
    end_b <- treesitter::node_end_byte(eq_node)
    dbg_txt <- substr(env$source_bytes_str, start_b, end_b)
    prefix_const <- ast_node("Constant", value = dbg_txt, ctype = "str",
                             s_kind = NULL, .ts_node = eq_node)
  }

  conv <- -1L
  if (!is.null(conv_node)) {
    ct <- treesitter::node_text(conv_node)
    ch <- substr(ct, 2L, 2L)
    conv <- utf8ToInt(ch)
  } else if (!is.null(eq_node) && is.null(spec_node)) {
    conv <- utf8ToInt("r")
  }

  fmt_spec <- NULL
  if (!is.null(spec_node)) {
    fmt_spec <- parse_format_specifier(spec_node, is_raw = is_raw, env = env)
  }

  fv <- ast_node(
    "FormattedValue",
    value = cst_to_ast(expr_node, env),
    conversion = conv,
    format_spec = fmt_spec,
    .ts_node = node
  )
  if (is.null(prefix_const)) list(fv) else list(prefix_const, fv)
}

parse_format_specifier <- function(spec_node, is_raw, env) {
  # A `format_specifier` begins with `:` and may contain literal text and
  # nested `format_expression` nodes (`f"{x:{w}.{p}f}"`). tree-sitter does not
  # wrap the literal text slices in child nodes, so we walk byte offsets between
  # `{` ... `}` children.
  start_b <- treesitter::node_start_byte(spec_node) + 2L  # skip `:` (1-based)
  end_b <- treesitter::node_end_byte(spec_node)
  named <- treesitter::node_named_children(spec_node)
  values <- list()
  cursor <- start_b
  for (ch in named) {
    ch_start <- treesitter::node_start_byte(ch) + 1L
    if (ch_start > cursor) {
      lit <- substr(env$source_bytes_str, cursor, ch_start - 1L)
      if (!is_raw) lit <- unescape_py_string(lit)
      values[[length(values) + 1L]] <- ast_node(
        "Constant", value = lit, ctype = "str", s_kind = NULL, .ts_node = spec_node
      )
    }
    values <- c(values, parse_interpolation(ch, is_raw = is_raw, env = env))
    cursor <- treesitter::node_end_byte(ch) + 1L
  }
  if (end_b >= cursor) {
    lit <- substr(env$source_bytes_str, cursor, end_b)
    if (!is_raw) lit <- unescape_py_string(lit)
    values[[length(values) + 1L]] <- ast_node(
      "Constant", value = lit, ctype = "str", s_kind = NULL, .ts_node = spec_node
    )
  }
  ast_node("JoinedStr", values = merge_adjacent_str_constants(values),
           .ts_node = spec_node)
}

merge_adjacent_str_constants <- function(values) {
  if (length(values) <= 1L) return(values)
  out <- list()
  for (v in values) {
    n <- length(out)
    if (n > 0L && identical(out[[n]]$kind, "Constant") &&
        identical(v$kind, "Constant") &&
        identical(out[[n]]$ctype, "str") && identical(v$ctype, "str")) {
      out[[n]]$value <- paste0(out[[n]]$value, v$value)
    } else {
      out[[n + 1L]] <- v
    }
  }
  out
}

# Unescape Python string escape sequences into a UTF-8 R string.
unescape_py_string <- function(s) {
  if (!grepl("\\", s, fixed = TRUE)) return(s)
  # Line continuations: `\` followed by `\r\n` or `\n`.
  s <- gsub("\\\\\r?\n", "", s, perl = TRUE)
  chars <- strsplit(s, "", fixed = TRUE)[[1L]]
  n <- length(chars)
  out <- character()
  i <- 1L
  while (i <= n) {
    c1 <- chars[[i]]
    if (c1 != "\\" || i == n) {
      out[[length(out) + 1L]] <- c1
      i <- i + 1L
      next
    }
    c2 <- chars[[i + 1L]]
    i <- i + 2L
    val <- switch(c2,
      "\\" = "\\",
      "'" = "'",
      "\"" = "\"",
      "a" = "\a",
      "b" = "\b",
      "f" = "\f",
      "n" = "\n",
      "r" = "\r",
      "t" = "\t",
      "v" = "\v",
      "x" = {
        hex <- paste0(chars[seq.int(i, min(n, i + 1L))], collapse = "")
        i <- i + 2L
        intToUtf8(strtoi(hex, 16L))
      },
      "u" = {
        hex <- paste0(chars[seq.int(i, min(n, i + 3L))], collapse = "")
        i <- i + 4L
        intToUtf8(strtoi(hex, 16L))
      },
      "U" = {
        hex <- paste0(chars[seq.int(i, min(n, i + 7L))], collapse = "")
        i <- i + 8L
        intToUtf8(strtoi(hex, 16L))
      },
      {
        if (grepl("^[0-7]$", c2)) {
          oct <- c2
          for (k in 1:2) {
            if (i <= n && grepl("^[0-7]$", chars[[i]])) {
              oct <- paste0(oct, chars[[i]])
              i <- i + 1L
            } else {
              break
            }
          }
          intToUtf8(strtoi(oct, 8L))
        } else {
          paste0("\\", c2)
        }
      }
    )
    out[[length(out) + 1L]] <- val
  }
  paste0(out, collapse = "")
}

unescape_py_bytes <- function(s) {
  s <- gsub("\\\\\r?\n", "", s, perl = TRUE)
  raw_in <- charToRaw(s)
  n <- length(raw_in)
  out <- integer()
  i <- 1L
  while (i <= n) {
    b1 <- as.integer(raw_in[[i]])
    if (b1 != 0x5cL || i == n) {
      out[[length(out) + 1L]] <- b1
      i <- i + 1L
      next
    }
    b2 <- rawToChar(raw_in[[i + 1L]])
    i <- i + 2L
    byte <- switch(b2,
      "\\" = 0x5cL,
      "'" = 0x27L,
      "\"" = 0x22L,
      "a" = 0x07L,
      "b" = 0x08L,
      "f" = 0x0cL,
      "n" = 0x0aL,
      "r" = 0x0dL,
      "t" = 0x09L,
      "v" = 0x0bL,
      "x" = {
        hex <- rawToChar(raw_in[seq.int(i, min(n, i + 1L))])
        i <- i + 2L
        strtoi(hex, 16L)
      },
      {
        if (grepl("^[0-7]$", b2)) {
          oct <- b2
          for (k in 1:2) {
            if (i <= n) {
              ch <- rawToChar(raw_in[[i]])
              if (grepl("^[0-7]$", ch)) {
                oct <- paste0(oct, ch)
                i <- i + 1L
                next
              }
            }
            break
          }
          strtoi(oct, 8L) %% 256L
        } else {
          c(0x5cL, utf8ToInt(b2))
        }
      }
    )
    out <- c(out, byte)
  }
  as.raw(out)
}
