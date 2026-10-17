# Python `str()`, `repr()`, `ascii()`, `format()` (Format Specification
# Mini-Language), `%` formatting, and `str` methods.

#' Python `str()`, `repr()`, `ascii()`, `format()`, and `str` methods
#'
#' `py_str()`, `py_repr()`, and `py_ascii()` format values using Python's
#' string and representation rules (dispatching to `__str__` / `__repr__` on
#' custom instances). `py_format(value, spec)` implements Python's Format
#' Specification Mini-Language (`[[fill]align][sign][#][0][width][grouping_option][.precision][type]`,
#' used by f-strings and `format()`). `py_fstring(...)` evaluates an f-string's
#' parts. `py_str_method(s, method)` dispatches Python `str` methods (`split`,
#' `rsplit`, `splitlines`, `join`, `strip`, `lstrip`, `rstrip`, `replace`,
#' `find`, `rfind`, `index`, `rindex`, `count`, `startswith`, `endswith`,
#' `upper`, `lower`, `capitalize`, `title`, `swapcase`, `zfill`, `center`,
#' `ljust`, `rjust`, `partition`, `rpartition`, `removeprefix`, `removesuffix`,
#' `isdigit`, `isalpha`, `isalnum`, `isspace`, `isupper`, `islower`, `format`).
#'
#' @param x,value Value to convert or format.
#' @param spec Format specification string (e.g. `">10.2f"`, `"08x"`, `",d"`,
#'   `"+.3e"`, `"^12s"`).
#' @param conversion Integer conversion code for f-string `FormattedValue`
#'   (`-1L` = none, `115L` = `!s`, `114L` = `!r`, `97L` = `!a`).
#' @param ... Parts passed to `py_fstring()`.
#' @param s A single character string for `py_str_method()`.
#' @param method Name of the Python `str` method.
#' @returns A character string (or container / logical / integer for `str`
#'   methods that return lists, tuples, booleans, or indices).
#' @export
#' @examples
#' py_format(3.14159, ">+10.2f")
#' py_format(255L, "#06x")
#' py_format(1234567L, ",d")
#' py_str_method("  hello, world  ", "strip")()
py_str <- function(x = "") {
  if (is.null(x)) return("None")
  if (inherits(x, "pyrt_ellipsis")) return("Ellipsis")
  if (inherits(x, "pyrt_exception")) {
    return(if (is.null(x$value)) "" else as.character(x$value))
  }
  if (inherits(x, "pyrt_instance")) {
    fn <- py_getattr_default(x, "__str__", NULL)
    if (!is.null(fn)) return(as.character(fn()))
    return(py_repr(x))
  }
  if (inherits(x, "pyrt_container") || inherits(x, "pyrt_range") || inherits(x, "pyrt_class")) {
    return(format(x))
  }
  if (is.logical(x) && length(x) == 1L) {
    return(if (is.na(x)) "None" else if (x) "True" else "False")
  }
  if (is_int_like(x)) {
    return(as.character(x))
  }
  if (is.double(x) && length(x) == 1L) {
    return(fmt_py_float(x))
  }
  if (is.complex(x) && length(x) == 1L) {
    re <- Re(x)
    im <- Im(x)
    im_s <- if (is.finite(im) && im == trunc(im) && abs(im) < 1e16) sprintf("%.0fj", im) else paste0(fmt_py_float(im), "j")
    if (re == 0) return(im_s)
    sign_ch <- if (im >= 0) "+" else ""
    return(paste0("(", fmt_py_float(re), sign_ch, im_s, ")"))
  }
  if (is.raw(x)) {
    return(fmt_py_bytes(x))
  }
  if (is.character(x) && length(x) == 1L) {
    return(x)
  }
  paste(format(x), collapse = " ")
}

#' @rdname py_str
#' @export
py_repr <- function(x) {
  if (is.null(x)) return("None")
  if (inherits(x, "pyrt_ellipsis")) return("Ellipsis")
  if (inherits(x, "pyrt_instance")) {
    fn <- py_getattr_default(x, "__repr__", NULL)
    if (!is.null(fn)) return(as.character(fn()))
  }
  if (is.character(x) && length(x) == 1L) {
    return(quote_py_str(x, ascii_only = FALSE))
  }
  py_str(x)
}

#' @rdname py_str
#' @export
py_ascii <- function(x) {
  s <- py_repr(x)
  cps <- utf8ToInt(enc2utf8(s))
  if (all(cps <= 127L)) return(s)
  chars <- vapply(cps, function(cp) {
    if (cp <= 127L) intToUtf8(cp)
    else if (cp <= 0xffL) sprintf("\\x%02x", cp)
    else if (cp <= 0xffffL) sprintf("\\u%04x", cp)
    else sprintf("\\U%08x", cp)
  }, character(1))
  paste0(chars, collapse = "")
}

fmt_py_float <- function(x) {
  if (is.nan(x)) return("nan")
  if (is.infinite(x)) return(if (x > 0) "inf" else "-inf")
  if (x == 0) return(if (1 / x < 0) "-0.0" else "0.0")
  s <- format(x, digits = 15, scientific = NA, trim = TRUE)
  if (!grepl("[.eE]", s)) paste0(s, ".0") else s
}

quote_py_str <- function(s, ascii_only = FALSE) {
  s <- enc2utf8(s)
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

fmt_py_bytes <- function(b) {
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

# ---- Format Specification Mini-Language (`format(value, spec)`) -------------

spec_re <- "^(?:(.?([<>=^]))?([+\\- ])?(#)?(0)?([0-9]+)?([,_])?(?:\\.([0-9]+))?([bcdeEfFgGnosxX%])?)$"

#' @rdname py_str
#' @export
py_format <- function(value, spec = "", conversion = -1L) {
  if (!is.null(conversion) && length(conversion) == 1L) {
    if (identical(conversion, 115L) || identical(conversion, "s")) value <- py_str(value)
    else if (identical(conversion, 114L) || identical(conversion, "r")) value <- py_repr(value)
    else if (identical(conversion, 97L) || identical(conversion, "a")) value <- py_ascii(value)
  }

  if (!nzchar(spec)) {
    if (inherits(value, "pyrt_instance")) {
      ffn <- py_getattr_default(value, "__format__", NULL)
      if (!is.null(ffn)) return(as.character(ffn("")))
    }
    return(py_str(value))
  }
  if (inherits(value, "pyrt_instance")) {
    ffn <- py_getattr_default(value, "__format__", NULL)
    if (!is.null(ffn)) return(as.character(ffn(spec)))
  }

  m <- regmatches(spec, regexec(spec_re, spec, perl = TRUE))[[1L]]
  if (length(m) == 0L) {
    py_raise("ValueError", sprintf("Invalid format specifier '%s'", spec))
  }
  fill_align <- m[[2L]]
  align <- m[[3L]]
  fill <- if (nchar(fill_align) == 2L) substr(fill_align, 1L, 1L) else " "
  sign_opt <- m[[4L]]
  alt_form <- nzchar(m[[5L]])
  zero_pad <- nzchar(m[[6L]])
  width <- if (nzchar(m[[7L]])) as.integer(m[[7L]]) else 0L
  group_ch <- m[[8L]]
  prec <- if (nzchar(m[[9L]])) as.integer(m[[9L]]) else NA_integer_
  ftype <- m[[10L]]

  if (zero_pad && !nzchar(align)) {
    fill <- "0"
    align <- "="
  }

  is_str <- is.character(value) && length(value) == 1L && (ftype %in% c("", "s"))
  if (is_str) {
    body <- value
    if (!is.na(prec) && nchar(body) > prec) {
      body <- substr(body, 1L, prec)
    }
    if (!nzchar(align)) align <- "<"
    return(apply_padding(body, "", width, align, fill))
  }

  # Numeric formatting
  if (!nzchar(align)) align <- ">"
  if (ftype %in% c("b", "o", "x", "X", "d", "c") ||
      (!nzchar(ftype) && (is_int_like(value) || is.logical(value)))) {
    if (!nzchar(ftype)) ftype <- "d"
    z <- as_bigz_exact(value)
    if (ftype == "c") {
      return(apply_padding(intToUtf8(as.integer(z)), "", width, align, fill))
    }
    neg <- (z < 0L)
    az <- abs(z)
    digits <- switch(ftype,
      d = as.character(az),
      b = gmp_to_base2(az),
      o = as.character(az, b = 8L),
      x = tolower(as.character(az, b = 16L)),
      X = toupper(as.character(az, b = 16L))
    )
    if (nzchar(group_ch)) {
      digits <- insert_grouping(digits, group_ch, ifelse(ftype %in% c("b", "x", "X"), 4L, 3L))
    }
    pfx <- if (neg) "-" else if (sign_opt == "+") "+" else if (sign_opt == " ") " " else ""
    if (alt_form && ftype %in% c("b", "o", "x", "X")) {
      pfx <- paste0(pfx, "0", ftype)
    }
    return(apply_padding(digits, pfx, width, align, fill))
  }

  # Floating-point formatting
  num <- as.double(value)
  neg <- (!is.nan(num)) && (num < 0 || (num == 0 && (1 / num < 0)))
  anum <- abs(num)
  p <- if (is.na(prec)) 6L else prec
  if (!nzchar(ftype)) ftype <- "g"

  body <- if (ftype == "%") {
    paste0(sprintf(paste0("%.", p, "f"), anum * 100), "%")
  } else if (ftype %in% c("f", "F", "e", "E", "g", "G")) {
    s <- sprintf(paste0("%", if (alt_form) "#" else "", ".", p, ftype), anum)
    # Normalize Windows/MinGW 3-digit exponent `e+005` -> `e+05` to match Python.
    sub("([eE][+-])0([0-9]{2})$", "\\1\\2", s)
  } else {
    fmt_py_float(anum)
  }
  if (nzchar(group_ch)) {
    parts <- strsplit(body, ".", fixed = TRUE)[[1L]]
    parts[[1L]] <- insert_grouping(parts[[1L]], group_ch, 3L)
    body <- paste(parts, collapse = ".")
  }
  pfx <- if (neg) "-" else if (sign_opt == "+") "+" else if (sign_opt == " ") " " else ""
  apply_padding(body, pfx, width, align, fill)
}

gmp_to_base2 <- function(az) {
  if (all(az == 0L)) return("0")
  as.character(az, b = 2L)
}

insert_grouping <- function(digits, sep, group_size = 3L) {
  n <- nchar(digits)
  if (n <= group_size) return(digits)
  rem <- n %% group_size
  starts <- if (rem == 0L) seq.int(1L, n, by = group_size) else c(1L, seq.int(rem + 1L, n, by = group_size))
  ends <- c(starts[-1L] - 1L, n)
  paste(substring(digits, starts, ends), collapse = sep)
}

apply_padding <- function(body, prefix, width, align, fill) {
  total_len <- nchar(prefix) + nchar(body)
  if (width <= total_len) return(paste0(prefix, body))
  pad_n <- width - total_len
  pad <- strrep(fill, pad_n)
  switch(align,
    "<" = paste0(prefix, body, pad),
    ">" = paste0(pad, prefix, body),
    "=" = paste0(prefix, pad, body),
    "^" = {
      left_n <- pad_n %/% 2L
      right_n <- pad_n - left_n
      paste0(strrep(fill, left_n), prefix, body, strrep(fill, right_n))
    }
  )
}

#' @rdname py_str
#' @export
py_fstring <- function(...) {
  parts <- list(...)
  if (length(parts) == 0L) return("")
  paste0(vapply(parts, py_str, character(1)), collapse = "")
}

# Classic `"fmt %s %d" % args` formatting.
py_percent_format <- function(fmt, args) {
  vals <- if (inherits(args, "pyrt_tuple")) args$items else list(args)
  do.call(sprintf, c(list(fmt), lapply(vals, function(v) {
    if (gmp::is.bigz(v)) as.character(v) else v
  })))
}

# ---- Python `str` methods ---------------------------------------------------

#' @rdname py_str
#' @export
py_str_method <- function(s, method) {
  if (!is.character(s) || length(s) != 1L) {
    py_raise("TypeError", "py_str_method expects a single character string")
  }
  switch(method,
    upper = function() toupper(s),
    lower = function() tolower(s),
    capitalize = function() {
      if (!nzchar(s)) return("")
      paste0(toupper(substr(s, 1L, 1L)), tolower(substring(s, 2L)))
    },
    title = function() {
      gsub("(\\b[a-zA-Z])([a-zA-Z]*)", "\\U\\1\\L\\2", s, perl = TRUE)
    },
    swapcase = function() chartr("a-zA-Z", "A-Za-z", s),
    strip = function(chars = NULL) str_strip_impl(s, chars, "both"),
    lstrip = function(chars = NULL) str_strip_impl(s, chars, "left"),
    rstrip = function(chars = NULL) str_strip_impl(s, chars, "right"),
    startswith = function(prefix, start = 0L, end = NULL) {
      sub_s <- py_getitem(s, py_slice(start, end))
      if (inherits(prefix, "pyrt_tuple")) {
        any(vapply(prefix$items, function(p) startsWith(sub_s, p), logical(1)))
      } else {
        startsWith(sub_s, prefix)
      }
    },
    endswith = function(suffix, start = 0L, end = NULL) {
      sub_s <- py_getitem(s, py_slice(start, end))
      if (inherits(suffix, "pyrt_tuple")) {
        any(vapply(suffix$items, function(p) endsWith(sub_s, p), logical(1)))
      } else {
        endsWith(sub_s, suffix)
      }
    },
    removeprefix = function(prefix) {
      if (startsWith(s, prefix)) substring(s, nchar(prefix) + 1L) else s
    },
    removesuffix = function(suffix) {
      if (nzchar(suffix) && endsWith(s, suffix)) substr(s, 1L, nchar(s) - nchar(suffix)) else s
    },
    replace = function(old, new, count = -1L) {
      count <- as.integer(count)
      if (count == 0L) return(s)
      if (count < 0L) return(gsub(old, new, s, fixed = TRUE))
      out <- s
      for (i in seq_len(count)) {
        pos <- regexpr(old, out, fixed = TRUE)
        if (pos < 0L) break
        out <- paste0(
          substr(out, 1L, pos - 1L),
          new,
          substring(out, pos + nchar(old))
        )
      }
      out
    },
    split = function(sep = NULL, maxsplit = -1L) {
      str_split_impl(s, sep, as.integer(maxsplit), from_right = FALSE)
    },
    rsplit = function(sep = NULL, maxsplit = -1L) {
      str_split_impl(s, sep, as.integer(maxsplit), from_right = TRUE)
    },
    splitlines = function(keepends = FALSE) {
      if (!nzchar(s)) return(py_list())
      m <- gregexpr("[^\r\n]*(\r\n|\r|\n)?", s, perl = TRUE)[[1L]]
      parts <- regmatches(s, list(m))[[1L]]
      if (length(parts) > 0L && !nzchar(parts[[length(parts)]])) {
        parts <- parts[-length(parts)]
      }
      if (!isTRUE(keepends)) {
        parts <- sub("(\r\n|\r|\n)$", "", parts, perl = TRUE)
      }
      py_list(as.list(parts))
    },
    join = function(iterable) {
      items <- coerce_items(iterable)
      strs <- vapply(items, function(el) {
        if (!is.character(el) || length(el) != 1L) {
          py_raise("TypeError", "sequence item: expected str instance")
        }
        el
      }, character(1))
      paste0(strs, collapse = s)
    },
    find = function(sub, start = 0L, end = NULL) {
      str_find_impl(s, sub, start, end, from_right = FALSE, raise = FALSE)
    },
    rfind = function(sub, start = 0L, end = NULL) {
      str_find_impl(s, sub, start, end, from_right = TRUE, raise = FALSE)
    },
    index = function(sub, start = 0L, end = NULL) {
      str_find_impl(s, sub, start, end, from_right = FALSE, raise = TRUE)
    },
    rindex = function(sub, start = 0L, end = NULL) {
      str_find_impl(s, sub, start, end, from_right = TRUE, raise = TRUE)
    },
    count = function(sub, start = 0L, end = NULL) {
      sub_s <- py_getitem(s, py_slice(start, end))
      if (!nzchar(sub)) return(nchar(sub_s) + 1L)
      m <- gregexpr(sub, sub_s, fixed = TRUE)[[1L]]
      if (m[[1L]] < 0L) 0L else length(m)
    },
    partition = function(sep) {
      if (!nzchar(sep)) py_raise("ValueError", "empty separator")
      pos <- regexpr(sep, s, fixed = TRUE)
      if (pos < 0L) return(py_tuple(list(s, "", "")))
      py_tuple(list(
        substr(s, 1L, pos - 1L),
        sep,
        substring(s, pos + nchar(sep))
      ))
    },
    rpartition = function(sep) {
      if (!nzchar(sep)) py_raise("ValueError", "empty separator")
      pos <- str_find_impl(s, sep, 0L, NULL, from_right = TRUE, raise = FALSE)
      if (pos < 0L) return(py_tuple(list("", "", s)))
      py_tuple(list(
        substr(s, 1L, pos),
        sep,
        substring(s, pos + 1L + nchar(sep))
      ))
    },
    zfill = function(width) {
      w <- as.integer(width)
      if (nchar(s) >= w) return(s)
      if (startsWith(s, "-") || startsWith(s, "+")) {
        paste0(substr(s, 1L, 1L), strrep("0", w - nchar(s)), substring(s, 2L))
      } else {
        paste0(strrep("0", w - nchar(s)), s)
      }
    },
    ljust = function(width, fillchar = " ") {
      apply_padding(s, "", as.integer(width), "<", fillchar)
    },
    rjust = function(width, fillchar = " ") {
      apply_padding(s, "", as.integer(width), ">", fillchar)
    },
    center = function(width, fillchar = " ") {
      apply_padding(s, "", as.integer(width), "^", fillchar)
    },
    isdigit = function() nzchar(s) && grepl("^[0-9]+$", s),
    isalpha = function() nzchar(s) && grepl("^[[:alpha:]]+$", s),
    isalnum = function() nzchar(s) && grepl("^[[:alnum:]]+$", s),
    isspace = function() nzchar(s) && grepl("^[[:space:]]+$", s),
    isupper = function() grepl("[[:upper:]]", s) && !grepl("[[:lower:]]", s),
    islower = function() grepl("[[:lower:]]", s) && !grepl("[[:upper:]]", s),
    format = function(...) {
      str_brace_format(s, list(...))
    },
    py_raise("AttributeError", sprintf("'str' object has no attribute '%s'", method))
  )
}

str_strip_impl <- function(s, chars, which) {
  if (is.null(chars)) {
    return(trimws(s, which = which))
  }
  esc <- gsub("([\\^\\-\\]\\\\])", "\\\\\\1", chars, perl = TRUE)
  cls <- paste0("[", esc, "]+")
  if (which %in% c("left", "both")) s <- sub(paste0("^", cls), "", s, perl = TRUE)
  if (which %in% c("right", "both")) s <- sub(paste0(cls, "$"), "", s, perl = TRUE)
  s
}

str_split_impl <- function(s, sep, maxsplit, from_right) {
  if (is.null(sep)) {
    s_trim <- trimws(s)
    if (!nzchar(s_trim)) return(py_list())
    pieces <- strsplit(s_trim, "[[:space:]]+", perl = TRUE)[[1L]]
    if (maxsplit < 0L || length(pieces) <= maxsplit + 1L) {
      return(py_list(as.list(pieces)))
    }
    # Re-join excess splits using original whitespace runs.
    m <- gregexpr("[^[:space:]]+", s, perl = TRUE)[[1L]]
    lens <- attr(m, "match.length")
    n <- length(m)
    if (!from_right) {
      head_p <- pieces[seq_len(maxsplit)]
      rest <- substring(s, m[[maxsplit + 1L]], m[[n]] + lens[[n]] - 1L)
      return(py_list(as.list(c(head_p, rest))))
    } else {
      tail_p <- pieces[seq.int(n - maxsplit + 1L, n)]
      rest <- substr(s, m[[1L]], m[[n - maxsplit]] + lens[[n - maxsplit]] - 1L)
      return(py_list(as.list(c(rest, tail_p))))
    }
  }
  if (!nzchar(sep)) py_raise("ValueError", "empty separator")
  m <- gregexpr(sep, s, fixed = TRUE)[[1L]]
  if (m[[1L]] < 0L || maxsplit == 0L) return(py_list(list(s)))
  sep_len <- nchar(sep)
  if (maxsplit > 0L && length(m) > maxsplit) {
    m <- if (!from_right) m[seq_len(maxsplit)] else m[seq.int(length(m) - maxsplit + 1L, length(m))]
  }
  starts <- c(1L, m + sep_len)
  ends <- c(m - 1L, nchar(s))
  py_list(as.list(substring(s, starts, ends)))
}

str_find_impl <- function(s, sub, start, end, from_right, raise) {
  n <- nchar(s)
  st <- clamp_slice_bound(as.integer(start), n)
  en <- if (is.null(end)) n else clamp_slice_bound(as.integer(end), n)
  if (en < st) {
    if (raise) py_raise("ValueError", "substring not found")
    return(-1L)
  }
  sub_s <- substr(s, st + 1L, en)
  if (!nzchar(sub)) {
    return(if (from_right) en else st)
  }
  m <- gregexpr(sub, sub_s, fixed = TRUE)[[1L]]
  if (m[[1L]] < 0L) {
    if (raise) py_raise("ValueError", "substring not found")
    return(-1L)
  }
  hit <- if (from_right) m[[length(m)]] else m[[1L]]
  st + hit - 1L
}

str_brace_format <- function(tmpl, args) {
  # Handle escaped `{{` and `}}`, positional `{}`, `{0}`, and `{name:spec}`.
  s <- gsub("{{", "\x01", tmpl, fixed = TRUE)
  s <- gsub("}}", "\x02", s, fixed = TRUE)
  auto_idx <- 0L
  nms <- names(args)
  m <- gregexpr("\\{[^{}]*\\}", s, perl = TRUE)[[1L]]
  if (m[[1L]] > 0L) {
    tokens <- regmatches(s, list(m))[[1L]]
    repls <- vapply(tokens, function(tok) {
      inner <- substr(tok, 2L, nchar(tok) - 1L)
      sp <- strsplit(inner, ":", fixed = TRUE)[[1L]]
      field <- sp[[1L]]
      spec <- if (length(sp) >= 2L) paste(sp[-1L], collapse = ":") else ""
      conv <- -1L
      if (grepl("![sra]$", field)) {
        conv <- utf8ToInt(substr(field, nchar(field), nchar(field)))
        field <- substr(field, 1L, nchar(field) - 2L)
      }
      val <- if (!nzchar(field)) {
        auto_idx <<- auto_idx + 1L
        args[[auto_idx]]
      } else if (grepl("^[0-9]+$", field)) {
        args[[as.integer(field) + 1L]]
      } else if (!is.null(nms) && field %in% nms) {
        args[[field]]
      } else {
        py_raise("KeyError", field)
      }
      py_format(val, spec = spec, conversion = conv)
    }, character(1))
    regmatches(s, list(m)) <- list(repls)
  }
  s <- gsub("\x01", "{", s, fixed = TRUE)
  gsub("\x02", "}", s, fixed = TRUE)
}
