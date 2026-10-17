# Python class system: C3 linearization (`mro()`), `super()`, descriptors
# (`py_property`, `py_staticmethod`, `py_classmethod`), attribute access
# (`py_getattr`, `py_setattr`, `py_delattr`, `py_hasattr`), `isinstance` /
# `issubclass`, and context-manager protocol (`py_with`).

# Base `object` class at the root of every Python MRO.
make_base_object_class <- function() {
  cls <- new.env(parent = emptyenv())
  cls$name <- "object"
  cls$bases <- list()
  cls$dict <- list(
    `__init__` = function(self, ...) invisible(NULL),
    `__repr__` = function(self) sprintf("<%s object>", self$`__class__`$name),
    `__str__` = function(self) {
      rfn <- find_in_mro(self$`__class__`$mro, "__repr__")
      rfn$attr(self)
    }
  )
  cls$mro <- list(cls)
  cls$mro_names <- "object"
  class(cls) <- "pyrt_class"
  cls
}

py_object_class <- make_base_object_class()

# C3 superclass linearization algorithm (matches Python's `type.mro()`).
c3_merge <- function(seqs) {
  out <- list()
  repeat {
    seqs <- Filter(function(s) length(s) > 0L, seqs)
    if (length(seqs) == 0L) return(out)

    candidate <- NULL
    for (s in seqs) {
      head_cls <- s[[1L]]
      in_tail <- any(vapply(seqs, function(other) {
        if (length(other) <= 1L) return(FALSE)
        any(vapply(other[-1L], identical, logical(1), y = head_cls))
      }, logical(1)))
      if (!in_tail) {
        candidate <- head_cls
        break
      }
    }
    if (is.null(candidate)) {
      py_raise("TypeError", "Cannot create a consistent method resolution order (MRO)")
    }
    out[[length(out) + 1L]] <- candidate
    seqs <- lapply(seqs, function(s) {
      Filter(function(c) !identical(c, candidate), s)
    })
  }
}

compute_c3_mro <- function(cls, bases) {
  if (length(bases) == 0L) {
    bases <- list(py_object_class)
  }
  seqs <- c(lapply(bases, `[[`, "mro"), list(bases))
  c(list(cls), c3_merge(seqs))
}

find_in_mro <- function(mro, name, start_after = NULL) {
  started <- is.null(start_after)
  for (cls in mro) {
    if (!started) {
      if (identical(cls, start_after)) started <- TRUE
      next
    }
    if (name %in% names(cls$dict)) {
      return(list(found = TRUE, attr = cls$dict[[name]], owner = cls))
    }
  }
  list(found = FALSE, attr = NULL, owner = NULL)
}

bind_descriptor <- function(attr, instance, owner_cls) {
  if (inherits(attr, "pyrt_property")) {
    if (is.null(instance)) return(attr)
    if (is.null(attr$fget)) py_raise("AttributeError", "unreadable attribute")
    return(attr$fget(instance))
  }
  if (inherits(attr, "pyrt_staticmethod")) {
    return(attr$fn)
  }
  if (inherits(attr, "pyrt_classmethod")) {
    callable_cls <- if (is.function(owner_cls)) owner_cls else wrap_callable_class(owner_cls)
    return(function(...) attr$fn(callable_cls, ...))
  }
  if (is.function(attr) && !is.null(instance)) {
    return(function(...) attr(instance, ...))
  }
  attr
}

#' Python class system: C3 MRO, `super()`, descriptors, and `with` context managers
#'
#' `py_class()` defines a Python class with C3 method-resolution order (matching
#' Python's multiple-inheritance rules), dunder hooks (`__init__`, `__call__`,
#' `__repr__`, `__str__`, `__eq__`, `__lt__`, `__len__`, `__getitem__`,
#' `__iter__`, `__enter__`, `__exit__`, etc.), and descriptors (`py_property()`,
#' `py_staticmethod()`, `py_classmethod()`). Calling a `pyrt_class` (or via `$new(...)`)
#' constructs an instance (`pyrt_instance`) with reference semantics.
#' `py_super(cls, self)` returns a proxy that resolves attributes starting after
#' `cls` in `self`'s MRO. `py_with(mgr, body)` implements Python's `with`
#' statement (`__enter__` / `__exit__`, including exception suppression when
#' `__exit__` returns a truthy value).
#'
#' @param name Class name string.
#' @param bases List of base `pyrt_class` objects (defaults to `object`).
#' @param dict Named list of methods, properties, and class attributes.
#' @param fget,fset,fdel Getter, setter, and deleter functions for `py_property()`.
#' @param fn Function wrapped by `py_staticmethod()` or `py_classmethod()`.
#' @param cls,self Current `pyrt_class` and `pyrt_instance` passed to `py_super()`,
#'   or tested by `py_isinstance()` / `py_issubclass()`.
#' @param obj Object whose attribute is accessed/mutated by `py_getattr()`,
#'   `py_setattr()`, `py_delattr()`, or `py_hasattr()`.
#' @param attr Attribute name string.
#' @param default Optional fallback returned by `py_getattr()` when the
#'   attribute does not exist.
#' @param value Value assigned by `py_setattr()`.
#' @param classinfo A `pyrt_class`, class name string, or list / `py_tuple` of
#'   classes for `py_isinstance()` and `py_issubclass()`.
#' @param mgr Context manager object implementing `__enter__` and `__exit__`.
#' @param body Function `function(entered_val) ...` executed inside `py_with()`.
#' @param args,kwargs Positional argument list and keyword argument list/dict
#'   passed to `py_call()`.
#' @returns `py_class()` returns a callable `pyrt_class`; instantiating it
#'   returns a `pyrt_instance`.
#' @export
#' @examples
#' Counter <- py_class("Counter", dict = list(
#'   `__init__` = function(self, start = 0L) {
#'     py_setattr(self, "n", start)
#'   },
#'   inc = function(self, step = 1L) {
#'     py_setattr(self, "n", py_getattr(self, "n") + step)
#'     py_getattr(self, "n")
#'   }
#' ))
#' c1 <- Counter(10L)
#' c1$inc(5L)
py_class <- function(name, bases = list(), dict = list()) {
  # Normalize string base names (e.g. built-in exception names like "ValueError").
  norm_bases <- lapply(bases, function(b) {
    if (inherits(b, "pyrt_class")) b
    else if (is.character(b) && length(b) == 1L) make_builtin_exc_class(b)
    else py_raise("TypeError", "bases must be pyrt_class objects")
  })

  cls <- new.env(parent = emptyenv())
  cls$name <- name
  cls$bases <- norm_bases
  cls$dict <- dict
  cls$mro <- compute_c3_mro(cls, norm_bases)
  cls$mro_names <- vapply(cls$mro, `[[`, character(1), "name")
  class(cls) <- "pyrt_class"

  instantiate <- function(...) {
    inst <- new.env(parent = emptyenv())
    inst$`__class__` <- cls
    inst$`__dict__` <- new.env(hash = TRUE, parent = emptyenv())
    class(inst) <- c(paste0("pyrt_inst_", name), "pyrt_instance")
    init <- find_in_mro(cls$mro, "__init__")
    if (init$found) {
      init$attr(inst, ...)
    }
    inst
  }

  cls$new <- instantiate
  cls
}

make_builtin_exc_class <- function(name) {
  cls <- new.env(parent = emptyenv())
  cls$name <- name
  cls$bases <- list(py_object_class)
  cls$dict <- list(
    `__init__` = function(self, ...) {
      assign("args", py_tuple(list(...)), envir = self$`__dict__`)
      invisible(NULL)
    }
  )
  cls$mro <- list(cls, py_object_class)
  cls$mro_names <- c(exc_mro(name), "object")
  class(cls) <- "pyrt_class"
  cls
}

# Allow calling `MyClass(arg1, arg2)` directly in R via a function wrapper or
# `$new()`. To let `Cls(...)` work with both `$` attribute access and call
# syntax, `py_class()` returns a closure whose environment holds the class
# metadata, and `$` / `[[` dispatch to the class environment.
wrap_callable_class <- function(cls_env) {
  fn <- function(...) cls_env$new(...)
  attr(fn, "cls_env") <- cls_env
  class(fn) <- c("pyrt_class", "function")
  fn
}

unwrap_cls <- function(cls) {
  if (is.function(cls) && !is.null(attr(cls, "cls_env"))) attr(cls, "cls_env") else cls
}

# Re-bind `py_class` to return the callable class object while preserving
# environment access on `$`.
py_class_env <- py_class
#' @rdname py_class
#' @export
py_class <- function(name, bases = list(), dict = list()) {
  bases_env <- lapply(bases, unwrap_cls)
  cls_env <- py_class_env(name, bases = bases_env, dict = dict)
  wrap_callable_class(cls_env)
}

#' @export
`$.pyrt_class` <- function(x, name) {
  env <- unwrap_cls(x)
  if (name %in% c("name", "bases", "dict", "mro", "mro_names", "new")) {
    return(env[[name]])
  }
  hit <- find_in_mro(env$mro, name)
  if (hit$found) return(bind_descriptor(hit$attr, NULL, env))
  py_raise("AttributeError", sprintf("type object '%s' has no attribute '%s'", env$name, name))
}

#' @export
format.pyrt_class <- function(x, ...) {
  env <- unwrap_cls(x)
  sprintf("<class '%s'>", env$name)
}

#' @export
print.pyrt_class <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

# ---- Descriptors: property, staticmethod, classmethod -----------------------

#' @rdname py_class
#' @export
py_property <- function(fget = NULL, fset = NULL, fdel = NULL) {
  prop <- list(fget = fget, fset = fset, fdel = fdel)
  prop$setter <- function(new_fset) py_property(fget = fget, fset = new_fset, fdel = fdel)
  prop$deleter <- function(new_fdel) py_property(fget = fget, fset = fset, fdel = new_fdel)
  class(prop) <- "pyrt_property"
  prop
}

#' @rdname py_class
#' @export
py_staticmethod <- function(fn) {
  structure(list(fn = fn), class = "pyrt_staticmethod")
}

#' @rdname py_class
#' @export
py_classmethod <- function(fn) {
  structure(list(fn = fn), class = "pyrt_classmethod")
}

# ---- Attribute access (`getattr`, `setattr`, `delattr`, `hasattr`, `$`) -----

py_getattr_default <- function(obj, attr, default = NULL) {
  if (inherits(obj, "pyrt_instance")) {
    cls <- unwrap_cls(obj$`__class__`)
    # Data descriptors (`property`) in the class MRO take priority over instance `__dict__`.
    hit <- find_in_mro(cls$mro, attr)
    if (hit$found && inherits(hit$attr, "pyrt_property")) {
      return(bind_descriptor(hit$attr, obj, cls))
    }
    if (exists(attr, envir = obj$`__dict__`, inherits = FALSE)) {
      return(get(attr, envir = obj$`__dict__`, inherits = FALSE))
    }
    if (hit$found) {
      return(bind_descriptor(hit$attr, obj, cls))
    }
    ga <- find_in_mro(cls$mro, "__getattr__")
    if (ga$found) return(ga$attr(obj, attr))
    return(default)
  }
  if (inherits(obj, "pyrt_class")) {
    env <- unwrap_cls(obj)
    hit <- find_in_mro(env$mro, attr)
    if (hit$found) return(bind_descriptor(hit$attr, NULL, env))
    return(default)
  }
  if (inherits(obj, "pyrt_ndarray")) {
    val <- `$.pyrt_ndarray`(obj, attr)
    if (!is.null(val)) return(val)
    return(default)
  }
  if (is.environment(obj) && exists(attr, envir = obj, inherits = FALSE)) {
    return(get(attr, envir = obj, inherits = FALSE))
  }
  default
}

#' @rdname py_class
#' @export
py_getattr <- function(obj, attr, default = missing_sentinel) {
  if (inherits(obj, "pyrt_instance")) {
    cls <- unwrap_cls(obj$`__class__`)
    if (identical(attr, "__class__")) return(obj$`__class__`)
    if (identical(attr, "__dict__")) return(obj$`__dict__`)
    hit <- find_in_mro(cls$mro, attr)
    if (hit$found && inherits(hit$attr, "pyrt_property")) {
      return(bind_descriptor(hit$attr, obj, cls))
    }
    if (exists(attr, envir = obj$`__dict__`, inherits = FALSE)) {
      return(get(attr, envir = obj$`__dict__`, inherits = FALSE))
    }
    if (hit$found) {
      return(bind_descriptor(hit$attr, obj, cls))
    }
    ga <- find_in_mro(cls$mro, "__getattr__")
    if (ga$found) return(ga$attr(obj, attr))
    if (!inherits(default, "pyrt_missing")) return(default)
    py_raise("AttributeError", sprintf("'%s' object has no attribute '%s'", cls$name, attr))
  }
  if (inherits(obj, "pyrt_class")) {
    env <- unwrap_cls(obj)
    if (attr %in% c("name", "bases", "dict", "mro", "mro_names", "new")) return(env[[attr]])
    hit <- find_in_mro(env$mro, attr)
    if (hit$found) return(bind_descriptor(hit$attr, NULL, env))
    if (!inherits(default, "pyrt_missing")) return(default)
    py_raise("AttributeError", sprintf("type object '%s' has no attribute '%s'", env$name, attr))
  }
  if (inherits(obj, "pyrt_super")) {
    return(`$.pyrt_super`(obj, attr))
  }
  if (inherits(obj, "pyrt_ndarray")) {
    val <- `$.pyrt_ndarray`(obj, attr)
    if (!is.null(val)) return(val)
    if (!inherits(default, "pyrt_missing")) return(default)
    py_raise("AttributeError", sprintf("'numpy.ndarray' object has no attribute '%s'", attr))
  }
  if (is.character(obj) && length(obj) == 1L) {
    return(py_str_method(obj, attr))
  }
  if (is_int_like(obj) && identical(attr, "bit_length")) {
    return(function() {
      z <- abs(as_bigz_exact(obj))
      if (all(z == 0L)) 0L else as.integer(gmp::sizeinbase(z, 2L))
    })
  }
  if (is.environment(obj)) {
    if (exists(attr, envir = obj, inherits = FALSE)) {
      return(get(attr, envir = obj, inherits = FALSE))
    }
  } else if (is.list(obj) && attr %in% names(obj)) {
    return(obj[[attr]])
  } else if (is.function(obj) || inherits(obj, "pyrt_callable_obj")) {
    a_val <- base::attr(obj, attr, exact = TRUE)
    if (!is.null(a_val)) return(a_val)
  }
  if (!inherits(default, "pyrt_missing")) return(default)
  py_raise("AttributeError", sprintf("object has no attribute '%s'", attr))
}

#' @rdname py_class
#' @export
py_setattr <- function(obj, attr, value) {
  if (inherits(obj, "pyrt_instance")) {
    cls <- unwrap_cls(obj$`__class__`)
    hit <- find_in_mro(cls$mro, attr)
    if (hit$found && inherits(hit$attr, "pyrt_property")) {
      if (is.null(hit$attr$fset)) py_raise("AttributeError", sprintf("property '%s' of '%s' object has no setter", attr, cls$name))
      hit$attr$fset(obj, value)
      return(invisible(obj))
    }
    assign(attr, value, envir = obj$`__dict__`)
    return(invisible(obj))
  }
  if (inherits(obj, "pyrt_class")) {
    env <- unwrap_cls(obj)
    env$dict[[attr]] <- value
    return(invisible(obj))
  }
  if (is.environment(obj)) {
    assign(attr, value, envir = obj)
    return(invisible(obj))
  }
  py_raise("AttributeError", "cannot set attribute on immutable object")
}

#' @rdname py_class
#' @export
py_delattr <- function(obj, attr) {
  if (inherits(obj, "pyrt_instance")) {
    cls <- unwrap_cls(obj$`__class__`)
    hit <- find_in_mro(cls$mro, attr)
    if (hit$found && inherits(hit$attr, "pyrt_property")) {
      if (is.null(hit$attr$fdel)) py_raise("AttributeError", sprintf("property '%s' of '%s' object has no deleter", attr, cls$name))
      hit$attr$fdel(obj)
      return(invisible(obj))
    }
    if (exists(attr, envir = obj$`__dict__`, inherits = FALSE)) {
      rm(list = attr, envir = obj$`__dict__`)
      return(invisible(obj))
    }
    py_raise("AttributeError", sprintf("'%s' object has no attribute '%s'", cls$name, attr))
  }
  py_raise("AttributeError", "cannot delete attribute")
}

#' @rdname py_class
#' @export
py_hasattr <- function(obj, attr) {
  py_try(
    {
      py_getattr(obj, attr)
      TRUE
    },
    handlers = list(AttributeError = function(e) FALSE)
  )
}

#' @export
`$.pyrt_instance` <- function(x, name) {
  if (name %in% c("__class__", "__dict__")) {
    return(get(name, envir = x, inherits = FALSE))
  }
  py_getattr(x, name)
}

#' @export
`$<-.pyrt_instance` <- function(x, name, value) {
  py_setattr(x, name, value)
  x
}

#' @export
format.pyrt_instance <- function(x, ...) {
  py_str(x)
}

#' @export
print.pyrt_instance <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

# ---- `super(cls, self)` proxy -----------------------------------------------

#' @rdname py_class
#' @export
py_super <- function(cls, self) {
  cls_env <- unwrap_cls(cls)
  inst_cls <- unwrap_cls(self$`__class__`)
  sup <- new.env(parent = emptyenv())
  sup$.cls <- cls_env
  sup$.self <- self
  sup$.mro <- inst_cls$mro
  class(sup) <- "pyrt_super"
  sup
}

#' @export
`$.pyrt_super` <- function(x, name) {
  mro <- get(".mro", envir = x, inherits = FALSE)
  cls <- get(".cls", envir = x, inherits = FALSE)
  self_obj <- get(".self", envir = x, inherits = FALSE)
  hit <- find_in_mro(mro, name, start_after = cls)
  if (!hit$found) {
    py_raise("AttributeError", sprintf("'super' object has no attribute '%s'", name))
  }
  bind_descriptor(hit$attr, self_obj, hit$owner)
}

# ---- `isinstance`, `issubclass`, and `py_call` ------------------------------

#' @rdname py_class
#' @export
py_isinstance <- function(obj, classinfo) {
  if (inherits(classinfo, "pyrt_tuple")) classinfo <- classinfo$items
  if (is.list(classinfo)) {
    return(any(vapply(classinfo, function(ci) py_isinstance(obj, ci), logical(1))))
  }
  if (inherits(classinfo, "pyrt_class")) {
    target_env <- unwrap_cls(classinfo)
    if (inherits(obj, "pyrt_instance")) {
      inst_cls <- unwrap_cls(obj$`__class__`)
      return(any(vapply(inst_cls$mro, identical, logical(1), y = target_env)) ||
             (target_env$name %in% inst_cls$mro_names))
    }
    if (inherits(obj, "pyrt_exception")) {
      return(target_env$name %in% class(obj))
    }
    return(FALSE)
  }
  if (is.character(classinfo) && length(classinfo) == 1L) {
    return(switch(classinfo,
      bool = is.logical(obj) && length(obj) == 1L,
      int = is_int_like(obj) || (is.logical(obj) && length(obj) == 1L),
      float = is.double(obj) && length(obj) == 1L,
      complex = is.complex(obj) && length(obj) == 1L,
      str = is.character(obj) && length(obj) == 1L,
      bytes = is.raw(obj),
      list = inherits(obj, "pyrt_list"),
      tuple = inherits(obj, "pyrt_tuple"),
      dict = inherits(obj, "pyrt_dict"),
      set = inherits(obj, "pyrt_set"),
      object = TRUE,
      {
        if (inherits(obj, "pyrt_instance")) {
          classinfo %in% unwrap_cls(obj$`__class__`)$mro_names
        } else if (inherits(obj, "pyrt_exception")) {
          classinfo %in% class(obj)
        } else {
          inherits(obj, classinfo)
        }
      }
    ))
  }
  FALSE
}

#' @rdname py_class
#' @export
py_issubclass <- function(cls, classinfo) {
  if (inherits(classinfo, "pyrt_tuple")) classinfo <- classinfo$items
  if (is.list(classinfo)) {
    return(any(vapply(classinfo, function(ci) py_issubclass(cls, ci), logical(1))))
  }
  cls_env <- unwrap_cls(cls)
  if (inherits(classinfo, "pyrt_class")) {
    target_env <- unwrap_cls(classinfo)
    return(any(vapply(cls_env$mro, identical, logical(1), y = target_env)) ||
           (target_env$name %in% cls_env$mro_names))
  }
  if (is.character(classinfo) && length(classinfo) == 1L) {
    return(classinfo %in% cls_env$mro_names)
  }
  FALSE
}

#' @rdname py_class
#' @export
py_call <- function(obj, args = list(), kwargs = list()) {
  pos <- if (inherits(args, "pyrt_list") || inherits(args, "pyrt_tuple")) args$as_r_list() else as.list(args)
  kw <- if (inherits(kwargs, "pyrt_dict")) {
    stats::setNames(kwargs$.vals, vapply(kwargs$.keys, as.character, character(1)))
  } else {
    as.list(kwargs)
  }
  all_args <- c(pos, kw)
  if (inherits(obj, "pyrt_instance")) {
    cfn <- py_getattr_default(obj, "__call__", NULL)
    if (is.null(cfn)) py_raise("TypeError", sprintf("'%s' object is not callable", unwrap_cls(obj$`__class__`)$name))
    return(do.call(cfn, all_args))
  }
  do.call(obj, all_args)
}

# ---- Context manager (`with mgr as v:`) -------------------------------------

#' @rdname py_class
#' @export
py_with <- function(mgr, body) {
  enter_fn <- py_getattr(mgr, "__enter__")
  exit_fn <- py_getattr(mgr, "__exit__")
  val <- enter_fn()
  exited <- FALSE
  on.exit({
    if (!exited) exit_fn(NULL, NULL, NULL)
  }, add = TRUE)

  raised <- NULL
  res <- tryCatch(
    if (length(formals(body)) == 0L) body() else body(val),
    error = function(e) {
      raised <<- e
      NULL
    }
  )
  exited <- TRUE
  if (!is.null(raised)) {
    exc_type <- if (!is.null(raised$type)) raised$type else class(raised)[[1L]]
    suppressed <- py_bool(exit_fn(exc_type, raised, NULL))
    if (!suppressed) stop(raised)
    return(invisible(NULL))
  }
  exit_fn(NULL, NULL, NULL)
  res
}
