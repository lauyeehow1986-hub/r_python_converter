#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <errno.h>
#include <stdlib.h>

#include "tree_sitter/parser.h"

// Defined in the vendored `parser.c`.
extern const TSLanguage* tree_sitter_python(void);

// External pointer to the static `TSLanguage` for Python. The pointer is
// wrapped into a `tree_sitter_language` object on the R side.
SEXP ffi_language(void) {
  static SEXP language = NULL;

  if (language == NULL) {
    const TSLanguage* pointer = tree_sitter_python();
    language = R_MakeExternalPtr((void*) pointer, R_NilValue, R_NilValue);
    R_PreserveObject(language);
  }

  return language;
}

// Correctly rounded decimal -> double conversion using the C library's
// `strtod()`, so float literals match CPython bit-for-bit. Input must already
// be stripped of `_` separators. Returns NA for unparsable input.
SEXP ffi_strtod(SEXP x) {
  if (TYPEOF(x) != STRSXP) {
    Rf_error("`x` must be a character vector.");
  }

  R_xlen_t n = Rf_xlength(x);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double* p_out = REAL(out);

  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP elt = STRING_ELT(x, i);
    if (elt == NA_STRING) {
      p_out[i] = NA_REAL;
      continue;
    }
    const char* s = CHAR(elt);
    char* end = NULL;
    errno = 0;
    double value = strtod(s, &end);
    // ERANGE on overflow gives +/-HUGE_VAL (= inf), which matches CPython's
    // behaviour for literals like 1e999. Underflow to 0 also matches.
    p_out[i] = (end == s || *end != '\0') ? NA_REAL : value;
  }

  UNPROTECT(1);
  return out;
}
