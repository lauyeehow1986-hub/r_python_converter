#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>  // for NULL

extern SEXP ffi_language(void);
extern SEXP ffi_strtod(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_language", (DL_FUNC) &ffi_language, 0},
    {"ffi_strtod", (DL_FUNC) &ffi_strtod, 1},
    {NULL, NULL, 0}};

void R_init_pyport(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
