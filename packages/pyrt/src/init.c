#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdlib.h>

extern SEXP ffi_mt_new(void);
extern SEXP ffi_mt_seed_words(SEXP, SEXP);
extern SEXP ffi_mt_seed_bytes(SEXP, SEXP);
extern SEXP ffi_mt_random(SEXP, SEXP);
extern SEXP ffi_mt_uint32(SEXP);
extern SEXP ffi_mt_getrandbits_words(SEXP, SEXP);
extern SEXP ffi_mt_randbelow(SEXP, SEXP);
extern SEXP ffi_mt_shuffle_perm(SEXP, SEXP);
extern SEXP ffi_mt_gauss(SEXP, SEXP, SEXP);
extern SEXP ffi_mt_getstate(SEXP);
extern SEXP ffi_mt_setstate(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP ffi_mt_seed_uint32(SEXP, SEXP);
extern SEXP ffi_mt_np_randn(SEXP, SEXP, SEXP, SEXP);
extern SEXP ffi_mt_np_randint(SEXP, SEXP, SEXP, SEXP);
extern SEXP ffi_pcg64_new(void);
extern SEXP ffi_pcg64_seed(SEXP, SEXP);
extern SEXP ffi_pcg64_random(SEXP, SEXP);
extern SEXP ffi_pcg64_integers(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"ffi_mt_new", (DL_FUNC) &ffi_mt_new, 0},
    {"ffi_mt_seed_words", (DL_FUNC) &ffi_mt_seed_words, 2},
    {"ffi_mt_seed_bytes", (DL_FUNC) &ffi_mt_seed_bytes, 2},
    {"ffi_mt_random", (DL_FUNC) &ffi_mt_random, 2},
    {"ffi_mt_uint32", (DL_FUNC) &ffi_mt_uint32, 1},
    {"ffi_mt_getrandbits_words", (DL_FUNC) &ffi_mt_getrandbits_words, 2},
    {"ffi_mt_randbelow", (DL_FUNC) &ffi_mt_randbelow, 2},
    {"ffi_mt_shuffle_perm", (DL_FUNC) &ffi_mt_shuffle_perm, 2},
    {"ffi_mt_gauss", (DL_FUNC) &ffi_mt_gauss, 3},
    {"ffi_mt_getstate", (DL_FUNC) &ffi_mt_getstate, 1},
    {"ffi_mt_setstate", (DL_FUNC) &ffi_mt_setstate, 5},
    {"ffi_mt_seed_uint32", (DL_FUNC) &ffi_mt_seed_uint32, 2},
    {"ffi_mt_np_randn", (DL_FUNC) &ffi_mt_np_randn, 4},
    {"ffi_mt_np_randint", (DL_FUNC) &ffi_mt_np_randint, 4},
    {"ffi_pcg64_new", (DL_FUNC) &ffi_pcg64_new, 0},
    {"ffi_pcg64_seed", (DL_FUNC) &ffi_pcg64_seed, 2},
    {"ffi_pcg64_random", (DL_FUNC) &ffi_pcg64_random, 2},
    {"ffi_pcg64_integers", (DL_FUNC) &ffi_pcg64_integers, 4},
    {NULL, NULL, 0}};

void R_init_pyrt(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
