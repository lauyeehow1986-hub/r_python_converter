#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// ============================================================================
// 1. CPython-identical MT19937 PRNG + SHA-512 (for str/bytes seeds, version=2)
// ============================================================================
//
// CPython's `_randommodule.c` and `Lib/random.py` implement:
// - MT19937 with state size N = 624, M = 397, MATRIX_A = 0x9908b0dfUL
// - `init_genrand(s)` and `init_by_array(init_key, key_length)` (with initial
//   `init_genrand(19650218UL)`)
// - `genrand_uint32()` (tempering shifts 11, 7, 15, 18)
// - `random_random()`: two 32-bit draws `a >> 5` (27 bits) and `b >> 6`
//   (26 bits) combined as `(a * 67108864.0 + b) * (1.0 / 9007199254740992.0)`
// - `seed(a)` (version=2):
//   * `None`: non-deterministic OS/time seed
//   * `int`: `a = abs(a)`, split into 32-bit little-endian words (at least 1
//     word `0` when `a == 0`), passed to `init_by_array`
//   * `str` / `bytes`: UTF-8 / raw bytes `x` concatenated with big-endian
//     64-bit `SHA512(x)` digest (total `len(x) + 64` bytes), interpreted as a
//     big-endian unsigned integer, then split into 32-bit little-endian words
//     and passed to `init_by_array`.
// - `_randbelow_with_getrandbits(n)`: `k = n.bit_length(); r = getrandbits(k);
//   while r >= n: r = getrandbits(k); return r` (returns 0 when `n == 1`).

#define MT_N 624
#define MT_M 397
#define MT_MATRIX_A 0x9908b0dfUL
#define MT_UPPER_MASK 0x80000000UL
#define MT_LOWER_MASK 0x7fffffffUL

typedef struct {
  uint32_t state[MT_N];
  int index;
  int has_gauss;
  double gauss_next;
} pyrt_mt_state;

static void mt_init_genrand(pyrt_mt_state* mt, uint32_t s) {
  mt->state[0] = s;
  for (int mti = 1; mti < MT_N; ++mti) {
    mt->state[mti] =
        (1812433253UL * (mt->state[mti - 1] ^ (mt->state[mti - 1] >> 30)) +
         (uint32_t) mti);
  }
  mt->index = MT_N;
}

static void mt_init_by_array(pyrt_mt_state* mt, const uint32_t* init_key,
                             int key_length) {
  mt_init_genrand(mt, 19650218UL);
  int i = 1;
  int j = 0;
  int k = (MT_N > key_length ? MT_N : key_length);
  for (; k > 0; --k) {
    mt->state[i] =
        (mt->state[i] ^
         ((mt->state[i - 1] ^ (mt->state[i - 1] >> 30)) * 1664525UL)) +
        init_key[j] + (uint32_t) j;
    ++i;
    ++j;
    if (i >= MT_N) {
      mt->state[0] = mt->state[MT_N - 1];
      i = 1;
    }
    if (j >= key_length) {
      j = 0;
    }
  }
  for (k = MT_N - 1; k > 0; --k) {
    mt->state[i] =
        (mt->state[i] ^
         ((mt->state[i - 1] ^ (mt->state[i - 1] >> 30)) * 1566083941UL)) -
        (uint32_t) i;
    ++i;
    if (i >= MT_N) {
      mt->state[0] = mt->state[MT_N - 1];
      i = 1;
    }
  }
  mt->state[0] = 0x80000000UL;
  mt->has_gauss = 0;
  mt->gauss_next = 0.0;
}

static uint32_t mt_genrand_uint32(pyrt_mt_state* mt) {
  uint32_t y;
  static const uint32_t mag01[2] = {0x0UL, MT_MATRIX_A};

  if (mt->index >= MT_N) {
    int kk;
    for (kk = 0; kk < MT_N - MT_M; ++kk) {
      y = (mt->state[kk] & MT_UPPER_MASK) | (mt->state[kk + 1] & MT_LOWER_MASK);
      mt->state[kk] = mt->state[kk + MT_M] ^ (y >> 1) ^ mag01[y & 0x1UL];
    }
    for (; kk < MT_N - 1; ++kk) {
      y = (mt->state[kk] & MT_UPPER_MASK) | (mt->state[kk + 1] & MT_LOWER_MASK);
      mt->state[kk] =
          mt->state[kk + (MT_M - MT_N)] ^ (y >> 1) ^ mag01[y & 0x1UL];
    }
    y = (mt->state[MT_N - 1] & MT_UPPER_MASK) | (mt->state[0] & MT_LOWER_MASK);
    mt->state[MT_N - 1] = mt->state[MT_M - 1] ^ (y >> 1) ^ mag01[y & 0x1UL];
    mt->index = 0;
  }

  y = mt->state[mt->index++];
  y ^= (y >> 11);
  y ^= (y << 7) & 0x9d2c5680UL;
  y ^= (y << 15) & 0xefc60000UL;
  y ^= (y >> 18);
  return y;
}

static double mt_random_res53(pyrt_mt_state* mt) {
  uint32_t a = mt_genrand_uint32(mt) >> 5;
  uint32_t b = mt_genrand_uint32(mt) >> 6;
  return (a * 67108864.0 + b) * (1.0 / 9007199254740992.0);
}

// ---- Minimal portable SHA-512 (FIPS 180-4) for version=2 str/bytes seeds ---

static const uint64_t sha512_k[80] = {
    0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL, 0xb5c0fbcfec4d3b2fULL,
    0xe9b5dba58189dbbcULL, 0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL,
    0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL, 0xd807aa98a3030242ULL,
    0x12835b0145706fbeULL, 0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
    0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL, 0x9bdc06a725c71235ULL,
    0xc19bf174cf692694ULL, 0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL,
    0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL, 0x2de92c6f592b0275ULL,
    0x4a7484aa6ea6e483ULL, 0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
    0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL, 0xb00327c898fb213fULL,
    0xbf597fc7beef0ee4ULL, 0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL,
    0x06ca6351e003826fULL, 0x142929670a0e6e70ULL, 0x27b70a8546d22ffcULL,
    0x2e1b21385c26c926ULL, 0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
    0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL, 0x81c2c92e47edaee6ULL,
    0x92722c851482353bULL, 0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL,
    0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL, 0xd192e819d6ef5218ULL,
    0xd69906245565a910ULL, 0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
    0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL, 0x2748774cdf8eeb99ULL,
    0x34b0bcb5e19b48a8ULL, 0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL,
    0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL, 0x748f82ee5defb2fcULL,
    0x78a5636f43172f60ULL, 0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
    0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL, 0xbef9a3f7b2c67915ULL,
    0xc67178f2e372532bULL, 0xca273eceea26619cULL, 0xd186b8c721c0c207ULL,
    0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL, 0x06f067aa72176fbaULL,
    0x0a637dc5a2c898a6ULL, 0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
    0x28db77f523047d84ULL, 0x32caab7b40c72493ULL, 0x3c9ebe0a15c9bebcULL,
    0x431d67c49c100d4cULL, 0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL,
    0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL};

static uint64_t rotr64(uint64_t x, int n) {
  return (x >> n) | (x << (64 - n));
}

static void sha512_block(uint64_t h[8], const uint8_t block[128]) {
  uint64_t w[80];
  for (int i = 0; i < 16; ++i) {
    uint64_t v = 0;
    for (int b = 0; b < 8; ++b) {
      v = (v << 8) | block[i * 8 + b];
    }
    w[i] = v;
  }
  for (int i = 16; i < 80; ++i) {
    uint64_t s0 = rotr64(w[i - 15], 1) ^ rotr64(w[i - 15], 8) ^ (w[i - 15] >> 7);
    uint64_t s1 = rotr64(w[i - 2], 19) ^ rotr64(w[i - 2], 61) ^ (w[i - 2] >> 6);
    w[i] = w[i - 16] + s0 + w[i - 7] + s1;
  }
  uint64_t a = h[0], b = h[1], c = h[2], d = h[3];
  uint64_t e = h[4], f = h[5], g = h[6], hh = h[7];
  for (int i = 0; i < 80; ++i) {
    uint64_t S1 = rotr64(e, 14) ^ rotr64(e, 18) ^ rotr64(e, 41);
    uint64_t ch = (e & f) ^ ((~e) & g);
    uint64_t temp1 = hh + S1 + ch + sha512_k[i] + w[i];
    uint64_t S0 = rotr64(a, 28) ^ rotr64(a, 34) ^ rotr64(a, 39);
    uint64_t maj = (a & b) ^ (a & c) ^ (b & c);
    uint64_t temp2 = S0 + maj;
    hh = g;
    g = f;
    f = e;
    e = d + temp1;
    d = c;
    c = b;
    b = a;
    a = temp1 + temp2;
  }
  h[0] += a;
  h[1] += b;
  h[2] += c;
  h[3] += d;
  h[4] += e;
  h[5] += f;
  h[6] += g;
  h[7] += hh;
}

static void sha512_compute(const uint8_t* msg, size_t len, uint8_t out[64]) {
  uint64_t h[8] = {0x6a09e667f3bcc908ULL, 0xbb67ae8584caa73bULL,
                   0x3c6ef372fe94f82bULL, 0xa54ff53a5f1d36f1ULL,
                   0x510e527fade682d1ULL, 0x9b05688c2b3e6c1fULL,
                   0x1f83d9abfb41bd6bULL, 0x5be0cd19137e2179ULL};
  size_t off = 0;
  while (len - off >= 128) {
    sha512_block(h, msg + off);
    off += 128;
  }
  uint8_t tail[256];
  memset(tail, 0, sizeof(tail));
  size_t rem = len - off;
  if (rem > 0) {
    memcpy(tail, msg + off, rem);
  }
  tail[rem] = 0x80;
  size_t total_blocks = (rem + 1 + 16 <= 128) ? 1 : 2;
  uint64_t bit_len = (uint64_t) len * 8ULL;
  size_t len_pos = total_blocks * 128 - 8;
  for (int b = 0; b < 8; ++b) {
    tail[len_pos + b] = (uint8_t) (bit_len >> (56 - 8 * b));
  }
  sha512_block(h, tail);
  if (total_blocks == 2) {
    sha512_block(h, tail + 128);
  }
  for (int i = 0; i < 8; ++i) {
    for (int b = 0; b < 8; ++b) {
      out[i * 8 + b] = (uint8_t) (h[i] >> (56 - 8 * b));
    }
  }
}

// ---- External pointer lifecycle for pyrt_mt_state --------------------------

static void mt_finalizer(SEXP ptr) {
  pyrt_mt_state* mt = (pyrt_mt_state*) R_ExternalPtrAddr(ptr);
  if (mt != NULL) {
    free(mt);
    R_ClearExternalPtr(ptr);
  }
}

static pyrt_mt_state* get_mt(SEXP ptr) {
  if (TYPEOF(ptr) != EXTPTRSXP) {
    Rf_error("`ptr` must be a `pyrt_mt` external pointer.");
  }
  pyrt_mt_state* mt = (pyrt_mt_state*) R_ExternalPtrAddr(ptr);
  if (mt == NULL) {
    Rf_error("`pyrt_mt` pointer has been finalized.");
  }
  return mt;
}

SEXP ffi_mt_new(void) {
  pyrt_mt_state* mt = (pyrt_mt_state*) calloc(1, sizeof(pyrt_mt_state));
  if (mt == NULL) {
    Rf_error("Out of memory allocating MT19937 state.");
  }
  uint32_t default_key[1] = {0UL};
  mt_init_by_array(mt, default_key, 1);
  SEXP ptr = PROTECT(R_MakeExternalPtr(mt, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(ptr, mt_finalizer, TRUE);
  UNPROTECT(1);
  return ptr;
}

// Seed from an integer vector of 32-bit unsigned words (in [0, 2^32-1], passed
// as R double or integer vector, little-endian word order).
SEXP ffi_mt_seed_words(SEXP ptr, SEXP words) {
  pyrt_mt_state* mt = get_mt(ptr);
  R_xlen_t n = Rf_xlength(words);
  if (n <= 0) {
    uint32_t zero = 0UL;
    mt_init_by_array(mt, &zero, 1);
    return R_NilValue;
  }
  SEXP dbl = PROTECT(Rf_coerceVector(words, REALSXP));
  const double* p = REAL(dbl);
  uint32_t* key = (uint32_t*) R_alloc((size_t) n, sizeof(uint32_t));
  for (R_xlen_t i = 0; i < n; ++i) {
    key[i] = (uint32_t) (uint64_t) p[i];
  }
  mt_init_by_array(mt, key, (int) n);
  UNPROTECT(1);
  return R_NilValue;
}

// Seed from a `str` or `bytes` object using CPython's version=2 algorithm:
// append the 64-byte SHA-512 digest to the raw bytes, interpret the combined
// buffer as a big-endian unsigned integer, and split into 32-bit little-endian
// words for `init_by_array`.
SEXP ffi_mt_seed_bytes(SEXP ptr, SEXP raw_bytes) {
  pyrt_mt_state* mt = get_mt(ptr);
  if (TYPEOF(raw_bytes) != RAWSXP) {
    Rf_error("`raw_bytes` must be a raw vector.");
  }
  size_t len = (size_t) Rf_xlength(raw_bytes);
  const uint8_t* src = RAW(raw_bytes);
  size_t total = len + 64;
  uint8_t* buf = (uint8_t*) R_alloc(total, 1);
  if (len > 0) {
    memcpy(buf, src, len);
  }
  sha512_compute(src, len, buf + len);

  // Strip leading zero bytes (big-endian) just as CPython's `int.from_bytes`
  // does when converting to a Python `int` before `_random.Random.seed`.
  size_t start = 0;
  while (start < total && buf[start] == 0) {
    ++start;
  }
  size_t nbytes = total - start;
  if (nbytes == 0) {
    uint32_t zero = 0UL;
    mt_init_by_array(mt, &zero, 1);
    return R_NilValue;
  }
  const uint8_t* b = buf + start;
  int nwords = (int) ((nbytes + 3) / 4);
  uint32_t* key = (uint32_t*) R_alloc((size_t) nwords, sizeof(uint32_t));
  // Pack from the least-significant end (rightmost bytes) into 32-bit words.
  for (int w = 0; w < nwords; ++w) {
    uint32_t word = 0;
    for (int byte_i = 0; byte_i < 4; ++byte_i) {
      size_t idx_from_end = (size_t) w * 4 + (size_t) byte_i;
      if (idx_from_end < nbytes) {
        uint8_t v = b[nbytes - 1 - idx_from_end];
        word |= ((uint32_t) v) << (8 * byte_i);
      }
    }
    key[w] = word;
  }
  mt_init_by_array(mt, key, nwords);
  return R_NilValue;
}

// Draw `n` floats from `random()`.
SEXP ffi_mt_random(SEXP ptr, SEXP n_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  R_xlen_t n = Rf_asInteger(n_sexp);
  if (n < 0) Rf_error("`n` must be non-negative.");
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double* p = REAL(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    p[i] = mt_random_res53(mt);
  }
  UNPROTECT(1);
  return out;
}

// Draw a single 32-bit unsigned integer (returned as R double so all 32 bits
// are preserved without colliding with NA_integer_).
SEXP ffi_mt_uint32(SEXP ptr) {
  pyrt_mt_state* mt = get_mt(ptr);
  return Rf_ScalarReal((double) mt_genrand_uint32(mt));
}

// Draw `k` random bits and return the 32-bit words (little-endian, high word
// masked to `k % 32` bits) exactly as CPython's `_random_Random_getrandbits_impl`
// builds them.
SEXP ffi_mt_getrandbits_words(SEXP ptr, SEXP k_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  int k = Rf_asInteger(k_sexp);
  if (k < 0) Rf_error("number of bits must be non-negative");
  if (k == 0) return Rf_allocVector(REALSXP, 0);
  int words = (k - 1) / 32 + 1;
  SEXP out = PROTECT(Rf_allocVector(REALSXP, words));
  double* p = REAL(out);
  for (int i = 0; i < words; ++i, k -= 32) {
    uint32_t r = mt_genrand_uint32(mt);
    if (k < 32) {
      r >>= (32 - k);
    }
    p[i] = (double) r;
  }
  UNPROTECT(1);
  return out;
}

// Fast path for `_randbelow(n)` when `0 < n <= 2^53`: matches CPython's
// `_randbelow_with_getrandbits(n)` bit-for-bit.
static uint64_t mt_getrandbits_u64(pyrt_mt_state* mt, int k) {
  if (k <= 0) return 0ULL;
  if (k <= 32) {
    uint32_t r = mt_genrand_uint32(mt);
    if (k < 32) r >>= (32 - k);
    return (uint64_t) r;
  }
  uint32_t low = mt_genrand_uint32(mt);
  int rem = k - 32;
  uint32_t high = mt_genrand_uint32(mt);
  if (rem < 32) high >>= (32 - rem);
  return ((uint64_t) high << 32) | (uint64_t) low;
}

static uint64_t mt_randbelow_u64(pyrt_mt_state* mt, uint64_t n) {
  if (n <= 1ULL) return 0ULL;
  int k = 0;
  uint64_t tmp = n;
  while (tmp > 0ULL) {
    ++k;
    tmp >>= 1;
  }
  uint64_t r;
  do {
    r = mt_getrandbits_u64(mt, k);
  } while (r >= n);
  return r;
}

SEXP ffi_mt_randbelow(SEXP ptr, SEXP n_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  double nd = Rf_asReal(n_sexp);
  if (!(nd >= 1.0 && nd <= 9007199254740992.0)) {
    Rf_error("`n` out of fast 53-bit range.");
  }
  uint64_t n = (uint64_t) nd;
  return Rf_ScalarReal((double) mt_randbelow_u64(mt, n));
}

// CPython's `Random.shuffle(x)`:
//   for i in reversed(range(1, len(x))):
//       j = randbelow(i + 1)
//       x[i], x[j] = x[j], x[i]
// Returns the 1-based permutation indices for a list of length `len`.
SEXP ffi_mt_shuffle_perm(SEXP ptr, SEXP len_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  int n = Rf_asInteger(len_sexp);
  if (n < 0) Rf_error("`len` must be non-negative.");
  SEXP perm = PROTECT(Rf_allocVector(INTSXP, n));
  int* p = INTEGER(perm);
  for (int i = 0; i < n; ++i) p[i] = i + 1;
  for (int i = n - 1; i >= 1; --i) {
    int j = (int) mt_randbelow_u64(mt, (uint64_t) (i + 1));
    int tmp = p[i];
    p[i] = p[j];
    p[j] = tmp;
  }
  UNPROTECT(1);
  return perm;
}

// CPython's `Random.gauss(mu, sigma)` (Box-Muller with cached `gauss_next`).
SEXP ffi_mt_gauss(SEXP ptr, SEXP mu_sexp, SEXP sigma_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  double mu = Rf_asReal(mu_sexp);
  double sigma = Rf_asReal(sigma_sexp);
  double z;
  if (mt->has_gauss) {
    z = mt->gauss_next;
    mt->has_gauss = 0;
    mt->gauss_next = 0.0;
  } else {
    double x2pi = mt_random_res53(mt) * (2.0 * 3.14159265358979323846);
    double g2rad = sqrt(-2.0 * log(1.0 - mt_random_res53(mt)));
    z = cos(x2pi) * g2rad;
    mt->gauss_next = sin(x2pi) * g2rad;
    mt->has_gauss = 1;
  }
  return Rf_ScalarReal(mu + z * sigma);
}

// Get / set internal state: 624 uint32 values (as double), index (int),
// has_gauss (logical), gauss_next (double).
SEXP ffi_mt_getstate(SEXP ptr) {
  pyrt_mt_state* mt = get_mt(ptr);
  SEXP state_vec = PROTECT(Rf_allocVector(REALSXP, MT_N));
  double* ps = REAL(state_vec);
  for (int i = 0; i < MT_N; ++i) ps[i] = (double) mt->state[i];

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 4));
  SET_VECTOR_ELT(out, 0, state_vec);
  SET_VECTOR_ELT(out, 1, Rf_ScalarInteger(mt->index));
  SET_VECTOR_ELT(out, 2, Rf_ScalarLogical(mt->has_gauss));
  SET_VECTOR_ELT(out, 3, Rf_ScalarReal(mt->gauss_next));
  UNPROTECT(2);
  return out;
}

SEXP ffi_mt_setstate(SEXP ptr, SEXP state_vec, SEXP index_sexp,
                     SEXP has_gauss_sexp, SEXP gauss_next_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  if (TYPEOF(state_vec) != REALSXP || Rf_xlength(state_vec) != MT_N) {
    Rf_error("`state_vec` must be a double vector of length 624.");
  }
  const double* ps = REAL(state_vec);
  for (int i = 0; i < MT_N; ++i) {
    mt->state[i] = (uint32_t) (uint64_t) ps[i];
  }
  mt->index = Rf_asInteger(index_sexp);
  mt->has_gauss = Rf_asLogical(has_gauss_sexp) ? 1 : 0;
  mt->gauss_next = Rf_asReal(gauss_next_sexp);
  return R_NilValue;
}

// ============================================================================
// 4. NumPy legacy `RandomState` MT19937 compatibility entry points
// ============================================================================
//
// NumPy's `RandomState(seed)` (for scalar 32-bit integer seeds) calls
// `mt_init_genrand(s)` directly (rather than `init_by_array`), uses Polar
// Marsaglia cached normal draws (`rk_gauss`), and draws bounded integers via
// Lemire/mask rejection (`rk_interval`).

SEXP ffi_mt_seed_uint32(SEXP ptr, SEXP seed_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  uint32_t s = (uint32_t) (int64_t) Rf_asReal(seed_sexp);
  mt_init_genrand(mt, s);
  mt->has_gauss = 0;
  mt->gauss_next = 0.0;
  return R_NilValue;
}

// NumPy `rk_gauss` Polar Marsaglia normal generator:
//   if (has_gauss) { return gauss_next; }
//   do {
//     x1 = 2.0 * rk_double(state) - 1.0;
//     x2 = 2.0 * rk_double(state) - 1.0;
//     r2 = x1 * x1 + x2 * x2;
//   } while (r2 >= 1.0 || r2 == 0.0);
//   f = sqrt(-2.0 * log(r2) / r2);
//   gauss = f * x1;
//   return f * x2;
static double np_rk_gauss(pyrt_mt_state* mt) {
  if (mt->has_gauss) {
    double tmp = mt->gauss_next;
    mt->has_gauss = 0;
    mt->gauss_next = 0.0;
    return tmp;
  }
  double x1, x2, r2;
  do {
    x1 = 2.0 * mt_random_res53(mt) - 1.0;
    x2 = 2.0 * mt_random_res53(mt) - 1.0;
    r2 = x1 * x1 + x2 * x2;
  } while (r2 >= 1.0 || r2 == 0.0);
  double f = sqrt(-2.0 * log(r2) / r2);
  mt->gauss_next = f * x1;
  mt->has_gauss = 1;
  return f * x2;
}

SEXP ffi_mt_np_randn(SEXP ptr, SEXP n_sexp, SEXP loc_sexp, SEXP scale_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  R_xlen_t n = (R_xlen_t) Rf_asInteger(n_sexp);
  double loc = Rf_asReal(loc_sexp);
  double scale = Rf_asReal(scale_sexp);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  double* p = REAL(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    p[i] = loc + scale * np_rk_gauss(mt);
  }
  UNPROTECT(1);
  return out;
}

// NumPy `rk_interval(max, state)`: smallest bitmask `>= max`, draw uint32
// masked until `<= max`.
static uint32_t np_rk_interval(pyrt_mt_state* mt, uint32_t max_val) {
  if (max_val == 0U) return 0U;
  uint32_t mask = max_val;
  mask |= mask >> 1;
  mask |= mask >> 2;
  mask |= mask >> 4;
  mask |= mask >> 8;
  mask |= mask >> 16;
  uint32_t value;
  while ((value = (mt_genrand_uint32(mt) & mask)) > max_val) {
  }
  return value;
}

SEXP ffi_mt_np_randint(SEXP ptr, SEXP low_sexp, SEXP high_sexp, SEXP n_sexp) {
  pyrt_mt_state* mt = get_mt(ptr);
  int low = Rf_asInteger(low_sexp);
  int high = Rf_asInteger(high_sexp);
  R_xlen_t n = (R_xlen_t) Rf_asInteger(n_sexp);
  if (high <= low) Rf_error("high <= low in randint");
  uint32_t rng = (uint32_t) (high - low - 1);
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n));
  int* p = INTEGER(out);
  for (R_xlen_t i = 0; i < n; ++i) {
    p[i] = low + (int) np_rk_interval(mt, rng);
  }
  UNPROTECT(1);
  return out;
}

