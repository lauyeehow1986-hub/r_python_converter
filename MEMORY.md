# MEMORY.md — persistent project memory

Update this file whenever a decision, discovery, or status change happens. Newest entries first
within each section.

## Status
- **2026-10-17** — **Completed all 5 post-Phase-8 deliverables in sequence (`pyrt` 0.1.0 & `pyport` 0.1.0, `0 errors | 0 warnings | 0 notes`):**
  1. **Binary & PyPI Batch Builder (`pyport::build_r_package()`, `tools/convert-pypi.R`)**: Builds `.tar.gz` source tarballs and `.zip`/`.tgz` binary R packages via `R CMD INSTALL --build` and benchmarks corpus conversion.
  2. **Codegen Loop & Escape Optimization (`pyrt::py_range_seq()`, `transpile.R`)**: Wired `for <id> in range(...)` without `else` directly to native R `for (<id> in pyrt::py_range_seq(...))` and attached `analyze_escape()` metadata to function contexts.
  3. **Bit-Exact NumPy `default_rng` (`PCG64` + `SeedSequence` in C), Multi-Axis Reductions, $N$-D Broadcasting, & `np$einsum`**: Implemented portable 128-bit `PCG-XSL-RR-128/64` + Melissa O'Neill's `SeedSequence` + Lemire's 32-bit bounded integers in `packages/pyrt/src/random.c` (verified bit-exact against NumPy `default_rng(42)`), general $N$-D broadcasting, tuple `axis = (0, 1)` + `keepdims = TRUE` reductions, and `np$einsum`.
  4. **Deeper Native C/Cython (`PyMethodDef` + `PyArg_ParseTuple`, Cython `cdef`/`cpdef`) & Rust (`#[pyclass]`/`#[pymethods]`) Lowering (`native.R`)**: Added variadic `PyArg_ParseTuple` to `pyport_cpython_shim.h`, automatic `PyMethodDef` wrapper generation, Cython `cdef`/`cpdef` scalar function lowering, and `#[pyclass]`/`#[pymethods]` -> `#[extendr]` lowering.
  5. **Binary Repository (`tools/build-drat-repo.R`, `.github/workflows/build-binaries.yaml`) & CRAN `0.1.0` Release Prep**: Bumped `pyrt` and `pyport` to `0.1.0`, added `NEWS.md` and `cran-comments.md`, and added `tools/build-drat-repo.R` + CI binary artifact workflow.
- **2026-10-17** — **Phases 0–8 + acceptance corpus & declarative mappings complete (`0 errors | 0 warnings | 0 notes` under `R CMD check` for both `pyrt` and `pyport`, pushed to `https://github.com/lauyeehow1986-hub/r_python_converter`).**
  - **Acceptance corpus (`corpus/`) & mappings (`mappings/index.yaml`)**: Populated all 6 acceptance corpus fixtures (`corpus/more-itertools`, `corpus/python-dateutil`, `corpus/attrs`, `corpus/sklearn-statsmodels`, `corpus/pyyaml-c`, `corpus/orjson-rust`), declarative mapping manifest (`mappings/index.yaml`), and end-to-end test suite (`packages/pyport/tests/testthat/test-corpus.R`). Verified `pyyaml-c` compiles its `<Python.h>` C extension via `pyport_cpython_shim.h` with GCC 14.3.0 (`-Wall -pedantic`) into a working native DLL with zero warnings.
  - **Phase 0 (`pyport` parser & AST)**: Vendored `tree-sitter-python` v0.23.6 (ABI 14) in `packages/pyport/src`, `parse_python()`, `ast_dump()` verified bit-for-bit against CPython 3.12 across 51 constructs.
  - **Phase 1 (`pyrt` core runtime)**: Reference-semantics containers (`py_list`, `py_dict`, `py_set`, `py_tuple`), C3 MRO classes & descriptors (`property`, `staticmethod`, `classmethod`, `super()`), exception hierarchy & `py_try()`/`py_with()`, 0-based negative/step slicing, CPS trampoline generators (`py_generator()`, `py_bounce()`, `py_yield()`), `gmp::bigz` arbitrary-precision ints & exact floor division / two's-complement bitwise ops, bit-exact CPython 3.12 `random` module in C (`MT19937` + `SHA-512` `version=2` seeding), and `py_builtin()`.
  - **Phase 2 (`pyport` scope analyzer & pure-Python transpiler)**: `analyze_python()`, `transpile_python()`, and `eval_python()` covering lexical scopes (`global`/`nonlocal`), closures, `*args`/`**kwargs`/kwonlyargs, decorators, multiple inheritance, `for/else`, `while/else`, `try/except/else/finally`, `with`, `match/case`, comprehensions, and lazy CPS-lowered generators (`yield`, `yield from`, `.send()`).
  - **Phase 3 (`pyport::convert()` standalone R package emitter)**: License auditing (`check_python_license()`, supporting permissive and GPL/LGPL while refusing proprietary/non-derivative licenses), `pytest`/`unittest` -> `testthat` converter (`convert_pytest_code()`), `.Rd` help generator matching R `formals()`, and `R CMD check`-clean standalone package generation.
  - **Phase 4 (`pyrt` stdlib mappings & pure-Python corpus)**: `py_stdlib()` implementing `math`, `itertools`, `functools`, `operator`, `collections` (`deque`, `defaultdict`, `Counter`, `OrderedDict`, `namedtuple`), `re`, `json`, `copy`, `bisect`, `heapq`, `string`, `datetime` (`date`, `time`, `datetime`, `timedelta`), `dataclasses`, `enum`, `random`, and `sys`.
  - **Phase 5 (`numpy` / `scipy` / `pandas` bridge)**: `py_ndarray()` and `py_stdlib("numpy")` (including bit-exact `numpy.random.RandomState` in C), `py_stdlib("scipy")` (`special`, `stats`, `optimize`, `linalg`), and `py_stdlib("pandas")` (`Series`, `DataFrame`), verified on `statsmodels`-style OLS and `scikit-learn`-style `StandardScaler` + `Ridge` models.
  - **Phases 6 & 7 (Native C/C++/Cython and Rust `PyO3` -> `extendr` bridges)**: `detect_native_extensions()`, `cpython_shim_header()`, `transpile_c_extension()`, and `transpile_pyo3_rust()` integrated into `pyport::convert()`.
  - **Phase 8 (Escape analysis, vignettes, CRAN readiness)**: `analyze_escape()` in `packages/pyport/R/analyze.R`, package vignettes (`packages/pyport/vignettes/pyport.Rmd` and `packages/pyrt/vignettes/pyrt.Rmd`), and full `R CMD check` verification (`0 errors | 0 warnings | 0 notes`).
- **2026-10-17** — **Phase 0 complete.** Monorepo scaffolded (`packages/pyport`,
  `packages/pyrt`, `.github/workflows/R-CMD-check.yaml`, `mappings/`, `corpus/`,
  `tools/vendor-grammar.R`, `docs/phase0-parser.md`). Vendored `tree-sitter-python`
  v0.23.6 (ABI 14) into `pyport/src` with CRAN-clean pragmas and zero compiler
  warnings. Implemented `pyport::python_language()`, `pyport::parse_python()`,
  `pyport::parse_python_file()`, `pyport::ast_dump()`, `pyport::is_py_ast()`,
  and `pyport::ast_loc()`. Verified `ast_dump(parse_python(...))` matches
  CPython 3.12's `ast.dump()` bit-for-bit across 51 constructs; `testthat` (63
  assertions in `pyport`, 21 in `pyrt`) and `R CMD check` pass on both packages.
  Next step: Phase 1 (`pyrt` core runtime).
- **2026-10-17** — User answered the open questions (names, targets, `random`, bigint, GPL).
  Rust toolchain found. Ready for Phase 0.
- **2026-10-17** — Project kicked off. Workspace was empty. Created `CLAUDE.md` and `MEMORY.md`.
  No code yet. Next step: Phase 0 (see Roadmap).

## Decisions log
| Date | Decision | Rationale |
|---|---|---|
| 2026-10-17 | Package names: **`pyport`** (converter) and **`pyrt`** (runtime) | User has no preference. Both names were free on CRAN when checked on 2026-10-17. Re-check before submitting. |
| 2026-10-17 | Acceptance corpus: `more-itertools`, `attrs`, `python-dateutil` (pure); `scikit-learn`/`statsmodels` subset (numpy/scipy); `pyyaml` (C ext); `orjson` (Rust) | Approved by user. |
| 2026-10-17 | `random` must be **bit-exact** with CPython (same seed gives the same sequence) | User requirement. Port CPython's MT19937 `init_by_array` seeding, `random()` (53-bit), `getrandbits`, `_randbelow`, and the algorithms of every distribution method (`randrange`, `shuffle`, `choice`, `sample`, `gauss`, …) in C. Do **not** use R's RNG. Same requirement for `numpy.random` (legacy `RandomState` MT19937 + `Generator`/PCG64) once the numpy tier lands. |
| 2026-10-17 | Arbitrary-precision `int` backed by **`gmp`** (CRAN pkg) | User choice. `pyrt` will `Imports: gmp`. Analyzer still emits native R int/double where it can prove bounds, for speed. |
| 2026-10-17 | GPL-licensed Python packages **are in scope** | User approved. The generated package inherits a GPL licence with the same or a compatible version. The emitter must set `License:` accordingly and refuse non-derivative/proprietary licences. |
| 2026-10-17 | Hybrid approach (transpile pure Python + map/port native code) | Gives native R and no interpreter. Embedding CPython was rejected (heavy, the same problem we are trying to escape). |
| 2026-10-17 | Scope: pure-Python, then numpy/scipy/pandas dependents, then own C/C++/Cython/Rust extensions | User wants broad coverage. Delivered in that order because each tier builds on the previous one. |
| 2026-10-17 | Generator output: one standalone R package per Python package | Users install it like any CRAN package. |
| 2026-10-17 | Faithful Python API (`module$func`, same names) + auto type conversion | Keeps docs and examples of the original transferable. |
| 2026-10-17 | Target CRAN for everything | User requirement. This drives strict portability, size and licensing rules. |
| 2026-10-17 | Converter written in R; parse Python via tree-sitter | Converting needs no Python. |

## Environment facts (dev machine)
- Windows. R 4.5.2 (ucrt) at `C:\Program Files\R`. Rtools45 present (`C:\rtools45`).
- Installed R pkgs: `treesitter` (0.3.1), `treesitter.r`, `gmp` (0.7-5.1), `Rcpp`, `cpp11`,
  `R6`, `testthat`, `devtools`, `pkgbuild`, `roxygen2`, `rextendr`, `reticulate`.
- Python 3.12 and 3.14 installed. Use them **only** for dev-time differential testing.
- Rust **is installed** (rustup, cargo/rustc 1.96.1, default `stable-x86_64-pc-windows-msvc`,
  `x86_64-pc-windows-gnu` target also installed, which R/Rtools on Windows needs). It lives in
  `%USERPROFILE%\.cargo\bin`, which is **not on PATH**. Don't edit PATH. Rust package
  `Makevars.win` must prepend `$(USERPROFILE)/.cargo/bin` (standard extendr template) and
  build with `--target x86_64-pc-windows-gnu`.

## Research notes
- `treesitter` 0.3.1 bundles tree-sitter 0.24.7 (ABI 13–14). `tree-sitter-python` v0.23.6 is
  generated with ABI 14 (`tree-sitter-python` v0.25.0 moved to ABI 15, so stick to v0.23.6 until
  CRAN `treesitter` bumps its C runtime). Confirmed working in `pyport`: `pyport` source tarball
  is ~179 KB (well under CRAN's 5 MB limit).
- `gmp::as.bigz()` parses `"0x..."` (hex), `"0b..."` (binary), and `"0..."` (leading zero as
  octal) directly; it does not take a `b =` base argument. Strip leading zeros on decimal
  literals like `00_0` before calling `gmp::as.bigz()`.
- reticulate ≥ 1.41 can provision Python via `uv` in the user directory without admin rights.
  That does not meet our goal (Python is still present and the downloads are heavy), but it is
  a useful reference for the differential-testing harness.
- Cython output (`.c`) targets the CPython C-API, so it cannot be reused directly. Plan:
  translate `.pyx` semantics into C++, or reuse the underlying pure-C library when one exists.
- PyO3 crates: the Rust core is often separable from the `#[pyfunction]` glue. Reuse the core
  and regenerate the glue with extendr.
- C/C++ extensions: many wrap a standalone C library (e.g. a hashing or compression lib). Reuse
  that library and rewrite only the CPython-API glue as cpp11.

## Known hard problems (track and design for)
1. Reference vs value semantics (lists/dicts mutated through aliases). Solved by
   environment-backed `pyrt` containers, at a performance cost. Later: escape analysis to emit
   plain R vectors when no aliasing is possible.
2. 0-based indexing, negative indices, slicing with step.
3. Generators/`yield`/`yield from`. Lower to state machines. `async` is out of scope initially.
4. Dynamic features: `eval/exec`, `getattr` with computed names, metaclasses, monkeypatching,
   `__import__`. Report these via diagnostics and support only common patterns.
5. Arbitrary-precision `int`. Use doubles/integers by default and switch to a bigint only where
   the analyzer can't prove the value stays bounded. Backend decided: `gmp` (`bigz`). Python
   semantics still need wrapping: floor division/modulo sign rules, `int(float)` truncation,
   `**` with negative exponents returning float, and bit ops on negatives (two's complement).
6. numpy row-major vs R column-major memory layout, broadcasting, dtype rules.
7. The scale of the stdlib. Map a curated subset (`math`, `re`, `json`, `collections`,
   `itertools`, `functools`, `os.path`, `datetime`, `random`, `string`, `typing`, `dataclasses`,
   `enum`, `abc`) and grow it based on corpus demand.
8. `re` module: Python regex vs R PCRE/ICU differences (named groups `(?P<n>)`, flags).
9. `random`: **bit-exact** with CPython (decided). This needs a C port of MT19937 plus CPython's
   seeding (int → `init_by_array`; str/bytes seeds go through SHA-512 in version 2) and each
   method's exact algorithm. Write golden tests from CPython output (generated in `tools/` and
   committed as fixtures, so CRAN tests need no Python). Floats must format/round-trip
   identically (`repr` uses shortest round-trip, so port or emulate it).

## Open questions (ask the user when relevant)
- All initial questions resolved on 2026-10-17 (see Decisions log).
- Which subset of `scikit-learn`/`statsmodels` to target first. Decide at Phase 5 based on
  numpy-shim coverage.

## Roadmap (draft)
| Phase | Deliverable |
|---|---|
| 0 | Monorepo scaffold, CI matrix, vendored tree-sitter-python, `pyport::parse_python()` returning AST |
| 1 | `pyrt` core: containers, classes/MRO, exceptions, iteration, slicing, str/format |
| 2 | Transpiler for pure-Python subset (functions, classes, control flow, comprehensions, generators) |
| 3 | Package emitter (DESCRIPTION/NAMESPACE/man/tests), pytest → testthat, `R CMD check` clean |
| 4 | Curated stdlib mappings + first pure-Python acceptance packages |
| 5 | `pyrt.numpy` shim + numpy mapping rules; then pandas/scipy subsets |
| 6 | Native tier: C/C++ glue regeneration (cpp11), Cython `.pyx` → C++ |
| 7 | Rust tier: PyO3 → extendr glue, vendored crates |
| 8 | Optimizations (escape analysis, vectorization), docs, CRAN submissions |
