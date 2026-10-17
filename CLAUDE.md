# CLAUDE.md — Python → native R package converter

> Package names: **`pyport`** (converter, dev-time) and **`pyrt`** (small runtime support
> package used by generated packages). Both were free on CRAN on 2026-10-17.
> Key runtime requirements: big integers via `gmp`; `random` bit-exact with CPython.

## Goal
Convert Python packages into **standalone, native R packages** (source tarball + Windows/macOS
binaries) that install with `install.packages()` and run **without any Python interpreter,
reticulate, virtualenv, or PATH change**. Every R platform must work (Windows, macOS
x86_64/arm64, Linux). Both the converter and the generated packages aim for **CRAN acceptance**.

Why not reticulate: it needs a Python install plus the full package tree on every user machine,
which is heavy and sometimes blocked on locked-down Windows machines.

## Decisions (agreed with the user, do not change without asking)
| Topic | Decision |
|---|---|
| Approach | **Hybrid**: pure-Python code is transpiled into R source. Native and C-extension calls are mapped to R equivalents or to ported C/C++ (Rcpp/cpp11) or Rust (extendr). |
| Scope | Pure-Python packages, then numpy/scipy/pandas dependents, then packages with their own C/C++/Cython/Rust extensions. Delivered in that order. |
| Output | A **generator**: each converted Python package becomes its own standalone R package. |
| API style | **Faithful to Python**: same names, `module$func()` / `obj$method()` style, automatic type conversion at the boundary. |
| Distribution | Target **CRAN** for the converter, the runtime and the generated packages. |
| Converter language | **R**. Python is parsed with **tree-sitter** (the `treesitter` R package plus a vendored `tree-sitter-python` grammar), so running the converter needs no Python. |

## Architecture
```
Python sdist/source dir
  └─▶ 1. Ingest      read pyproject/setup.cfg metadata, license, module tree, tests
  └─▶ 2. Parse       tree-sitter-python → CST → normalized Python AST (R lists)
  └─▶ 3. Analyze     scopes, imports, class hierarchy/MRO, generators, dynamic-feature report
  └─▶ 4. Lower       Python AST → pyport IR (desugared: comprehensions, with, f-strings, etc.)
  └─▶ 5. Map         stdlib / numpy / scipy / pandas mapping rules (mappings/*.yaml + R shims)
  └─▶ 6. Native      C/C++ → Rcpp/cpp11 glue; Cython .pyx → C++; PyO3 Rust → extendr
  └─▶ 7. Emit        R/ code, src/, NAMESPACE, DESCRIPTION, man/, tests/testthat (from pytest)
  └─▶ 8. Verify      R CMD check --as-cran + differential tests vs real Python (dev only)
```

### Runtime package `pyrt` (the only runtime dependency of generated packages)
Supplies the Python semantics that R lacks. Keep it small, pure R where possible, CRAN-clean.
- Reference-semantics containers: `py_list`, `py_dict`, `py_set`, `py_tuple` (environment-backed;
  R's copy-on-modify would otherwise break aliasing).
- Class system: Python classes with C3 MRO, `super()`, dunder dispatch (`__add__`, `__getitem__`,
  `__iter__`, `__enter__`, …), properties, staticmethods/classmethods, and decorators.
- Iteration protocol, generators (lowered to closure state machines), `StopIteration`.
- Exceptions as R conditions with a Python-compatible hierarchy (`try/except/finally`, `raise from`).
- Numerics: Python `int` (arbitrary precision, needed only when overflow is possible), floor
  division, `%` semantics, `True/False/None`, 0-based indexing and slicing helpers.
- `str`/`bytes` methods, f-string formatting (format-spec mini-language), `*args/**kwargs` binding.
- Shims: `pyrt.numpy` (ndarray over R arrays with row-major view and broadcasting),
  `pyrt.pandas` (DataFrame/Series over data.frame), selected `scipy` APIs mapped to
  `stats`/`Matrix`/ported C.

## Repository layout (target)
```
packages/pyport/        Converter R package (parser, analyzer, IR, codegen, scaffolder)
  src/                  Vendored tree-sitter-python grammar (MIT; keep its LICENSE)
packages/pyrt/          Runtime support R package
mappings/               Declarative mapping rules: stdlib, numpy, scipy, pandas (YAML)
corpus/                 Pinned sample Python packages used as conversion fixtures (licenses OK)
examples/               End-to-end conversions (input → generated R package)
tools/                  Dev-only scripts (differential testing harness, may use Python)
docs/                   Design notes, semantics table, roadmap
.github/workflows/      R CMD check matrix (windows/macos/ubuntu × R release/devel/oldrel)
```

## Conventions
- R code: tidyverse style, `snake_case` functions, explicit `pkg::fun()` in package code, no
  `library()` calls inside packages. Roxygen2 for docs; testthat 3e for tests.
- Converter internals are plain R lists and S3. Use R6 only where reference semantics are needed.
- Native code: C++17 via cpp11 (preferred) or Rcpp. Use Rust/extendr only to reuse existing Rust
  crates (CRAN requires vendored crates, offline builds, and a documented `cargo` minimum version).
- Generated code must be **deterministic**: same input gives byte-identical output. Emit a
  header comment naming the source package, version and upstream license.
- Every unsupported construct produces a **structured diagnostic** (file:line, construct,
  suggested fix). Never drop code silently. Unsupported code becomes a stub that calls `stop()`
  with that diagnostic.

## CRAN rules that generated packages must satisfy
- No writes outside `tempdir()`. No network in examples/tests/vignettes. Examples run in under 5 s.
- Tarball should be ≤ 5 MB (large data → separate data package or download on explicit request).
- Portable C/C++ (no GNU-only flags, no `-march=native`), clean `R CMD check --as-cran`
  (0 errors, 0 warnings, 0 notes target), `Authors@R` with `cph` for upstream authors.
- `License:` must be compatible with and preserve the upstream license. Copy upstream `LICENSE`
  and attribution. **Never convert packages whose license forbids derivative works.**

## Build & test
```r
# from packages/pyport or packages/pyrt
devtools::document(); devtools::test(); devtools::check(args = "--as-cran")
# convert a package (planned API)
pyport::convert("path/to/python/pkg", out = "out/rpkg", name = "pkgname")
```
- Toolchain: R ≥ 4.1, Rtools45 on Windows, Xcode CLT on macOS.
- Differential tests (`tools/`) run the original Python package and the generated R package
  on the same inputs and compare results. They are dev/CI only and use a local Python. **End
  users never need Python.**

## Working rules for the assistant
- Read **MEMORY.md** at the start of each session, and update it when a decision, discovery or
  status change happens.
- Ask before changing any agreed decision in the table above.
- Add a corpus fixture and a test before adding each new mapping or semantic feature.
- Prefer correctness over coverage. A clear "unsupported" diagnostic beats a wrong translation.
