# pyport & pyrt — Convert Python Packages into Standalone Native R Packages

`pyport` converts Python 3 packages into standalone, CRAN-clean R packages that install and run across Windows, macOS, and Linux **without any Python installation, `reticulate`, virtualenv, or `PATH` changes**.

## Repository Structure

- **`packages/pyrt`** — Lightweight CRAN-clean runtime package providing Python 3 semantics in R:
  - Reference-semantics containers (`py_list`, `py_dict`, `py_set`, `py_tuple`)
  - Classes, C3 MRO multiple inheritance, `super()`, descriptors (`@property`, `@staticmethod`, `@classmethod`)
  - Exception hierarchy (`py_raise`, `py_try`, `py_with`)
  - 0-based negative and step slicing (`py_slice`, `py_getitem`, `py_setitem`)
  - Lazy $O(1)$-stack CPS trampoline generators (`py_generator`, `py_yield`, `py_bounce`)
  - Arbitrary-precision integers (`gmp::bigz`) with floor division/modulo toward $-\infty$ and two's-complement bitwise operations
  - Bit-exact CPython 3.12 `random` (`MT19937` + FIPS 180-4 `SHA-512` `version=2` seeding in C)
  - Curated Python standard library (`math`, `itertools`, `functools`, `operator`, `collections`, `re`, `json`, `copy`, `bisect`, `heapq`, `string`, `datetime`, `dataclasses`, `enum`, `sys`)
  - `numpy` (`py_ndarray`, broadcasting, axis reductions, `@` matmul, `linalg`, `fft`, `random.RandomState`), `scipy` (`special`, `stats`, `optimize`, `linalg`), and `pandas` (`Series`, `DataFrame`) bridges
- **`packages/pyport`** — Python-to-R converter package:
  - Vendored `tree-sitter-python` v0.23.6 C grammar (`parse_python()`, `ast_dump()`) — parses Python 3.12 syntax with zero Python dependency
  - Scope & escape analyzer (`analyze_python()`, `analyze_escape()`)
  - Pure-Python transpiler (`transpile_python()`, `eval_python()`)
  - Standalone R package generator (`convert()`) with license auditing (`check_python_license()`) and `pytest`/`unittest` -> `testthat` test conversion (`convert_pytest_code()`)
  - Native C/C++/Cython (`cpython_shim_header()`, `transpile_c_extension()`) and Rust (`PyO3` -> `extendr` via `transpile_pyo3_rust()`) bridges

## Installation & Verification

```r
install.packages(c("packages/pyrt", "packages/pyport"), repos = NULL, type = "source")

# Run R CMD check on both packages (0 errors | 0 warnings | 0 notes)
rcmdcheck::rcmdcheck("packages/pyrt", args = "--no-manual")
rcmdcheck::rcmdcheck("packages/pyport", args = "--no-manual")
```
