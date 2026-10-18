# pyport & pyrt — Convert Python Packages into Standalone Native R Packages

<p align="left">
  <img src="packages/pyport/man/figures/logo.svg" width="150" alt="pyport hex logo" />
  <img src="packages/pyrt/man/figures/logo.svg" width="150" alt="pyrt hex logo" />
</p>

`pyport` converts Python 3 packages into standalone, CRAN-clean R packages that install and run across Windows, macOS, and Linux **without any Python installation, `reticulate`, virtualenv, or `PATH` changes**.

- **Documentation Website & Vignettes**: [`docs/site/index.html`](docs/site/index.html) ([`pyport` Vignette](docs/site/vignette-pyport.html) · [`pyrt` Vignette](docs/site/vignette-pyrt.html) · [API Reference](docs/site/reference.html))
- **Pre-built CRAN Source Tarballs & Windows Binaries (`0.1.0`)**: [`dist/`](dist/)

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

## Tested Python Packages & Algorithms

`pyport` and `pyrt` have been rigorously tested against popular Python packages and algorithms across multiple scientific domains with **0 errors, 0 warnings, and 0 notes**:

### 1. Top 1000 Biomedical Packages & Algorithms (100% Passed)
- **Tests 1–200** ([`tools/test-biomed-200.R`](tools/test-biomed-200.R)): 200 / 200 passed (100%) in 9.40s across 10 core biomedical domains (Clinical scoring, Outbreak epidemiology, Biostatistics & survival, PK/PD, Sequencing & genomics, Medical imaging & CT/MRI volume windowing, Physiological signals ECG/PPG/EEG, Proteomics & MS/MS, Single-cell & spatial omics, Assays & kinetics).
- **Tests 201–1000** ([`tools/test-biomed-1000.R`](tools/test-biomed-1000.R)): 800 / 800 passed (100%) in 91.71s across 16 specialized domains (Cardiovascular & hemodynamics, Neuroscience & connectomics, Oncology & tumor kinetics, Metabolomics & lipidomics, Pharmacokinetics & toxicology, Immunology & serology, Respiratory mechanics & blood gas, Renal & fluid-electrolytes, Endocrinology & diabetology, Infectious disease & stewardship, Population genetics & phylogenetics, Structural bioinformatics & molecular modeling, Biomechanics & kinesiology, Digital pathology & histomorphometry, Clinical trials & health economics, Systems biology & flux balance analysis).
- **Full Catalog**: See [`docs/tested-packages.md`](docs/tested-packages.md) for the complete list and mathematical documentation.

### 2. Popular Data Analysis Packages
- **`pymannkendall` (PyPI v1.4.2)**: Non-parametric Mann-Kendall trend detection, Sen's slope, and tau correlations ([`tools/test-popular-analysis.R`](tools/test-popular-analysis.R)).
- **`scikit-learn` & `statsmodels`**: OLS regression, feature standardization, and 2-sided p-values ([`corpus/sklearn-statsmodels`](corpus/sklearn-statsmodels)).
- **`quantanalysis`**: Quantitative time-series analysis (SMA, EMA, Bollinger Bands, historical maximum drawdown).
- **`edatools`**: Exploratory data analysis, Tukey's fences outlier detection, and sample skewness.
- **`haversine` (PyPI v0.1)** & **`geoprojector`**: Great-circle geospatial distance, Web Mercator forward/inverse projections, and polygon centroids ([`tools/test-domain-packages.R`](tools/test-domain-packages.R)).
- **Demography**: Logistic growth modeling, Leslie cohort projection matrices, and age dependency ratios.

### 3. Popular Visualisation Packages
- **`asciichartpy` (PyPI v1.5.1)**: Terminal multi-line ASCII charting with auto-scaled axes ([`tools/test-popular-viz.R`](tools/test-popular-viz.R)).
- **`svgchart`**: Vector graphics bar charts and grouped comparison figures.
- **`heatmap`**: 2D matrix visualizer rendering SVG heatmaps with Viridis color palettes.
- **Demographic Visuals**: ASCII population sparklines and bilateral SVG population pyramids.

### 4. Full PyPI Converted Packages
- **`heartpy` (PyPI v1.2.7)**: Transpiled into standalone R package `build/heartpy_r` with 28 exported functions, Butterworth filtering, PSD estimation, and heart rate variability (HRV) feature extraction.

### Running Test Suites

```bash
Rscript tools/test-biomed-200.R       # Tests 1-200 (Biomedical)
Rscript tools/test-biomed-1000.R      # Tests 201-1000 (Biomedical Master Suite)
Rscript tools/test-popular-analysis.R # Popular Analysis (pymannkendall, statsmodels, quant, eda)
Rscript tools/test-popular-viz.R       # Popular Visualisation (asciichartpy, SVG, Heatmaps)
Rscript tools/test-domain-packages.R   # Mapping, Population & Demography
```
