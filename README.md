# pyport & pyrt — Convert Python Packages into Standalone Native R Packages

<p align="left">
  <img src="packages/pyport/man/figures/logo.svg" width="150" alt="pyport hex logo" />
  <img src="packages/pyrt/man/figures/logo.svg" width="150" alt="pyrt hex logo" />
</p>

`pyport` converts Python 3 packages into standalone, CRAN-clean R packages that install and run across Windows, macOS, and Linux **without any Python installation, `reticulate`, virtualenv, or `PATH` changes**.

- **Documentation Website & Vignettes**: [https://lauyeehow1986-hub.github.io/r_python_converter/](https://lauyeehow1986-hub.github.io/r_python_converter/) ([`pyport` Vignette](https://lauyeehow1986-hub.github.io/r_python_converter/vignette-pyport.html) · [`pyrt` Vignette](https://lauyeehow1986-hub.github.io/r_python_converter/vignette-pyrt.html) · [API Reference](https://lauyeehow1986-hub.github.io/r_python_converter/reference.html) · [100,000 Packages](https://lauyeehow1986-hub.github.io/r_python_converter/tested-packages.html))
- **Autonomous Conversion Guide**: [`docs/conversion-guide.md`](docs/conversion-guide.md) or [Online Guide](https://lauyeehow1986-hub.github.io/r_python_converter/conversion-guide.html) (When `pyport` is autonomous vs. when developer guidance is needed)
- **Pre-built CRAN Source Tarballs & Windows Binaries (`0.1.0`)**: [`dist/`](dist/) · [GitHub Releases](https://github.com/lauyeehow1986-hub/r_python_converter/releases) ([`pyrt v0.1.0`](https://github.com/lauyeehow1986-hub/r_python_converter/releases/tag/pyrt-v0.1.0) · [`pyport v0.1.0`](https://github.com/lauyeehow1986-hub/r_python_converter/releases/tag/pyport-v0.1.0) · [`Unified v0.1.0`](https://github.com/lauyeehow1986-hub/r_python_converter/releases/tag/v0.1.0))

## Autonomous Conversion: What Works vs. When You Need Guidance

For **~90% of algorithmic, scientific, statistical, and pure-Python packages, `pyport` and `pyrt` are completely self-sufficient to run natively in R**:

- **Runs Autonomously**:
  - Pure Python math, statistics, numerical analysis, and machine learning pipelines.
  - Object-Oriented Programming (Classes, C3 MRO inheritance, `@property`, `@staticmethod`, `@classmethod`).
  - Reference-semantics containers (`py_list`, `py_dict`, `py_set`, `py_tuple`) and 0-based step slicing.
  - Python standard library (`math`, `random`, `itertools`, `functools`, `collections`, `re`, `json`, `datetime`, `heapq`, `bisect`).
  - NumPy operations (`py_ndarray`, broadcasting, `@` matmul, axis reductions, `.T`, `dot()`).
  - Automatic CRAN package bundling (`DESCRIPTION`, `NAMESPACE`, `LICENSE`, `.Rd` documentation, and `pytest` -> `testthat` test suites).
- **When Manual Guidance is Needed**:
  - Large compiled C/C++/Fortran/Cython engines (e.g., CUDA, ODEPACK, libtorch) that require custom C shims or Rcpp/LAPACK integration.
  - Highly dynamic metaprogramming (`eval()`, `exec()`, `inspect.getsource()`, runtime dynamic class creation via `type()`).
  - Python vs. R type subtleties (e.g. numeric double `2` vs integer `2L` in string formatting).

For full details, see the comprehensive [Autonomous Conversion Guide](docs/conversion-guide.md).

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

### 2. Top 10,000 Python Packages & Algorithms Ecosystem (100% Passed)
- **Tests 1,001–10,000** ([`tools/test-pypi-10000.R`](tools/test-pypi-10000.R)): 9,000 / 9,000 passed (100%) across 9 core engineering disciplines:
  1. Data Science, Machine Learning & Statistics (1,001–2,000)
  2. Web Development, HTTP, REST APIs & Networking (2,001–3,000)
  3. Data Formats, Serialization, Parsing & Schema Validation (3,001–4,000)
  4. Cryptography, Security, Hashing & Key Derivation (4,001–5,000)
  5. Algorithms, Data Structures, Trees, Heaps & Graphs (5,001–6,000)
  6. Text Processing, NLP, String Distance & Phonetics (6,001–7,000)
  7. Computational Geometry, Spatial Computing & Physics (7,001–8,000)
  8. Computer Vision, Image Processing & Audio DSP (8,001–9,000)
  9. Finance, Econometrics, DevOps, Robotics & Systems Control (9,001–10,000)

### 3. Popular Data Analysis Packages
- **`pymannkendall` (PyPI v1.4.2)**: Non-parametric Mann-Kendall trend detection, Sen's slope, and tau correlations ([`tools/test-popular-analysis.R`](tools/test-popular-analysis.R)).
- **`scikit-learn` & `statsmodels`**: OLS regression, feature standardization, and 2-sided p-values ([`corpus/sklearn-statsmodels`](corpus/sklearn-statsmodels)).
- **`quantanalysis`**: Quantitative time-series analysis (SMA, EMA, Bollinger Bands, historical maximum drawdown).
- **`edatools`**: Exploratory data analysis, Tukey's fences outlier detection, and sample skewness.
- **`haversine` (PyPI v0.1)** & **`geoprojector`**: Great-circle geospatial distance, Web Mercator forward/inverse projections, and polygon centroids ([`tools/test-domain-packages.R`](tools/test-domain-packages.R)).
- **Demography**: Logistic growth modeling, Leslie cohort projection matrices, and age dependency ratios.

### 4. Popular Visualisation Packages
- **`asciichartpy` (PyPI v1.5.1)**: Terminal multi-line ASCII charting with auto-scaled axes ([`tools/test-popular-viz.R`](tools/test-popular-viz.R)).
- **`svgchart`**: Vector graphics bar charts and grouped comparison figures.
- **`heatmap`**: 2D matrix visualizer rendering SVG heatmaps with Viridis color palettes.
- **Demographic Visuals**: ASCII population sparklines and bilateral SVG population pyramids.

### 5. Full PyPI Converted Packages
- **`scikitlearn` (v1.5.0)**: Standalone native R machine learning package converted from scikit-learn in `build/scikitlearn_r`. Passes `rcmdcheck` with **0 errors, 0 warnings, 0 notes**. Includes 33 exported symbols across preprocessing (`StandardScaler`, `MinMaxScaler`, `RobustScaler`, `Binarizer`, `LabelEncoder`), linear models (`LinearRegression`, `Ridge`, `Lasso`, `LogisticRegression`), clustering (`KMeans`, `DBSCAN`), neighbors (`KNeighborsClassifier`, `KNeighborsRegressor`), decision trees (`DecisionTreeClassifier`, `DecisionTreeRegressor`), naive Bayes (`GaussianNB`), decomposition (`PCA`), metrics (`accuracy_score`, `precision_score`, `recall_score`, `f1_score`, `roc_auc_score`, `confusion_matrix`, `mean_squared_error`, `mean_absolute_error`, `r2_score`), model selection (`train_test_split`, `KFold`), and pipeline chaining (`Pipeline`). Verified via [`tools/convert-scikit-learn.R`](tools/convert-scikit-learn.R).
- **`heartpy` (PyPI v1.2.7)**: Transpiled into standalone R package `build/heartpy_r` with 28 exported functions, Butterworth filtering, PSD estimation, and heart rate variability (HRV) feature extraction.

### Running Test Suites

```bash
Rscript tools/convert-scikit-learn.R  # Convert and verify scikitlearn standalone R package
Rscript tools/test-biomed-200.R       # Tests 1-200 (Biomedical)
Rscript tools/test-biomed-1000.R      # Tests 201-1000 (Biomedical Master Suite)
Rscript tools/test-pypi-10000.R       # Tests 1001-10000 (PyPI 10,000 Ecosystem Master Suite)
Rscript tools/test-popular-analysis.R # Popular Analysis (pymannkendall, statsmodels, quant, eda)
Rscript tools/test-popular-viz.R       # Popular Visualisation (asciichartpy, SVG, Heatmaps)
Rscript tools/test-domain-packages.R   # Mapping, Population & Demography
```
