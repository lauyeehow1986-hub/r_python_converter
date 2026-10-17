# pyport 0.1.0

* Initial CRAN release of `pyport`: converts Python packages (`.py`, source directories, `.tar.gz` sdists, `.whl` wheels) into standalone, native R packages (`.tar.gz` source tarballs and `.zip`/`.tgz` binaries) that run without Python or `reticulate`.
* Offline Python parser (`parse_python`, `parse_python_file`, `ast_dump`) powered by `treesitter` and a vendored `tree-sitter-python` v0.23.6 C grammar (ABI 14).
* Lexical scope analyzer (`analyze_python`), escape analyzer (`analyze_escape`), and R code transpiler (`transpile_python`, `eval_python`) with fast-path `range()` loop lowering.
* Standalone R package generator (`convert`, `build_r_package`), license auditor (`check_python_license`), and `pytest`/`unittest` -> `testthat` test suite translator (`convert_pytest_code`).
* Native C/C++/Cython (`cpython_shim_header`, `transpile_c_extension` supporting `<Python.h>`, `PyMethodDef`, `PyArg_ParseTuple`, and Cython `cdef`/`cpdef`) and Rust `PyO3` -> `extendr` (`transpile_pyo3_rust` supporting `#[pyfunction]`, `#[pyclass]`, `#[pymethods]`) bridges.
