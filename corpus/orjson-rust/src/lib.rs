use pyo3::prelude::*;

#[pyfunction]
fn json_quote_int(x: i64) -> PyResult<i64> {
    Ok(x)
}

#[pyfunction]
fn json_byte_len(n: i64, overhead: i64) -> i64 {
    n + overhead
}
