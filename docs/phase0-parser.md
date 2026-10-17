# Phase 0 — Parser & AST design notes

## Why vendor `tree-sitter-python` inside `pyport`?
- The CRAN package `treesitter` (≥ 0.3.0) embeds the Tree-sitter C runtime
  (v0.24.x, supporting grammar ABI versions 13–14), but a `treesitter.python`
  grammar package is not on CRAN.
- `pyport` therefore vendors `parser.c`, `scanner.c`, and the three header files
  from `tree-sitter-python` v0.23.6 (MIT license, ABI 14) under
  `packages/pyport/src/` via `tools/vendor-grammar.R`.
- Two CRAN-portability edits are applied automatically by `tools/vendor-grammar.R`:
  1. `#pragma` is rewritten as `# pragma` (matching `treesitter.r` on CRAN so
     `R CMD check` does not flag diagnostic-suppression pragmas).
  2. Statement-level `array_pop(...)` calls in `scanner.c` are cast to `(void)`
     to silence `-Wunused-value` under `-Wall`.

## AST representation
- `parse_python()` and `parse_python_file()` return S3 nodes with class
  `c("py_ast_<Kind>", "py_ast_node")` whose node names and field names match
  Python's `ast` module (`Module`, `FunctionDef`, `AsyncFunctionDef`, `ClassDef`,
  `Assign`, `AnnAssign`, `AugAssign`, `For`, `AsyncFor`, `While`, `If`, `With`,
  `AsyncWith`, `Try`, `TryStar`, `Match`, `TypeAlias`, `BinOp`, `BoolOp`,
  `Compare`, `Call`, `JoinedStr`, `FormattedValue`, `Constant`, etc.).
- Source spans (`lineno`, `col_offset`, `end_lineno`, `end_col_offset`) live in
  the `loc` attribute (`ast_loc(node)`).
- Integer literals inside 32-bit signed range (`[-2^31 + 1, 2^31 - 1]`) are
  stored as R `integer`; larger integers are stored as `gmp::bigz` so arbitrary
  precision is preserved without Python.
- Floating-point literals are parsed with C `strtod()` (`ffi_strtod`) and
  formatted with the shortest decimal string that round-trips to the exact
  IEEE-754 double, matching CPython's `ast.dump()`.
