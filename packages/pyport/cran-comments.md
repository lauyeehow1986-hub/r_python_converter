## R CMD check results

0 errors | 0 warnings | 1 note

* Note: New submission.
* Dependency: `pyrt` (>= 0.1.0) is already accepted on CRAN.

## Test environments

* Local Windows 11 x64 (R 4.5.2 ucrt, Rtools45 GCC 14.3.0)
* GitHub Actions: Ubuntu latest (R-release, R-devel), macOS latest (R-release), Windows latest (R-release)

## Notes for CRAN reviewers

* `pyport` is the companion transpiler package to `pyrt` (which is already accepted on CRAN).
* `pyport` vendors the MIT-licensed `tree-sitter-python` v0.23.6 C grammar (`src/parser.c`, `src/scanner.c`) so Python source code can be parsed offline without any Python installation. The copyright holders (`Max Brunsfeld` and `Tree-sitter authors`) are credited in `Authors@R` and `LICENSE`.
