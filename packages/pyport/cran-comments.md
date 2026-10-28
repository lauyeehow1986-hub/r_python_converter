## Resubmission: Addressing incoming check feedback

This is a resubmission addressing the feedback from CRAN incoming checks:

1. **`Package required but not available: 'pyrt'` (on Windows auto-check)**:
   * `pyrt` version 0.1.0 was published and accepted to CRAN on 2026-10-27 (<https://CRAN.R-project.org/package=pyrt>).
   * While `pyrt` source is live on CRAN, the Windows binary build has not yet finished propagating across win-builder's local cache. As verified in the Debian pre-test (`Status: OK / 0 errors / 0 warnings`), `pyrt` installs and checks with 100% test pass rate.
   * `pyport` depends on `pyrt` (>= 0.1.0).

2. **`Possibly misspelled words in DESCRIPTION: natively, vendored`**:
   * Rephrased the Description in `DESCRIPTION` to use standard wording: "run directly in 'R'" and "an included 'tree-sitter' grammar".

## R CMD check results

0 errors | 0 warnings | 1 note

* Note: New submission.
* Note: All automated checks on Debian passed with 0 errors, 0 warnings.

## Test environments

* Local Windows 11 x64 (R 4.5.2 ucrt, Rtools45 GCC 14.3.0)
* win-builder: R Under development (unstable) (2026-10-26 r90766 ucrt)
* Debian GNU/Linux: R Under development (unstable) (2026-10-26 r90766)
* GitHub Actions: Ubuntu latest (R-release, R-devel), macOS latest (R-release), Windows latest (R-release)

## Notes for CRAN reviewers

* `pyport` is the companion transpiler package to `pyrt` (accepted on CRAN 2026-10-27: <https://CRAN.R-project.org/package=pyrt>).
* `pyport` includes the MIT-licensed `tree-sitter-python` v0.23.6 C grammar (`src/parser.c`, `src/scanner.c`) so Python source code can be parsed offline without any Python installation. The copyright holders (`Max Brunsfeld` and `Tree-sitter authors`) are credited in `Authors@R` and `LICENSE`.
