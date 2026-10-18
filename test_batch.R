
suppressPackageStartupMessages({ pkgload::load_all('packages/pyrt', quiet=TRUE); pkgload::load_all('packages/pyport', quiet=TRUE); library(testthat) })
code <- '
def f1(x): return x + 1
def f2(x): return x * 2
'
eval(parse(text = as.character(pyport::transpile_python(code))), envir = .GlobalEnv)
expect_equal(f1(10), 11)
expect_equal(f2(10), 20)
cat('BATCH TRANSPILATION WORKS PERFECTLY!
')
