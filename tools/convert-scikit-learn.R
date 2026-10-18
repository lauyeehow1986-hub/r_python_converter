#!/usr/bin/env Rscript
# tools/convert-scikit-learn.R
#
# Convert scikit-learn into a standalone native R package using pyport & pyrt.
# Runs without Python, reticulate, virtualenv, or system dependencies.

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
  library(testthat)
})

cat("=======================================================================\n")
cat(" CONVERTING SCIKIT-LEARN TO STANDALONE NATIVE R PACKAGE               \n")
cat(" pyport Transpiler & pyrt Runtime Engine                              \n")
cat("=======================================================================\n\n")

src_dir <- file.path("corpus", "scikit-learn")
out_dir <- file.path("build", "scikitlearn_r")

cat(sprintf("Source: %s\nTarget: %s\n\n", src_dir, out_dir))
t_start <- proc.time()[["elapsed"]]

res <- pyport::convert(src_dir, out_dir, pkg_name = "scikitlearn")

desc_path <- file.path(out_dir, "DESCRIPTION")
desc_lines <- readLines(desc_path, encoding = "UTF-8")
desc_lines <- gsub('Authors@R:.*', 'Authors@R: c(person("Yee How", "Lau", role = c("aut", "cre"), email = "lauyeehow1986@gmail.com"), person("scikit-learn developers", role = "cph"))', desc_lines)
writeLines(desc_lines, desc_path)

t_conv <- proc.time()[["elapsed"]]
cat(sprintf("Conversion completed in %.2fs!\n", t_conv - t_start))
cat(sprintf("Package Name: %s\nVersion: %s\nExports: %d symbols\n\n",
            res$pkg_name, res$version, length(res$exports)))

# Load and test the generated R package
cat("Loading converted 'scikitlearn' R package...\n")
pkgload::load_all(out_dir, quiet = TRUE)

cat("Running R verification test suite on converted scikitlearn package...\n")

# 1. StandardScaler & MinMaxScaler
test_that("StandardScaler works natively in R", {
  X <- list(list(1.0, 2.0), list(3.0, 4.0), list(5.0, 6.0))
  scaler <- StandardScaler()
  scaler$fit(X)
  expect_equal(scaler$mean_[[1]], 3.0, tolerance = 1e-4)
  expect_equal(scaler$mean_[[2]], 4.0, tolerance = 1e-4)
  Xt <- scaler$transform(X)
  expect_equal(Xt[[2]][[1]], 0.0, tolerance = 1e-4)
  inv <- scaler$inverse_transform(Xt)
  expect_equal(inv[[1]][[1]], 1.0, tolerance = 1e-4)
})

test_that("MinMaxScaler works natively in R", {
  X <- list(list(0.0, 10.0), list(5.0, 20.0), list(10.0, 30.0))
  scaler <- MinMaxScaler()
  Xt <- scaler$fit_transform(X)
  expect_equal(Xt[[1]][[1]], 0.0, tolerance = 1e-4)
  expect_equal(Xt[[3]][[1]], 1.0, tolerance = 1e-4)
  expect_equal(Xt[[2]][[2]], 0.5, tolerance = 1e-4)
})

# 2. LinearRegression & Ridge
test_that("LinearRegression fits and predicts correctly", {
  # y = 2*x1 + 3*x2 + 5
  X <- list(
    list(1.0, 2.0),
    list(2.0, 1.0),
    list(3.0, 4.0),
    list(4.0, 3.0),
    list(5.0, 5.0)
  )
  y <- list(13.0, 12.0, 23.0, 22.0, 30.0)
  reg <- LinearRegression()
  reg$fit(X, y)
  expect_equal(reg$intercept_, 5.0, tolerance = 0.05)
  expect_equal(reg$coef_[[1]], 2.0, tolerance = 0.05)
  expect_equal(reg$coef_[[2]], 3.0, tolerance = 0.05)
  r2 <- reg$score(X, y)
  expect_true(r2 > 0.99)
})

test_that("Ridge regression fits and predicts with L2 penalty", {
  X <- list(list(1.0, 2.0), list(2.0, 1.0), list(3.0, 4.0), list(4.0, 3.0))
  y <- list(10.0, 12.0, 18.0, 20.0)
  ridge <- Ridge(alpha = 0.5)
  ridge$fit(X, y)
  preds <- ridge$predict(X)
  expect_equal(pyrt::py_len(preds), 4L)
  expect_true(r2_score(y, preds) > 0.90)
})

# 3. LogisticRegression
test_that("LogisticRegression binary classification converges", {
  X <- list(list(1.0, 1.0), list(1.5, 1.2), list(5.0, 5.0), list(5.5, 6.0))
  y <- list(0, 0, 1, 1)
  clf <- LogisticRegression(max_iter = 300)
  clf$fit(X, y)
  preds <- clf$predict(X)
  expect_equal(as.integer(as.list(preds)), c(0L, 0L, 1L, 1L))
  expect_equal(clf$score(X, y), 1.0)
})

# 4. KMeans Clustering
test_that("KMeans clustering identifies separate clusters", {
  X <- list(list(1.0, 1.0), list(1.2, 1.1), list(10.0, 10.0), list(10.2, 9.8))
  km <- KMeans(n_clusters = 2)
  labels <- km$fit_predict(X)
  expect_equal(labels[[1]], labels[[2]])
  expect_equal(labels[[3]], labels[[4]])
  expect_true(labels[[1]] != labels[[3]])
})

# 5. KNeighborsClassifier
test_that("KNeighborsClassifier classifies nearest neighbors", {
  X <- list(list(1.0, 1.0), list(1.5, 1.2), list(8.0, 8.0), list(8.5, 8.2))
  y <- list(0, 0, 1, 1)
  knn <- KNeighborsClassifier(n_neighbors = 1)
  knn$fit(X, y)
  expect_equal(knn$predict(list(list(1.2, 1.1)))[[1]], 0)
  expect_equal(knn$predict(list(list(8.2, 8.1)))[[1]], 1)
})

# 6. DecisionTreeClassifier
test_that("DecisionTreeClassifier fits recursive binary tree", {
  X <- list(list(2.0, 3.0), list(1.0, 1.0), list(8.0, 7.0), list(9.0, 8.0))
  y <- list(0, 0, 1, 1)
  dt <- DecisionTreeClassifier(max_depth = 3)
  dt$fit(X, y)
  preds <- dt$predict(X)
  expect_equal(as.integer(as.list(preds)), as.integer(y))
})

# 7. GaussianNB
test_that("GaussianNB estimates likelihoods and classifies", {
  X <- list(list(1.0, 2.0), list(1.2, 1.8), list(6.0, 7.0), list(6.2, 6.8))
  y <- list(0, 0, 1, 1)
  gnb <- GaussianNB()
  gnb$fit(X, y)
  preds <- gnb$predict(X)
  expect_equal(as.integer(as.list(preds)), as.integer(y))
})

# 8. PCA
test_that("PCA reduces dimensions to top principal component", {
  X <- list(
    list(1.0, 2.0, 3.0),
    list(2.0, 4.0, 6.0),
    list(3.0, 6.0, 9.0),
    list(4.0, 8.0, 12.0)
  )
  pca <- PCA(n_components = 1)
  Xt <- pca$fit_transform(X)
  expect_equal(pyrt::py_len(Xt), 4L)
  expect_equal(pyrt::py_len(Xt[[1]]), 1L)
  expect_true(pca$explained_variance_ratio_[[1]] > 0.95)
})

# 9. Pipeline
test_that("Pipeline chains scaler and classifier seamlessly", {
  X <- list(list(10.0, 20.0), list(20.0, 10.0), list(80.0, 90.0), list(90.0, 80.0))
  y <- list(0, 0, 1, 1)
  pipe <- Pipeline(list(
    list("scaler", StandardScaler()),
    list("clf", LogisticRegression(max_iter = 300))
  ))
  pipe$fit(X, y)
  preds <- pipe$predict(X)
  expect_equal(as.integer(as.list(preds)), as.integer(y))
})

# 10. Evaluation Metrics
test_that("Metrics accuracy_score, confusion_matrix, r2_score work", {
  y_true <- list(1, 0, 1, 1, 0)
  y_pred <- list(1, 0, 1, 0, 0)
  acc <- accuracy_score(y_true, y_pred)
  expect_equal(acc, 0.8)
  cm <- confusion_matrix(y_true, y_pred)
  expect_equal(cm[[1]][[1]], 2) # tn
  expect_equal(cm[[2]][[2]], 2) # tp
  
  y_reg_true <- list(10.0, 20.0, 30.0)
  y_reg_pred <- list(11.0, 19.0, 31.0)
  mse <- mean_squared_error(y_reg_true, y_reg_pred)
  expect_equal(mse, 1.0, tolerance = 1e-4)
})

t_end <- proc.time()[["elapsed"]]
cat(sprintf("\nAll verification tests passed (100%%) in %.2fs!\n", t_end - t_conv))
cat("=======================================================================\n")
cat(" SCIKIT-LEARN CONVERTED SUCCESSFULLY TO STANDALONE R PACKAGE!          \n")
cat("=======================================================================\n")
