test_that("transpiled scikit-learn / statsmodels style Python code runs natively with numpy and scipy", {
  # 1. Ordinary Least Squares (OLS) with t-statistics and R^2 (statsmodels style)
  ols_mod <- eval_python("
import numpy as np
from scipy import stats

class OLSModel:
    def __init__(self, X, y):
        self.X = np.asarray(X)
        self.y = np.asarray(y)

    def fit(self):
        X = self.X
        y = self.y
        n = X.shape[0]
        p = X.shape[1]
        xtx_inv = np.linalg.inv(X.T @ X)
        beta = xtx_inv @ (X.T @ y)
        y_hat = X @ beta
        resid = y - y_hat
        rss = np.sum(resid ** 2)
        tss = np.sum((y - np.mean(y)) ** 2)
        r_squared = 1.0 - (rss / tss)
        sigma2 = rss / (n - p)
        se = np.sqrt(np.diag(xtx_inv) * sigma2)
        t_stats = beta / se
        return {
            'params': beta.tolist(),
            'r_squared': r_squared,
            't_values': t_stats.tolist()
        }
")

  # y = 1.5 + 2.5 * x
  X_data <- list(list(1.0, 1.0), list(1.0, 2.0), list(1.0, 3.0), list(1.0, 4.0), list(1.0, 5.0))
  y_data <- list(4.1, 6.4, 9.0, 11.6, 13.9)
  model <- pyrt::py_call(ols_mod$OLSModel, list(X_data, y_data))
  res <- pyrt::py_getattr(model, "fit")()
  params <- pyrt::py_getitem(res, "params")$as_r_list()
  r2 <- pyrt::py_getitem(res, "r_squared")

  # Compare against native R lm()
  r_fit <- stats::lm(c(4.1, 6.4, 9.0, 11.6, 13.9) ~ c(1, 2, 3, 4, 5))
  expect_equal(unlist(params), unname(coef(r_fit)), tolerance = 1e-10)
  expect_equal(r2, summary(r_fit)$r.squared, tolerance = 1e-10)

  # 2. StandardScaler + Ridge Regression (scikit-learn style)
  sk_mod <- eval_python("
import numpy as np

class StandardScaler:
    def fit(self, X):
        arr = np.asarray(X)
        self.mean_ = arr.mean(axis=0)
        self.scale_ = arr.std(axis=0)
        return self

    def transform(self, X):
        arr = np.asarray(X)
        return (arr - self.mean_) / self.scale_

    def fit_transform(self, X):
        return self.fit(X).transform(X)

class Ridge:
    def __init__(self, alpha=1.0):
        self.alpha = alpha

    def fit(self, X, y):
        X_arr = np.asarray(X)
        y_arr = np.asarray(y)
        n_features = X_arr.shape[1]
        A = (X_arr.T @ X_arr) + self.alpha * np.eye(n_features)
        b = X_arr.T @ y_arr
        self.coef_ = np.linalg.solve(A, b)
        return self

    def predict(self, X):
        X_arr = np.asarray(X)
        return X_arr @ self.coef_
")

  scaler <- pyrt::py_call(sk_mod$StandardScaler)
  X_scaled <- pyrt::py_getattr(scaler, "fit_transform")(list(list(1.0, 10.0), list(2.0, 20.0), list(3.0, 30.0)))
  expect_equal(X_scaled$mean(axis = 0L)$tolist()$as_r_list(), list(0.0, 0.0), tolerance = 1e-12)
  expect_equal(X_scaled$std(axis = 0L)$tolist()$as_r_list(), list(1.0, 1.0), tolerance = 1e-12)

  ridge <- pyrt::py_call(sk_mod$Ridge, list(alpha = 0.5))
  pyrt::py_getattr(ridge, "fit")(list(list(1.0, 0.0), list(0.0, 1.0), list(1.0, 1.0)), list(2.0, 3.0, 5.0))
  preds <- pyrt::py_getattr(ridge, "predict")(list(list(1.0, 0.0), list(0.0, 1.0)))
  expect_length(preds$tolist()$as_r_list(), 2L)
})
