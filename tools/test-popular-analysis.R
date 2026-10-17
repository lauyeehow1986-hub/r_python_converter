#!/usr/bin/env Rscript
# tools/test-popular-analysis.R
#
# Comprehensive test suite converting and executing popular Python data analysis
# and statistical modeling packages using pyport and pyrt.

suppressPackageStartupMessages({
  pkgload::load_all("packages/pyrt", quiet = TRUE)
  pkgload::load_all("packages/pyport", quiet = TRUE)
  library(testthat)
})

cat("=======================================================================\n")
cat(" popular analysis packages testing with pyport & pyrt                   \n")
cat("=======================================================================\n\n")

cache_dir <- file.path("build", "pypi_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
source("tools/convert-pypi.R")

# -----------------------------------------------------------------------------
# 1. PyPI 'pymannkendall' (Popular Time-Series Trend & Statistical Analysis)
# -----------------------------------------------------------------------------
cat("[1/4] Testing PyPI 'pymannkendall' (Mann-Kendall Trend Analysis)...\n")

pmk_sdist <- file.path(cache_dir, "pymannkendall-1.0.tar.gz")
if (!file.exists(pmk_sdist)) {
  pmk_sdist <- download_pypi_sdist("pymannkendall", cache_dir)
}

pmk_out <- file.path("build", "pymannkendall_r")
res_pmk <- pyport::convert(pmk_sdist, pmk_out)
cat("  - Converted pymannkendall (", length(res_pmk$exports), " exports, license: ", res_pmk$license$cran_license, ")\n", sep = "")

pkgload::load_all(res_pmk$path, quiet = TRUE)

# Upward monotonic time series data
upward_data <- list(12.5, 14.1, 15.8, 17.2, 19.0, 21.4, 23.8, 26.1, 28.5, 31.0)
mk_res <- original_test(upward_data)

cat(sprintf("  - Mann-Kendall result: trend = '%s', p-value = %.5f, slope = %.3f\n",
            mk_res$trend, mk_res$p, mk_res$slope))
expect_identical(mk_res$trend, "increasing")
expect_true(mk_res$p < 0.01)
expect_true(mk_res$slope > 1.5)

# Flat / non-trending random data
flat_data <- list(10.1, 9.9, 10.2, 10.0, 9.8, 10.1, 10.0, 9.9, 10.1, 10.0)
mk_flat <- original_test(flat_data)
cat(sprintf("  - Flat data result: trend = '%s', p-value = %.4f\n",
            mk_flat$trend, mk_flat$p))
expect_identical(mk_flat$trend, "no trend")
cat("  ✓ PyPI 'pymannkendall' statistical trend tests passed.\n\n")

# -----------------------------------------------------------------------------
# 2. Statistical Modeling & OLS Regression (sklearn-statsmodels corpus)
# -----------------------------------------------------------------------------
cat("[2/4] Testing Statistical Modeling & OLS Regression (sklearn-statsmodels)...\n")

sm_out <- file.path("build", "statmodels_r")
res_sm <- pyport::convert("corpus/sklearn-statsmodels", sm_out)
cat("  - Converted corpus/sklearn-statsmodels\n")

pkgload::load_all(res_sm$path, quiet = TRUE)

# Test 1: Standardization
raw_vals <- list(10.0, 20.0, 30.0, 40.0, 50.0)
std_vals <- standardize_1d(raw_vals)
std_num <- as.numeric(std_vals$as_r_list())
expect_equal(round(mean(std_num), 5), 0.0)
expect_equal(round(sqrt(mean((std_num - mean(std_num))^2)), 5), 1.0)
cat("  - Standardization (Z-score): mean = 0, sd = 1 verified\n")

# Test 2: Multi-variable OLS regression: y = 2.0*x1 + 3.0*x2 + 5.0
X <- list(
  list(1.0, 1.0, 1.0),
  list(1.0, 2.0, 1.0),
  list(1.0, 1.0, 2.0),
  list(1.0, 3.0, 2.0),
  list(1.0, 4.0, 3.0)
)
# y = 5*1 + 2*x1 + 3*x2
y <- list(10.0, 12.0, 13.0, 17.0, 22.0)
beta_raw <- ols_fit(X, y)
beta <- as.numeric(beta_raw$as_r_list())
cat(sprintf("  - OLS coefficients: intercept = %.2f, b1 = %.2f, b2 = %.2f\n",
            beta[[1]], beta[[2]], beta[[3]]))
expect_equal(round(beta[[1]], 1), 5.0)
expect_equal(round(beta[[2]], 1), 2.0)
expect_equal(round(beta[[3]], 1), 3.0)

# Test 3: Normal two-sided p-value
pval_z2 <- normal_two_sided_pvalue(1.96)
cat(sprintf("  - Normal 2-sided p-value at z = 1.96: %.4f (expected ~0.05)\n", pval_z2))
expect_true(abs(pval_z2 - 0.05) < 0.005)
cat("  ✓ Statistical modeling and OLS regression verified.\n\n")

# -----------------------------------------------------------------------------
# 3. Financial & Technical Time Series Analysis (quant_analysis)
# -----------------------------------------------------------------------------
cat("[3/4] Testing Financial & Technical Time Series Analysis...\n")

py_quant_dir <- file.path("build", "quant_py")
dir.create(py_quant_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(r"---(
import math

class TechnicalAnalysis:
    """Quantitative time series indicators: SMA, EMA, Bollinger Bands, and RSI."""

    @staticmethod
    def sma(series, window):
        n = len(series)
        if n < window or window <= 0:
            return []
        res = []
        for i in range(window - 1, n):
            sub = series[i - window + 1 : i + 1]
            res.append(round(sum(sub) / float(window), 4))
        return res

    @staticmethod
    def ema(series, window):
        n = len(series)
        if n < window or window <= 0:
            return []
        alpha = 2.0 / (window + 1.0)
        # Seed EMA with initial SMA
        ema_val = sum(series[:window]) / float(window)
        res = [round(ema_val, 4)]
        for i in range(window, n):
            ema_val = alpha * series[i] + (1.0 - alpha) * ema_val
            res.append(round(ema_val, 4))
        return res

    @staticmethod
    def bollinger_bands(series, window=5, num_std=2.0):
        n = len(series)
        if n < window:
            return {"mid": [], "upper": [], "lower": []}
        mid = []
        upper = []
        lower = []
        for i in range(window - 1, n):
            sub = series[i - window + 1 : i + 1]
            m = sum(sub) / float(window)
            var = sum((x - m) ** 2 for x in sub) / float(window)
            s = math.sqrt(var)
            mid.append(round(m, 2))
            upper.append(round(m + num_std * s, 2))
            lower.append(round(m - num_std * s, 2))
        return {"mid": mid, "upper": upper, "lower": lower}

    @staticmethod
    def max_drawdown(prices):
        """Calculates Maximum Drawdown (MDD) percentage."""
        if not prices:
            return 0.0
        peak = prices[0]
        max_dd = 0.0
        for p in prices:
            if p > peak:
                peak = p
            dd = (peak - p) / float(peak) if peak > 0 else 0.0
            if dd > max_dd:
                max_dd = dd
        return round(max_dd * 100.0, 2)
)---", file.path(py_quant_dir, "quant.py"))

quant_out <- file.path("build", "quant_r")
res_quant <- pyport::convert(py_quant_dir, quant_out, pkg_name = "quantanalysis")
cat("  - Converted quantanalysis package\n")

pkgload::load_all(res_quant$path, quiet = TRUE)

ta <- TechnicalAnalysis$new()
prices <- list(100.0, 102.0, 101.0, 105.0, 107.0, 110.0, 108.0, 104.0, 98.0, 95.0, 99.0, 103.0)

# SMA (window = 5)
sma_res <- ta$sma(prices, 5L)$as_r_list()
cat("  - 5-period SMA:", paste(head(sma_res, 3), collapse = ", "), "...\n")
expect_equal(length(sma_res), length(prices) - 4L)
expect_equal(sma_res[[1]], round(mean(c(100, 102, 101, 105, 107)), 4))

# Bollinger Bands
bands <- ta$bollinger_bands(prices, 5L, 2.0)
mid_vals <- py_getitem(bands, "mid")$as_r_list()
upper_vals <- py_getitem(bands, "upper")$as_r_list()
lower_vals <- py_getitem(bands, "lower")$as_r_list()
cat(sprintf("  - Bollinger Bands (t = -1): Upper = %.2f, Mid = %.2f, Lower = %.2f\n",
            tail(upper_vals, 1)[[1]], tail(mid_vals, 1)[[1]], tail(lower_vals, 1)[[1]]))
expect_true(tail(upper_vals, 1)[[1]] > tail(mid_vals, 1)[[1]])
expect_true(tail(mid_vals, 1)[[1]] > tail(lower_vals, 1)[[1]])

# Max Drawdown
mdd <- ta$max_drawdown(prices)
cat(sprintf("  - Maximum Drawdown (Peak 110 -> Trough 95): %.2f%% (expected 13.64%%)\n", mdd))
expect_equal(mdd, 13.64)
cat("  ✓ Quantitative time series analysis verified.\n\n")

# -----------------------------------------------------------------------------
# 4. Exploratory Data Analysis & Outlier Detection (eda_tools)
# -----------------------------------------------------------------------------
cat("[4/4] Testing Exploratory Data Analysis & Outlier Detection...\n")

py_eda_dir <- file.path("build", "eda_py")
dir.create(py_eda_dir, recursive = TRUE, showWarnings = FALSE)
writeLines(r"---(
import math

class EdaAnalyzer:
    """Exploratory Data Analysis: Quantiles, IQR, Outlier Fences, and Skewness."""

    @staticmethod
    def percentile(values, p):
        """Computes p-th percentile (0 <= p <= 100) using linear interpolation."""
        if not values:
            return 0.0
        sorted_v = sorted(values)
        k = (len(sorted_v) - 1) * (p / 100.0)
        f = math.floor(k)
        c = math.ceil(k)
        if f == c:
            return sorted_v[int(k)]
        d0 = sorted_v[int(f)] * (c - k)
        d1 = sorted_v[int(c)] * (k - f)
        return round(d0 + d1, 4)

    def five_number_summary(self, values):
        return {
            "min": min(values),
            "q1": self.percentile(values, 25.0),
            "median": self.percentile(values, 50.0),
            "q3": self.percentile(values, 75.0),
            "max": max(values)
        }

    def detect_outliers_tukey(self, values):
        """Identifies outliers beyond 1.5 * IQR."""
        q1 = self.percentile(values, 25.0)
        q3 = self.percentile(values, 75.0)
        iqr = q3 - q1
        lower_fence = q1 - 1.5 * iqr
        upper_fence = q3 + 1.5 * iqr
        outliers = []
        for x in values:
            if x < lower_fence or x > upper_fence:
                outliers.append(x)
        return {
            "lower_fence": lower_fence,
            "upper_fence": upper_fence,
            "outliers": outliers
        }

    @staticmethod
    def skewness(values):
        """Fisher-Pearson coefficient of skewness."""
        n = len(values)
        if n < 3:
            return 0.0
        m = sum(values) / float(n)
        m2 = sum((x - m) ** 2 for x in values) / float(n)
        m3 = sum((x - m) ** 3 for x in values) / float(n)
        if m2 <= 0:
            return 0.0
        g1 = m3 / (m2 ** 1.5)
        # Sample correction
        val = (math.sqrt(n * (n - 1)) / (n - 2)) * g1
        return round(val, 4)
)---", file.path(py_eda_dir, "eda.py"))

eda_out <- file.path("build", "eda_r")
res_eda <- pyport::convert(py_eda_dir, eda_out, pkg_name = "edatools")
cat("  - Converted edatools package\n")

pkgload::load_all(res_eda$path, quiet = TRUE)

eda <- EdaAnalyzer$new()
sample_data <- list(12, 14, 15, 16, 17, 18, 19, 20, 22, 23, 25, 95) # 95 is a clear outlier

fivenum <- eda$five_number_summary(sample_data)
cat(sprintf("  - Five-number summary: Min = %d, Q1 = %.1f, Median = %.1f, Q3 = %.1f, Max = %d\n",
            py_getitem(fivenum, "min"), py_getitem(fivenum, "q1"),
            py_getitem(fivenum, "median"), py_getitem(fivenum, "q3"),
            py_getitem(fivenum, "max")))
expect_equal(py_getitem(fivenum, "min"), 12)
expect_equal(py_getitem(fivenum, "max"), 95)

outlier_res <- eda$detect_outliers_tukey(sample_data)
detected <- py_getitem(outlier_res, "outliers")$as_r_list()
cat("  - Outliers detected via Tukey's fences:", paste(detected, collapse = ", "), "\n")
expect_equal(detected[[1]], 95)

skew_val <- eda$skewness(sample_data)
cat(sprintf("  - Sample skewness (right-skewed by 95): %.3f\n", skew_val))
expect_true(skew_val > 2.0)
cat("  ✓ Exploratory data analysis and outlier detection verified.\n\n")

cat("=======================================================================\n")
cat(" ALL TESTS PASSED: Popular analysis Python packages run cleanly in     \n")
cat(" native R under pyport and pyrt!                                       \n")
cat("=======================================================================\n")
