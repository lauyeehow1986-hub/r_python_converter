import numpy as np
import scipy.stats as stats


def standardize_1d(values):
    arr = np.array(values)
    m = np.mean(arr)
    s = np.std(arr)
    out = []
    for x in values:
        out.append((x - m) / s)
    return out


def ols_fit(X_rows, y_vals):
    X = np.array(X_rows)
    y = np.array(y_vals)
    Xt = np.transpose(X)
    XtX = np.dot(Xt, X)
    Xty = np.dot(Xt, y)
    beta = np.linalg.solve(XtX, Xty)
    return beta.tolist()


def normal_two_sided_pvalue(z):
    if z < 0:
        z = -z
    return 2.0 * (1.0 - stats.norm.cdf(z))
