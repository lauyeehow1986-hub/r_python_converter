# sklearn/linear_model.py - Linear regression, Ridge, Lasso, and Logistic Regression

import math

def _solve_linear_system(A, b):
    """Solve A * x = b via Gaussian elimination with partial pivoting."""
    n = len(A)
    # Augment matrix [A | b]
    M = []
    for i in range(n):
        M.append([float(x) for x in A[i]] + [float(b[i])])

    for i in range(n):
        # Pivot selection
        max_row = i
        max_val = abs(M[i][i])
        for k in range(i + 1, n):
            if abs(M[k][i]) > max_val:
                max_val = abs(M[k][i])
                max_row = k
        if max_row != i:
            M[i], M[max_row] = M[max_row], M[i]

        pivot = M[i][i]
        if abs(pivot) < 1e-12:
            pivot = 1e-12 if pivot >= 0 else -1e-12

        for j in range(i, n + 1):
            M[i][j] /= pivot

        for k in range(n):
            if k != i:
                factor = M[k][i]
                for j in range(i, n + 1):
                    M[k][j] -= factor * M[i][j]

    return [M[i][n] for i in range(n)]


class LinearRegression:
    """Ordinary least squares Linear Regression."""
    def __init__(self, fit_intercept=True):
        self.fit_intercept = fit_intercept
        self.coef_ = []
        self.intercept_ = 0.0

    def fit(self, X, y):
        n_samples = len(X)
        n_features = len(X[0])

        if self.fit_intercept:
            # Add column of 1s
            X_aug = [[1.0] + [float(v) for v in row] for row in X]
            p = n_features + 1
        else:
            X_aug = [[float(v) for v in row] for row in X]
            p = n_features

        # Compute X^T * X (p x p) and X^T * y (p x 1)
        XtX = [[0.0] * p for _ in range(p)]
        Xty = [0.0] * p

        for i in range(n_samples):
            row = X_aug[i]
            yi = float(y[i])
            for j in range(p):
                rj = row[j]
                Xty[j] += rj * yi
                for k in range(j, p):
                    XtX[j][k] += rj * row[k]

        # Symmetrize
        for j in range(p):
            for k in range(j + 1, p):
                XtX[k][j] = XtX[j][k]

        # Regularize slightly for numerical stability if ill-conditioned
        for j in range(p):
            XtX[j][j] += 1e-9

        beta = _solve_linear_system(XtX, Xty)

        if self.fit_intercept:
            self.intercept_ = beta[0]
            self.coef_ = beta[1:]
        else:
            self.intercept_ = 0.0
            self.coef_ = beta
        return self

    def predict(self, X):
        preds = []
        for row in X:
            val = self.intercept_
            for j in range(len(self.coef_)):
                val += self.coef_[j] * float(row[j])
            preds.append(val)
        return preds

    def score(self, X, y):
        preds = self.predict(X)
        n = len(y)
        if n <= 1:
            return 1.0
        y_mean = sum(float(yi) for yi in y) / float(n)
        ss_tot = sum((float(y[i]) - y_mean) ** 2 for i in range(n))
        ss_res = sum((float(y[i]) - preds[i]) ** 2 for i in range(n))
        if ss_tot == 0.0:
            return 1.0 if ss_res == 0.0 else 0.0
        return 1.0 - (ss_res / ss_tot)


class Ridge:
    """Linear least squares with L2 regularization."""
    def __init__(self, alpha=1.0, fit_intercept=True):
        self.alpha = float(alpha)
        self.fit_intercept = fit_intercept
        self.coef_ = []
        self.intercept_ = 0.0

    def fit(self, X, y):
        n_samples = len(X)
        n_features = len(X[0])

        if self.fit_intercept:
            X_aug = [[1.0] + [float(v) for v in row] for row in X]
            p = n_features + 1
        else:
            X_aug = [[float(v) for v in row] for row in X]
            p = n_features

        XtX = [[0.0] * p for _ in range(p)]
        Xty = [0.0] * p

        for i in range(n_samples):
            row = X_aug[i]
            yi = float(y[i])
            for j in range(p):
                Xty[j] += row[j] * yi
                for k in range(j, p):
                    XtX[j][k] += row[j] * row[k]

        for j in range(p):
            for k in range(j + 1, p):
                XtX[k][j] = XtX[j][k]

        # Add alpha * I (do not penalize intercept if present)
        start_penalty = 1 if self.fit_intercept else 0
        for j in range(start_penalty, p):
            XtX[j][j] += self.alpha

        beta = _solve_linear_system(XtX, Xty)

        if self.fit_intercept:
            self.intercept_ = beta[0]
            self.coef_ = beta[1:]
        else:
            self.intercept_ = 0.0
            self.coef_ = beta
        return self

    def predict(self, X):
        preds = []
        for row in X:
            val = self.intercept_
            for j in range(len(self.coef_)):
                val += self.coef_[j] * float(row[j])
            preds.append(val)
        return preds

    def score(self, X, y):
        preds = self.predict(X)
        n = len(y)
        if n <= 1:
            return 1.0
        y_mean = sum(float(yi) for yi in y) / float(n)
        ss_tot = sum((float(y[i]) - y_mean) ** 2 for i in range(n))
        ss_res = sum((float(y[i]) - preds[i]) ** 2 for i in range(n))
        if ss_tot == 0.0:
            return 1.0 if ss_res == 0.0 else 0.0
        return 1.0 - (ss_res / ss_tot)


class Lasso:
    """Linear Model trained with L1 prior as regularizer (coordinate descent)."""
    def __init__(self, alpha=1.0, max_iter=500, tol=1e-4):
        self.alpha = float(alpha)
        self.max_iter = max_iter
        self.tol = tol
        self.coef_ = []
        self.intercept_ = 0.0

    def fit(self, X, y):
        n = len(X)
        p = len(X[0])
        y_mean = sum(float(v) for v in y) / float(n)

        # Center features and target
        x_means = [sum(float(row[j]) for row in X) / float(n) for j in range(p)]
        Xc = [[float(X[i][j]) - x_means[j] for j in range(p)] for i in range(n)]
        yc = [float(y[i]) - y_mean for i in range(n)]

        # Precompute sum of squares for each feature
        z = [sum(Xc[i][j] ** 2 for i in range(n)) for j in range(p)]
        for j in range(p):
            if z[j] < 1e-12:
                z[j] = 1e-12

        w = [0.0] * p
        r = list(yc)

        for _ in range(self.max_iter):
            max_change = 0.0
            for j in range(p):
                # Add back current feature contribution to residuals
                wj_old = w[j]
                rho_j = 0.0
                for i in range(n):
                    r_no_j = r[i] + Xc[i][j] * wj_old
                    rho_j += Xc[i][j] * r_no_j

                # Soft thresholding
                gamma = self.alpha * float(n)
                if rho_j > gamma:
                    w_new = (rho_j - gamma) / z[j]
                elif rho_j < -gamma:
                    w_new = (rho_j + gamma) / z[j]
                else:
                    w_new = 0.0

                diff = abs(w_new - wj_old)
                if diff > max_change:
                    max_change = diff

                # Update residuals
                for i in range(n):
                    r[i] -= Xc[i][j] * (w_new - wj_old)
                w[j] = w_new

            if max_change < self.tol:
                break

        self.coef_ = w
        self.intercept_ = y_mean - sum(w[j] * x_means[j] for j in range(p))
        return self

    def predict(self, X):
        preds = []
        for row in X:
            val = self.intercept_
            for j in range(len(self.coef_)):
                val += self.coef_[j] * float(row[j])
            preds.append(val)
        return preds


class LogisticRegression:
    """Logistic Regression classifier (gradient descent)."""
    def __init__(self, C=1.0, max_iter=200, fit_intercept=True, lr=0.1):
        self.C = float(C)
        self.lr = float(lr)
        self.max_iter = max_iter
        self.fit_intercept = fit_intercept
        self.coef_ = []
        self.intercept_ = 0.0
        self.classes_ = [0, 1]

    def _sigmoid(self, z):
        if z < -20.0:
            return 0.0
        if z > 20.0:
            return 1.0
        return 1.0 / (1.0 + math.exp(-z))

    def fit(self, X, y):
        n = len(X)
        p = len(X[0])
        w = [0.0] * p
        b = 0.0

        for _ in range(self.max_iter):
            grad_w = [0.0] * p
            grad_b = 0.0

            for i in range(n):
                z = b
                for j in range(p):
                    z += w[j] * float(X[i][j])
                p_i = self._sigmoid(z)
                err = p_i - float(y[i])

                for j in range(p):
                    grad_w[j] += err * float(X[i][j])
                if self.fit_intercept:
                    grad_b += err

            inv_n = self.lr / float(n)
            for j in range(p):
                w[j] -= grad_w[j] * inv_n
            if self.fit_intercept:
                b -= grad_b * inv_n

        self.coef_ = w
        self.intercept_ = b
        return self

    def predict_proba(self, X):
        probs = []
        for row in X:
            z = self.intercept_
            for j in range(len(self.coef_)):
                z += self.coef_[j] * float(row[j])
            p1 = self._sigmoid(z)
            probs.append([1.0 - p1, p1])
        return probs

    def predict(self, X):
        probs = self.predict_proba(X)
        return [1 if p[1] >= 0.5 else 0 for p in probs]

    def score(self, X, y):
        preds = self.predict(X)
        correct = sum(1 for i in range(len(y)) if preds[i] == int(y[i]))
        return float(correct) / float(len(y))
