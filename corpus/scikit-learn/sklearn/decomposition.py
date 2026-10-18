# sklearn/decomposition.py - Principal Component Analysis (PCA)

import math

def _mat_vec_mul(A, v):
    p = len(v)
    return [sum(A[i][j] * v[j] for j in range(p)) for i in range(len(A))]


def _dot(v1, v2):
    return sum(v1[i] * v2[i] for i in range(len(v1)))


def _power_iteration(A, num_iter=100):
    p = len(A)
    # Initial vector
    v = [1.0 / math.sqrt(p)] * p
    for _ in range(num_iter):
        w = _mat_vec_mul(A, v)
        norm = math.sqrt(_dot(w, w))
        if norm < 1e-12:
            break
        v = [x / norm for x in w]
    # Rayleigh quotient for eigenvalue
    Av = _mat_vec_mul(A, v)
    eigenval = _dot(v, Av)
    return eigenval, v


class PCA:
    """Principal component analysis (PCA)."""
    def __init__(self, n_components=2):
        self.n_components = int(n_components)
        self.mean_ = []
        self.components_ = []
        self.explained_variance_ = []
        self.explained_variance_ratio_ = []

    def fit(self, X, y=None):
        n = len(X)
        p = len(X[0])
        k = min(self.n_components, p, n)

        # Center X
        means = [sum(float(row[j]) for row in X) / float(n) for j in range(p)]
        self.mean_ = means

        Xc = [[float(X[i][j]) - means[j] for j in range(p)] for i in range(n)]

        # Sample Covariance matrix C (p x p)
        C = [[0.0] * p for _ in range(p)]
        denom = float(max(1, n - 1))
        for i in range(n):
            row = Xc[i]
            for j in range(p):
                for m in range(j, p):
                    C[j][m] += row[j] * row[m]

        for j in range(p):
            for m in range(j, p):
                val = C[j][m] / denom
                C[j][m] = val
                C[m][j] = val

        # Total variance
        total_var = sum(C[j][j] for j in range(p))
        if total_var < 1e-12:
            total_var = 1e-12

        # Extract top k components via power iteration with deflation
        components = []
        explained_var = []
        explained_ratio = []

        C_deflated = [list(row) for row in C]
        for _ in range(k):
            val, vec = _power_iteration(C_deflated, num_iter=80)
            components.append(vec)
            explained_var.append(max(0.0, val))
            explained_ratio.append(max(0.0, val) / total_var)

            # Deflate: C = C - val * (vec * vec^T)
            for r in range(p):
                for c in range(p):
                    C_deflated[r][c] -= val * vec[r] * vec[c]

        self.components_ = components
        self.explained_variance_ = explained_var
        self.explained_variance_ratio_ = explained_ratio
        return self

    def transform(self, X):
        out = []
        p = len(self.mean_)
        for row in X:
            centered = [float(row[j]) - self.mean_[j] for j in range(p)]
            proj = [_dot(comp, centered) for comp in self.components_]
            out.append(proj)
        return out

    def fit_transform(self, X, y=None):
        return self.fit(X, y).transform(X)

    def inverse_transform(self, X):
        out = []
        p = len(self.mean_)
        k = len(self.components_)
        for row in X:
            reconstructed = list(self.mean_)
            for comp_idx in range(k):
                coeff = float(row[comp_idx])
                for j in range(p):
                    reconstructed[j] += coeff * self.components_[comp_idx][j]
            out.append(reconstructed)
        return out
