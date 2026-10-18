# sklearn/cluster.py - KMeans and DBSCAN clustering algorithms

import math

def _euclidean_dist_sq(p1, p2):
    s = 0.0
    for j in range(len(p1)):
        d = float(p1[j]) - float(p2[j])
        s += d * d
    return s


class KMeans:
    """K-Means clustering algorithm."""
    def __init__(self, n_clusters=2, max_iter=100, tol=1e-4):
        self.n_clusters = int(n_clusters)
        self.max_iter = int(max_iter)
        self.tol = float(tol)
        self.cluster_centers_ = []
        self.labels_ = []
        self.inertia_ = 0.0

    def fit(self, X, y=None):
        n_samples = len(X)
        n_features = len(X[0])
        k = min(self.n_clusters, n_samples)

        # Deterministic centroid initialization (evenly spaced indices)
        step = max(1, n_samples // k)
        centers = [[float(v) for v in X[min(i * step, n_samples - 1)]] for i in range(k)]

        labels = [0] * n_samples
        for _ in range(self.max_iter):
            # Assignment step
            new_labels = []
            for row in X:
                best_k = 0
                min_d = _euclidean_dist_sq(row, centers[0])
                for c_idx in range(1, k):
                    d = _euclidean_dist_sq(row, centers[c_idx])
                    if d < min_d:
                        min_d = d
                        best_k = c_idx
                new_labels.append(best_k)

            # Update step
            new_centers = [[0.0] * n_features for _ in range(k)]
            counts = [0] * k
            for i in range(n_samples):
                c_idx = new_labels[i]
                counts[c_idx] += 1
                for j in range(n_features):
                    new_centers[c_idx][j] += float(X[i][j])

            max_shift = 0.0
            for c_idx in range(k):
                if counts[c_idx] > 0:
                    for j in range(n_features):
                        new_centers[c_idx][j] /= float(counts[c_idx])
                        shift = abs(new_centers[c_idx][j] - centers[c_idx][j])
                        if shift > max_shift:
                            max_shift = shift
                else:
                    new_centers[c_idx] = list(centers[c_idx])

            centers = new_centers
            labels = new_labels
            if max_shift < self.tol:
                break

        # Compute inertia
        inertia = 0.0
        for i in range(n_samples):
            inertia += _euclidean_dist_sq(X[i], centers[labels[i]])

        self.cluster_centers_ = centers
        self.labels_ = labels
        self.inertia_ = inertia
        return self

    def predict(self, X):
        preds = []
        for row in X:
            best_k = 0
            min_d = _euclidean_dist_sq(row, self.cluster_centers_[0])
            for c_idx in range(1, len(self.cluster_centers_)):
                d = _euclidean_dist_sq(row, self.cluster_centers_[c_idx])
                if d < min_d:
                    min_d = d
                    best_k = c_idx
            preds.append(best_k)
        return preds

    def fit_predict(self, X, y=None):
        return self.fit(X, y).labels_


class DBSCAN:
    """Density-Based Spatial Clustering of Applications with Noise (DBSCAN)."""
    def __init__(self, eps=0.5, min_samples=5):
        self.eps = float(eps)
        self.min_samples = int(min_samples)
        self.labels_ = []
        self.core_sample_indices_ = []

    def fit(self, X, y=None):
        n = len(X)
        eps_sq = self.eps * self.eps
        labels = [-1] * n
        core_samples = []
        visited = [False] * n

        # Precompute neighbors
        neighbors = []
        for i in range(n):
            nbrs_i = []
            for j in range(n):
                if _euclidean_dist_sq(X[i], X[j]) <= eps_sq:
                    nbrs_i.append(j)
            neighbors.append(nbrs_i)
            if len(nbrs_i) >= self.min_samples:
                core_samples.append(i)

        cluster_id = 0
        for i in range(n):
            if visited[i]:
                continue
            visited[i] = True

            nbrs = list(neighbors[i])
            if len(nbrs) < self.min_samples:
                labels[i] = -1  # Noise
            else:
                labels[i] = cluster_id
                k = 0
                while k < len(nbrs):
                    pt = nbrs[k]
                    k += 1
                    if not visited[pt]:
                        visited[pt] = True
                        pt_nbrs = neighbors[pt]
                        if len(pt_nbrs) >= self.min_samples:
                            for p_sub in pt_nbrs:
                                if p_sub not in nbrs:
                                    nbrs.append(p_sub)
                    if labels[pt] == -1:
                        labels[pt] = cluster_id
                cluster_id += 1

        self.labels_ = labels
        self.core_sample_indices_ = core_samples
        return self

    def fit_predict(self, X, y=None):
        return self.fit(X, y).labels_
