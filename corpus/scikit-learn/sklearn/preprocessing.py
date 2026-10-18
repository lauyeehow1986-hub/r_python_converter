# sklearn/preprocessing.py - Data preprocessing and feature scaling

import math

class StandardScaler:
    """Standardize features by removing the mean and scaling to unit variance."""
    def __init__(self, with_mean=True, with_std=True):
        self.with_mean = with_mean
        self.with_std = with_std
        self.mean_ = []
        self.var_ = []
        self.scale_ = []
        self.n_samples_seen_ = 0

    def fit(self, X, y=None):
        n_samples = len(X)
        if n_samples == 0:
            return self
        n_features = len(X[0])
        self.n_samples_seen_ = n_samples

        means = [0.0] * n_features
        for row in X:
            for j in range(n_features):
                means[j] += float(row[j])
        for j in range(n_features):
            means[j] /= float(n_samples)

        vars_ = [0.0] * n_features
        for row in X:
            for j in range(n_features):
                diff = float(row[j]) - means[j]
                vars_[j] += diff * diff
        for j in range(n_features):
            vars_[j] /= float(n_samples)

        scales = []
        for j in range(n_features):
            s = math.sqrt(vars_[j])
            scales.append(s if s > 1e-12 else 1.0)

        self.mean_ = means
        self.var_ = vars_
        self.scale_ = scales
        return self

    def transform(self, X):
        out = []
        n_features = len(self.mean_)
        for row in X:
            scaled_row = []
            for j in range(n_features):
                val = float(row[j])
                if self.with_mean:
                    val -= self.mean_[j]
                if self.with_std:
                    val /= self.scale_[j]
                scaled_row.append(val)
            out.append(scaled_row)
        return out

    def fit_transform(self, X, y=None):
        return self.fit(X, y).transform(X)

    def inverse_transform(self, X):
        out = []
        n_features = len(self.mean_)
        for row in X:
            orig_row = []
            for j in range(n_features):
                val = float(row[j])
                if self.with_std:
                    val *= self.scale_[j]
                if self.with_mean:
                    val += self.mean_[j]
                orig_row.append(val)
            out.append(orig_row)
        return out


class MinMaxScaler:
    """Transform features by scaling each feature to a given range [min, max]."""
    def __init__(self, feature_range=(0.0, 1.0)):
        self.feature_range = feature_range
        self.data_min_ = []
        self.data_max_ = []
        self.data_range_ = []
        self.scale_ = []

    def fit(self, X, y=None):
        n_samples = len(X)
        if n_samples == 0:
            return self
        n_features = len(X[0])

        mins = [float(X[0][j]) for j in range(n_features)]
        maxs = [float(X[0][j]) for j in range(n_features)]

        for row in X:
            for j in range(n_features):
                val = float(row[j])
                if val < mins[j]:
                    mins[j] = val
                if val > maxs[j]:
                    maxs[j] = val

        ranges = []
        scales = []
        target_range = float(self.feature_range[1] - self.feature_range[0])
        for j in range(n_features):
            rng = maxs[j] - mins[j]
            ranges.append(rng)
            scales.append(target_range / rng if rng > 1e-12 else 1.0)

        self.data_min_ = mins
        self.data_max_ = maxs
        self.data_range_ = ranges
        self.scale_ = scales
        return self

    def transform(self, X):
        out = []
        n_features = len(self.data_min_)
        t_min = float(self.feature_range[0])
        for row in X:
            scaled_row = []
            for j in range(n_features):
                val = float(row[j])
                scaled = (val - self.data_min_[j]) * self.scale_[j] + t_min
                scaled_row.append(scaled)
            out.append(scaled_row)
        return out

    def fit_transform(self, X, y=None):
        return self.fit(X, y).transform(X)

    def inverse_transform(self, X):
        out = []
        n_features = len(self.data_min_)
        t_min = float(self.feature_range[0])
        for row in X:
            orig_row = []
            for j in range(n_features):
                val = float(row[j])
                orig = (val - t_min) / self.scale_[j] + self.data_min_[j]
                orig_row.append(orig)
            out.append(orig_row)
        return out


class RobustScaler:
    """Scale features using statistics that are robust to outliers (median and IQR)."""
    def __init__(self):
        self.center_ = []
        self.scale_ = []

    def fit(self, X, y=None):
        n_samples = len(X)
        if n_samples == 0:
            return self
        n_features = len(X[0])

        centers = []
        scales = []
        for j in range(n_features):
            col = sorted([float(row[j]) for row in X])
            med = col[n_samples // 2]
            q25 = col[int(n_samples * 0.25)]
            q75 = col[int(n_samples * 0.75)]
            iqr = q75 - q25
            centers.append(med)
            scales.append(iqr if iqr > 1e-12 else 1.0)

        self.center_ = centers
        self.scale_ = scales
        return self

    def transform(self, X):
        out = []
        n_features = len(self.center_)
        for row in X:
            scaled_row = []
            for j in range(n_features):
                val = (float(row[j]) - self.center_[j]) / self.scale_[j]
                scaled_row.append(val)
            out.append(scaled_row)
        return out

    def fit_transform(self, X, y=None):
        return self.fit(X, y).transform(X)


class Binarizer:
    """Binarize data (set feature values to 0 or 1) according to a threshold."""
    def __init__(self, threshold=0.0):
        self.threshold = float(threshold)

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        out = []
        for row in X:
            out.append([1.0 if float(x) > self.threshold else 0.0 for x in row])
        return out

    def fit_transform(self, X, y=None):
        return self.transform(X)


class LabelEncoder:
    """Encode target labels with value between 0 and n_classes-1."""
    def __init__(self):
        self.classes_ = []
        self._mapping = {}

    def fit(self, y):
        unique = []
        for label in y:
            if label not in unique:
                unique.append(label)
        unique.sort()
        self.classes_ = unique
        self._mapping = {label: i for i, label in enumerate(unique)}
        return self

    def transform(self, y):
        return [self._mapping[label] for label in y]

    def fit_transform(self, y):
        return self.fit(y).transform(y)

    def inverse_transform(self, y):
        return [self.classes_[int(idx)] for idx in y]
