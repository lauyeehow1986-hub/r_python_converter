# sklearn/naive_bayes.py - Gaussian Naive Bayes classification

import math

class GaussianNB:
    """Gaussian Naive Bayes (GaussianNB) classifier."""
    def __init__(self, var_smoothing=1e-9):
        self.var_smoothing = float(var_smoothing)
        self.classes_ = []
        self.class_priors_ = []
        self.theta_ = []  # means per class per feature
        self.var_ = []    # variances per class per feature

    def fit(self, X, y):
        n = len(X)
        p = len(X[0])
        y_int = [int(v) for v in y]

        classes = sorted(list(set(y_int)))
        self.classes_ = classes
        n_classes = len(classes)

        priors = []
        thetas = []
        vars_ = []

        for c in classes:
            c_indices = [i for i in range(n) if y_int[i] == c]
            n_c = len(c_indices)
            priors.append(float(n_c) / float(n))

            means = [0.0] * p
            for i in c_indices:
                for j in range(p):
                    means[j] += float(X[i][j])
            for j in range(p):
                means[j] /= float(n_c)

            variances = [0.0] * p
            for i in c_indices:
                for j in range(p):
                    diff = float(X[i][j]) - means[j]
                    variances[j] += diff * diff
            for j in range(p):
                variances[j] = (variances[j] / float(n_c)) + self.var_smoothing

            thetas.append(means)
            vars_.append(variances)

        self.class_priors_ = priors
        self.theta_ = thetas
        self.var_ = vars_
        return self

    def _log_gaussian_prob(self, x_val, mean, var):
        # -0.5 * log(2 * pi * var) - ((x - mean)^2) / (2 * var)
        log_norm = -0.5 * math.log(2.0 * math.pi * var)
        sq_diff = (x_val - mean) ** 2
        return log_norm - (sq_diff / (2.0 * var))

    def predict_log_proba(self, X):
        log_probs = []
        n_classes = len(self.classes_)
        p = len(self.theta_[0])

        for row in X:
            row_lps = []
            for c_idx in range(n_classes):
                lp = math.log(self.class_priors_[c_idx] + 1e-15)
                for j in range(p):
                    lp += self._log_gaussian_prob(float(row[j]), self.theta_[c_idx][j], self.var_[c_idx][j])
                row_lps.append(lp)
            log_probs.append(row_lps)
        return log_probs

    def predict(self, X):
        lps = self.predict_log_proba(X)
        preds = []
        for row in lps:
            best_c = 0
            best_val = row[0]
            for c_idx in range(1, len(row)):
                if row[c_idx] > best_val:
                    best_val = row[c_idx]
                    best_c = c_idx
            preds.append(self.classes_[best_c])
        return preds

    def score(self, X, y):
        preds = self.predict(X)
        correct = sum(1 for i in range(len(y)) if preds[i] == int(y[i]))
        return float(correct) / float(len(y))
