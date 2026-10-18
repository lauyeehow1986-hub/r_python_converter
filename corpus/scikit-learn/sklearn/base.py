# sklearn/base.py - Base classes for scikit-learn estimators and transformers

class BaseEstimator:
    """Base class for all estimators in scikit-learn."""
    def __init__(self):
        pass

    def get_params(self):
        return {}

    def set_params(self, **params):
        return self


class ClassifierMixin:
    """Mixin class for all classifiers in scikit-learn."""
    def score(self, X, y):
        preds = self.predict(X)
        correct = 0
        n = len(y)
        if n == 0:
            return 0.0
        for i in range(n):
            if preds[i] == y[i]:
                correct += 1
        return float(correct) / float(n)


class RegressorMixin:
    """Mixin class for all regression estimators in scikit-learn."""
    def score(self, X, y):
        preds = self.predict(X)
        n = len(y)
        if n <= 1:
            return 1.0
        y_mean = sum(y) / float(n)
        ss_tot = sum((y[i] - y_mean) ** 2 for i in range(n))
        ss_res = sum((y[i] - preds[i]) ** 2 for i in range(n))
        if ss_tot == 0.0:
            return 1.0 if ss_res == 0.0 else 0.0
        return 1.0 - (ss_res / ss_tot)


class TransformerMixin:
    """Mixin class for all transformers in scikit-learn."""
    def fit_transform(self, X, y=None):
        return self.fit(X, y).transform(X)
