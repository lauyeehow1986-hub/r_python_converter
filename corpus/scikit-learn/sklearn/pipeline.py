# sklearn/pipeline.py - Pipeline of transforms with a final estimator

class Pipeline:
    """Pipeline of transforms with a final estimator."""
    def __init__(self, steps):
        self.steps = steps
        self.named_steps = {name: step for name, step in steps}

    def fit(self, X, y=None):
        Xt = X
        # Transform through all steps except the last
        for i in range(len(self.steps) - 1):
            name, transform = self.steps[i]
            Xt = transform.fit_transform(Xt, y)
        # Fit final estimator
        final_name, final_estimator = self.steps[-1]
        final_estimator.fit(Xt, y)
        return self

    def predict(self, X):
        Xt = X
        for i in range(len(self.steps) - 1):
            name, transform = self.steps[i]
            Xt = transform.transform(Xt)
        final_name, final_estimator = self.steps[-1]
        return final_estimator.predict(Xt)

    def fit_predict(self, X, y=None):
        self.fit(X, y)
        return self.predict(X)

    def score(self, X, y):
        Xt = X
        for i in range(len(self.steps) - 1):
            name, transform = self.steps[i]
            Xt = transform.transform(Xt)
        final_name, final_estimator = self.steps[-1]
        return final_estimator.score(Xt, y)
