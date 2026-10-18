# sklearn/neighbors.py - Nearest Neighbors classification and regression

def _dist_sq(p1, p2):
    s = 0.0
    for j in range(len(p1)):
        d = float(p1[j]) - float(p2[j])
        s += d * d
    return s


class KNeighborsClassifier:
    """Classifier implementing the k-nearest neighbors vote."""
    def __init__(self, n_neighbors=3):
        self.n_neighbors = int(n_neighbors)
        self.X_train_ = []
        self.y_train_ = []
        self.classes_ = []

    def fit(self, X, y):
        self.X_train_ = [[float(v) for v in row] for row in X]
        self.y_train_ = [int(v) for v in y]
        unique = []
        for lab in self.y_train_:
            if lab not in unique:
                unique.append(lab)
        unique.sort()
        self.classes_ = unique
        return self

    def predict(self, X):
        preds = []
        k = min(self.n_neighbors, len(self.X_train_))
        for row in X:
            dists = []
            for i in range(len(self.X_train_)):
                dists.append((_dist_sq(row, self.X_train_[i]), self.y_train_[i]))
            dists.sort(key=lambda t: t[0])
            top_k = dists[:k]
            votes = {}
            for _, lab in top_k:
                votes[lab] = votes.get(lab, 0) + 1
            best_lab = max(votes.keys(), key=lambda l: votes[l])
            preds.append(best_lab)
        return preds

    def score(self, X, y):
        preds = self.predict(X)
        correct = sum(1 for i in range(len(y)) if preds[i] == int(y[i]))
        return float(correct) / float(len(y))


class KNeighborsRegressor:
    """Regression based on k-nearest neighbors."""
    def __init__(self, n_neighbors=3):
        self.n_neighbors = int(n_neighbors)
        self.X_train_ = []
        self.y_train_ = []

    def fit(self, X, y):
        self.X_train_ = [[float(v) for v in row] for row in X]
        self.y_train_ = [float(v) for v in y]
        return self

    def predict(self, X):
        preds = []
        k = min(self.n_neighbors, len(self.X_train_))
        for row in X:
            dists = []
            for i in range(len(self.X_train_)):
                dists.append((_dist_sq(row, self.X_train_[i]), self.y_train_[i]))
            dists.sort(key=lambda t: t[0])
            top_k = dists[:k]
            avg = sum(val for _, val in top_k) / float(k)
            preds.append(avg)
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
