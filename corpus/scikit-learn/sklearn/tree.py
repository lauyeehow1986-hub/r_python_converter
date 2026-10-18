# sklearn/tree.py - Decision Tree classification and regression

class _TreeNode:
    def __init__(self, feature=None, threshold=None, left=None, right=None, value=None):
        self.feature = feature
        self.threshold = threshold
        self.left = left
        self.right = right
        self.value = value


def _gini(y):
    n = len(y)
    if n == 0:
        return 0.0
    counts = {}
    for lab in y:
        counts[lab] = counts.get(lab, 0) + 1
    impurity = 1.0
    for c in counts.values():
        p = c / float(n)
        impurity -= p * p
    return impurity


class DecisionTreeClassifier:
    """A decision tree classifier."""
    def __init__(self, max_depth=5, min_samples_split=2):
        self.max_depth = int(max_depth)
        self.min_samples_split = int(min_samples_split)
        self.root_ = None

    def _majority_vote(self, y):
        counts = {}
        for lab in y:
            counts[lab] = counts.get(lab, 0) + 1
        return max(counts.keys(), key=lambda l: counts[l])

    def _build_tree(self, X, y, depth=0):
        n = len(y)
        if n == 0:
            return None
        # Base cases
        majority = self._majority_vote(y)
        if depth >= self.max_depth or n < self.min_samples_split or _gini(y) == 0.0:
            return _TreeNode(value=majority)

        p = len(X[0])
        best_gain = 0.0
        best_feat = None
        best_thresh = None

        current_gini = _gini(y)

        for feat in range(p):
            vals = sorted(list(set(row[feat] for row in X)))
            for k in range(len(vals) - 1):
                thresh = (vals[k] + vals[k + 1]) / 2.0
                left_y = [y[i] for i in range(n) if X[i][feat] <= thresh]
                right_y = [y[i] for i in range(n) if X[i][feat] > thresh]

                if len(left_y) == 0 or len(right_y) == 0:
                    continue

                gain = current_gini - (len(left_y)/float(n) * _gini(left_y) + len(right_y)/float(n) * _gini(right_y))
                if gain > best_gain:
                    best_gain = gain
                    best_feat = feat
                    best_thresh = thresh

        if best_gain < 1e-7:
            return _TreeNode(value=majority)

        left_idx = [i for i in range(n) if X[i][best_feat] <= best_thresh]
        right_idx = [i for i in range(n) if X[i][best_feat] > best_thresh]

        left_child = self._build_tree([X[i] for i in left_idx], [y[i] for i in left_idx], depth + 1)
        right_child = self._build_tree([X[i] for i in right_idx], [y[i] for i in right_idx], depth + 1)

        return _TreeNode(feature=best_feat, threshold=best_thresh, left=left_child, right=right_child, value=majority)

    def fit(self, X, y):
        X_float = [[float(v) for v in row] for row in X]
        y_int = [int(v) for v in y]
        self.root_ = self._build_tree(X_float, y_int, depth=0)
        return self

    def _predict_row(self, row, node):
        if node.left is None and node.right is None:
            return node.value
        if node.feature is not None and row[node.feature] <= node.threshold:
            return self._predict_row(row, node.left) if node.left else node.value
        else:
            return self._predict_row(row, node.right) if node.right else node.value

    def predict(self, X):
        return [self._predict_row(row, self.root_) for row in X]

    def score(self, X, y):
        preds = self.predict(X)
        correct = sum(1 for i in range(len(y)) if preds[i] == int(y[i]))
        return float(correct) / float(len(y))


class DecisionTreeRegressor:
    """A decision tree regressor."""
    def __init__(self, max_depth=5, min_samples_split=2):
        self.max_depth = int(max_depth)
        self.min_samples_split = int(min_samples_split)
        self.root_ = None

    def _mean_val(self, y):
        return sum(y) / float(len(y)) if len(y) > 0 else 0.0

    def _mse(self, y):
        n = len(y)
        if n <= 1:
            return 0.0
        m = self._mean_val(y)
        return sum((yi - m) ** 2 for yi in y) / float(n)

    def _build_tree(self, X, y, depth=0):
        n = len(y)
        if n == 0:
            return None
        mean_y = self._mean_val(y)
        if depth >= self.max_depth or n < self.min_samples_split:
            return _TreeNode(value=mean_y)

        p = len(X[0])
        best_gain = 0.0
        best_feat = None
        best_thresh = None

        current_mse = self._mse(y)

        for feat in range(p):
            vals = sorted(list(set(row[feat] for row in X)))
            for k in range(len(vals) - 1):
                thresh = (vals[k] + vals[k + 1]) / 2.0
                left_y = [y[i] for i in range(n) if X[i][feat] <= thresh]
                right_y = [y[i] for i in range(n) if X[i][feat] > thresh]

                if len(left_y) == 0 or len(right_y) == 0:
                    continue

                gain = current_mse - (len(left_y)/float(n) * self._mse(left_y) + len(right_y)/float(n) * self._mse(right_y))
                if gain > best_gain:
                    best_gain = gain
                    best_feat = feat
                    best_thresh = thresh

        if best_gain < 1e-7:
            return _TreeNode(value=mean_y)

        left_idx = [i for i in range(n) if X[i][best_feat] <= best_thresh]
        right_idx = [i for i in range(n) if X[i][best_feat] > best_thresh]

        left_child = self._build_tree([X[i] for i in left_idx], [y[i] for i in left_idx], depth + 1)
        right_child = self._build_tree([X[i] for i in right_idx], [y[i] for i in right_idx], depth + 1)

        return _TreeNode(feature=best_feat, threshold=best_thresh, left=left_child, right=right_child, value=mean_y)

    def fit(self, X, y):
        X_float = [[float(v) for v in row] for row in X]
        y_float = [float(v) for v in y]
        self.root_ = self._build_tree(X_float, y_float, depth=0)
        return self

    def _predict_row(self, row, node):
        if node.left is None and node.right is None:
            return node.value
        if node.feature is not None and row[node.feature] <= node.threshold:
            return self._predict_row(row, node.left) if node.left else node.value
        else:
            return self._predict_row(row, node.right) if node.right else node.value

    def predict(self, X):
        return [self._predict_row(row, self.root_) for row in X]

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
