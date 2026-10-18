# sklearn/model_selection.py - Model selection and evaluation tools

def train_test_split(X, y, test_size=0.25, shuffle=True, random_state=None):
    """Split arrays or matrices into random train and test subsets."""
    n = len(X)
    n_test = max(1, int(n * float(test_size)))
    indices = list(range(n))

    if shuffle:
        # Deterministic pseudo-random shuffle (LCG)
        seed = 42 if random_state is None else int(random_state)
        for i in range(n - 1, 0, -1):
            seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF
            j = seed % (i + 1)
            indices[i], indices[j] = indices[j], indices[i]

    test_idx = indices[:n_test]
    train_idx = indices[n_test:]

    X_train = [X[i] for i in train_idx]
    X_test = [X[i] for i in test_idx]
    y_train = [y[i] for i in train_idx]
    y_test = [y[i] for i in test_idx]

    return X_train, X_test, y_train, y_test


class KFold:
    """K-Folds cross-validator."""
    def __init__(self, n_splits=5, shuffle=False, random_state=None):
        self.n_splits = int(n_splits)
        self.shuffle = shuffle
        self.random_state = random_state

    def split(self, X):
        n = len(X)
        indices = list(range(n))
        if self.shuffle:
            seed = 42 if self.random_state is None else int(self.random_state)
            for i in range(n - 1, 0, -1):
                seed = (seed * 1103515245 + 12345) & 0x7FFFFFFF
                j = seed % (i + 1)
                indices[i], indices[j] = indices[j], indices[i]

        fold_sizes = [n // self.n_splits] * self.n_splits
        for i in range(n % self.n_splits):
            fold_sizes[i] += 1

        splits = []
        current = 0
        for fold_size in fold_sizes:
            test_indices = indices[current:current + fold_size]
            train_indices = indices[:current] + indices[current + fold_size:]
            splits.append((train_indices, test_indices))
            current += fold_size
        return splits
