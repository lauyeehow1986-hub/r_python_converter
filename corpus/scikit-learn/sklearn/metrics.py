# sklearn/metrics.py - Evaluation metrics for classification and regression

def accuracy_score(y_true, y_pred):
    """Accuracy classification score."""
    n = len(y_true)
    if n == 0:
        return 0.0
    correct = sum(1 for i in range(n) if y_true[i] == y_pred[i])
    return float(correct) / float(n)


def confusion_matrix(y_true, y_pred):
    """Compute confusion matrix to evaluate the accuracy of a classification."""
    labels = sorted(list(set(list(y_true) + list(y_pred))))
    label_to_idx = {l: i for i, l in enumerate(labels)}
    k = len(labels)
    cm = [[0] * k for _ in range(k)]
    for i in range(len(y_true)):
        ti = label_to_idx[y_true[i]]
        pi = label_to_idx[y_pred[i]]
        cm[ti][pi] += 1
    return cm


def precision_score(y_true, y_pred, pos_label=1):
    """Compute the precision: tp / (tp + fp)."""
    tp = sum(1 for i in range(len(y_true)) if y_true[i] == pos_label and y_pred[i] == pos_label)
    fp = sum(1 for i in range(len(y_true)) if y_true[i] != pos_label and y_pred[i] == pos_label)
    denom = tp + fp
    return float(tp) / float(denom) if denom > 0 else 0.0


def recall_score(y_true, y_pred, pos_label=1):
    """Compute the recall: tp / (tp + fn)."""
    tp = sum(1 for i in range(len(y_true)) if y_true[i] == pos_label and y_pred[i] == pos_label)
    fn = sum(1 for i in range(len(y_true)) if y_true[i] == pos_label and y_pred[i] != pos_label)
    denom = tp + fn
    return float(tp) / float(denom) if denom > 0 else 0.0


def f1_score(y_true, y_pred, pos_label=1):
    """Compute the F1 score: 2 * (precision * recall) / (precision + recall)."""
    p = precision_score(y_true, y_pred, pos_label=pos_label)
    r = recall_score(y_true, y_pred, pos_label=pos_label)
    denom = p + r
    return 2.0 * (p * r) / denom if denom > 0 else 0.0


def roc_auc_score(y_true, y_score):
    """Compute Area Under the Receiver Operating Characteristic Curve (ROC AUC)."""
    # Wilcoxon-Mann-Whitney trapezoidal calculation
    pairs = sorted(zip(y_score, y_true), key=lambda x: x[0])
    n = len(pairs)
    pos_count = sum(1 for _, y in pairs if y == 1)
    neg_count = n - pos_count
    if pos_count == 0 or neg_count == 0:
        return 0.5

    rank_sum = 0
    for rank, (_, y) in enumerate(pairs, start=1):
        if y == 1:
            rank_sum += rank

    u = rank_sum - (pos_count * (pos_count + 1)) / 2.0
    return u / float(pos_count * neg_count)


def mean_squared_error(y_true, y_pred):
    """Mean squared error regression loss."""
    n = len(y_true)
    if n == 0:
        return 0.0
    s = sum((float(y_true[i]) - float(y_pred[i])) ** 2 for i in range(n))
    return s / float(n)


def mean_absolute_error(y_true, y_pred):
    """Mean absolute error regression loss."""
    n = len(y_true)
    if n == 0:
        return 0.0
    s = sum(abs(float(y_true[i]) - float(y_pred[i])) for i in range(n))
    return s / float(n)


def r2_score(y_true, y_pred):
    """R^2 (coefficient of determination) regression score function."""
    n = len(y_true)
    if n <= 1:
        return 1.0
    y_mean = sum(float(yi) for yi in y_true) / float(n)
    ss_tot = sum((float(y_true[i]) - y_mean) ** 2 for i in range(n))
    ss_res = sum((float(y_true[i]) - float(y_pred[i])) ** 2 for i in range(n))
    if ss_tot == 0.0:
        return 1.0 if ss_res == 0.0 else 0.0
    return 1.0 - (ss_res / ss_tot)
