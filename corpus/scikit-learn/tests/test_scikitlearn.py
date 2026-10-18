# tests/test_scikitlearn.py - Test suite for scikitlearn package

from preprocessing import StandardScaler, MinMaxScaler, Binarizer, LabelEncoder
from linear_model import LinearRegression, Ridge, LogisticRegression
from cluster import KMeans
from neighbors import KNeighborsClassifier
from tree import DecisionTreeClassifier
from naive_bayes import GaussianNB
from decomposition import PCA
from metrics import accuracy_score, r2_score, mean_squared_error, confusion_matrix
from pipeline import Pipeline
from model_selection import train_test_split

def test_standard_scaler():
    X = [[1.0, 2.0], [3.0, 4.0], [5.0, 6.0]]
    scaler = StandardScaler()
    scaler.fit(X)
    assert abs(scaler.mean_[0] - 3.0) < 1e-5
    assert abs(scaler.mean_[1] - 4.0) < 1e-5
    Xt = scaler.transform(X)
    assert abs(Xt[1][0] - 0.0) < 1e-5
    inv = scaler.inverse_transform(Xt)
    assert abs(inv[0][0] - 1.0) < 1e-5

def test_minmax_scaler():
    X = [[0.0, 10.0], [5.0, 20.0], [10.0, 30.0]]
    scaler = MinMaxScaler()
    Xt = scaler.fit_transform(X)
    assert abs(Xt[0][0] - 0.0) < 1e-5
    assert abs(Xt[2][0] - 1.0) < 1e-5
    assert abs(Xt[1][1] - 0.5) < 1e-5

def test_linear_regression():
    # y = 2*x1 + 3*x2 + 5
    X = [[1.0, 2.0], [2.0, 1.0], [3.0, 4.0], [4.0, 3.0], [5.0, 5.0]]
    y = [2*r[0] + 3*r[1] + 5 for r in X]
    reg = LinearRegression()
    reg.fit(X, y)
    assert abs(reg.intercept_ - 5.0) < 1e-3
    assert abs(reg.coef_[0] - 2.0) < 1e-3
    assert abs(reg.coef_[1] - 3.0) < 1e-3
    r2 = reg.score(X, y)
    assert abs(r2 - 1.0) < 1e-4

def test_ridge_regression():
    X = [[1.0, 2.0], [2.0, 1.0], [3.0, 4.0], [4.0, 3.0]]
    y = [10.0, 12.0, 18.0, 20.0]
    ridge = Ridge(alpha=0.5)
    ridge.fit(X, y)
    preds = ridge.predict(X)
    assert len(preds) == 4
    assert r2_score(y, preds) > 0.90

def test_logistic_regression():
    # Linear separable classification
    X = [[1.0, 1.0], [1.5, 1.2], [5.0, 5.0], [5.5, 6.0]]
    y = [0, 0, 1, 1]
    clf = LogisticRegression(max_iter=300)
    clf.fit(X, y)
    preds = clf.predict(X)
    assert preds[0] == 0
    assert preds[1] == 0
    assert preds[2] == 1
    assert preds[3] == 1
    assert clf.score(X, y) == 1.0

def test_kmeans_clustering():
    X = [[1.0, 1.0], [1.2, 1.1], [10.0, 10.0], [10.2, 9.8]]
    km = KMeans(n_clusters=2)
    labels = km.fit_predict(X)
    assert labels[0] == labels[1]
    assert labels[2] == labels[3]
    assert labels[0] != labels[2]

def test_knn_classifier():
    X = [[1.0, 1.0], [1.5, 1.2], [8.0, 8.0], [8.5, 8.2]]
    y = [0, 0, 1, 1]
    knn = KNeighborsClassifier(n_neighbors=1)
    knn.fit(X, y)
    assert knn.predict([[1.2, 1.1]])[0] == 0
    assert knn.predict([[8.2, 8.1]])[0] == 1

def test_decision_tree():
    X = [[2.0, 3.0], [1.0, 1.0], [8.0, 7.0], [9.0, 8.0]]
    y = [0, 0, 1, 1]
    dt = DecisionTreeClassifier(max_depth=3)
    dt.fit(X, y)
    preds = dt.predict(X)
    assert list(preds) == list(y)

def test_gaussian_nb():
    X = [[1.0, 2.0], [1.2, 1.8], [6.0, 7.0], [6.2, 6.8]]
    y = [0, 0, 1, 1]
    gnb = GaussianNB()
    gnb.fit(X, y)
    preds = gnb.predict(X)
    assert list(preds) == list(y)

def test_pca():
    X = [[1.0, 2.0, 3.0], [2.0, 4.0, 6.0], [3.0, 6.0, 9.0], [4.0, 8.0, 12.0]]
    pca = PCA(n_components=1)
    Xt = pca.fit_transform(X)
    assert len(Xt) == 4
    assert len(Xt[0]) == 1
    assert pca.explained_variance_ratio_[0] > 0.95

def test_pipeline():
    X = [[10.0, 20.0], [20.0, 10.0], [80.0, 90.0], [90.0, 80.0]]
    y = [0, 0, 1, 1]
    pipe = Pipeline([
        ('scaler', StandardScaler()),
        ('clf', LogisticRegression(max_iter=300))
    ])
    pipe.fit(X, y)
    preds = pipe.predict(X)
    assert list(preds) == list(y)
