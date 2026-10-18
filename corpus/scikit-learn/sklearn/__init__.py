# sklearn/__init__.py - Top-level scikit-learn package metadata and exports

__version__ = "1.5.0"
__author__ = "scikit-learn developers / ported by Yee How Lau"

__all__ = [
    # Preprocessing
    "StandardScaler",
    "MinMaxScaler",
    "RobustScaler",
    "Binarizer",
    "LabelEncoder",
    # Linear Models
    "LinearRegression",
    "Ridge",
    "Lasso",
    "LogisticRegression",
    # Clustering
    "KMeans",
    "DBSCAN",
    # Neighbors
    "KNeighborsClassifier",
    "KNeighborsRegressor",
    # Decision Trees
    "DecisionTreeClassifier",
    "DecisionTreeRegressor",
    # Naive Bayes
    "GaussianNB",
    # Decomposition
    "PCA",
    # Metrics
    "accuracy_score",
    "precision_score",
    "recall_score",
    "f1_score",
    "confusion_matrix",
    "roc_auc_score",
    "mean_squared_error",
    "mean_absolute_error",
    "r2_score",
    # Model Selection
    "train_test_split",
    "KFold",
    # Pipeline
    "Pipeline"
]
