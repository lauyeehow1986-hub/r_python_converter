def test_ols_and_stats():
    X = [[1.0, 1.0], [1.0, 2.0], [1.0, 3.0], [1.0, 4.0]]
    y = [3.0, 5.0, 7.0, 9.0]
    beta = ols_fit(X, y)
    assert round(beta[0], 6) == 1.0
    assert round(beta[1], 6) == 2.0
    z = standardize_1d([2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0])
    assert round(z[0], 4) == -1.5
    p = normal_two_sided_pvalue(0.0)
    assert round(p, 6) == 1.0
