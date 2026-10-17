from more_itertools import chunked, windowed, flatten, unique_everseen, take, first

def test_take_and_first():
    assert take(3, [10, 20, 30, 40, 50]) == [10, 20, 30]
    assert first([99, 100]) == 99
    assert first([], default=42) == 42

def test_windowed_and_unique():
    assert windowed([1, 2, 3, 4], 2) == [(1, 2), (2, 3), (3, 4)]
    assert unique_everseen([1, 2, 1, 3, 2, 4]) == [1, 2, 3, 4]
    assert flatten([[1, 2], [3, 4]]) == [1, 2, 3, 4]
