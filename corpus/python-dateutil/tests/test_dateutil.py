from dateutil_ext import add_months, parse_isodate, days_between

def test_dateutil_operations():
    d = parse_isodate("2024-01-31")
    d_next = add_months(d, 1)
    assert d_next.isoformat() == "2024-02-29"
    assert days_between("2024-02-28", "2024-03-01") == 2
