from datetime import date, timedelta

def is_leap(year):
    return (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)

def month_days(year, month):
    if month == 2:
        return 29 if is_leap(year) else 28
    if month in (4, 6, 9, 11):
        return 30
    return 31

def add_months(dt, months):
    total_m = (dt.year * 12 + (dt.month - 1)) + months
    ny = total_m // 12
    nm = (total_m % 12) + 1
    nd = min(dt.day, month_days(ny, nm))
    return date(ny, nm, nd)

def parse_isodate(text):
    parts = text.strip().split("-")
    return date(int(parts[0]), int(parts[1]), int(parts[2]))

def days_between(iso_start, iso_end):
    d1 = parse_isodate(iso_start)
    d2 = parse_isodate(iso_end)
    diff = d2 - d1
    return diff.days
