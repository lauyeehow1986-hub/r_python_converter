import itertools
from collections import deque

def chunked(iterable, n):
    it = iter(iterable)
    while True:
        batch = list(itertools.islice(it, n))
        if not batch:
            break
        yield batch

def windowed(seq, n, step=1):
    items = list(seq)
    out = []
    for i in range(0, len(items) - n + 1, step):
        out.append(tuple(items[i:i + n]))
    return out

def flatten(list_of_lists):
    return list(itertools.chain.from_iterable(list_of_lists))

def unique_everseen(iterable):
    seen = set()
    out = []
    for element in iterable:
        if element not in seen:
            seen.add(element)
            out.append(element)
    return out

def take(n, iterable):
    return list(itertools.islice(iterable, n))

def first(iterable, default=None):
    for item in iterable:
        return item
    if default is not None:
        return default
    raise ValueError("first() was called on an empty iterable")
