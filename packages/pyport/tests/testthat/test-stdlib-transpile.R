test_that("transpiled Python code can import and use stdlib modules seamlessly", {
  mod <- eval_python("
import math
import itertools
from collections import deque, Counter, defaultdict, namedtuple
from functools import reduce, lru_cache
import operator
import re
import json
from datetime import date, timedelta

def compute_stats(nums):
    g = reduce(math.gcd, nums)
    pairs = list(itertools.pairwise(nums))
    return g, len(pairs)

def sliding_window(iterable, n):
    it = iter(iterable)
    window = deque(itertools.islice(it, n - 1), maxlen=n)
    for x in it:
        window.append(x)
        yield tuple(window)

def count_words(text):
    words = re.findall(r'[a-z]+', text.lower())
    c = Counter(words)
    return c.most_common(2)

def next_leap_check(y, m, d, step_days):
    start = date(y, m, d)
    end = start + timedelta(days=step_days)
    return end.isoformat()
")

  res <- mod$compute_stats(pyrt::py_list(list(24L, 36L, 60L)))
  expect_Equal <- expect_equal
  expect_equal(res$as_r_list(), list(12L, 2L))

  win <- pyrt::py_collect(mod$sliding_window(pyrt::py_list(list(1L, 2L, 3L, 4L, 5L)), 3L))
  expect_length(win, 3L)
  expect_equal(win[[1L]]$as_r_list(), list(1L, 2L, 3L))
  expect_equal(win[[3L]]$as_r_list(), list(3L, 4L, 5L))

  top2 <- mod$count_words("the cat and the hat and the mat")$as_r_list()
  expect_equal(top2[[1L]]$as_r_list(), list("the", 3L))
  expect_equal(top2[[2L]]$as_r_list(), list("and", 2L))

  expect_identical(mod$next_leap_check(2024L, 2L, 28L, 1L), "2024-02-29")
})

test_that("pure-Python corpus patterns (more-itertools, attrs/dataclasses, dateutil) transpile and pass", {
  # 1. more-itertools style combinators & chunking generators
  more_it <- eval_python("
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
    res = []
    items = list(seq)
    for i in range(0, len(items) - n + 1, step):
        res.append(tuple(items[i:i+n]))
    return res

def flatten(list_of_lists):
    return list(itertools.chain.from_iterable(list_of_lists))

def unique_everseen(iterable):
    seen = set()
    for element in iterable:
        if element not in seen:
            seen.add(element)
            yield element
")

  chunks <- pyrt::py_collect(more_it$chunked(pyrt::py_list(as.list(1:7)), 3L))
  expect_length(chunks, 3L)
  expect_equal(chunks[[1L]]$as_r_list(), list(1L, 2L, 3L))
  expect_equal(chunks[[3L]]$as_r_list(), list(7L))

  wins <- more_it$windowed(pyrt::py_list(as.list(1:5)), 3L, 2L)$as_r_list()
  expect_length(wins, 2L)
  expect_equal(wins[[1L]]$as_r_list(), list(1L, 2L, 3L))
  expect_equal(wins[[2L]]$as_r_list(), list(3L, 4L, 5L))

  uniq <- pyrt::py_collect(more_it$unique_everseen(pyrt::py_list(list(1L, 2L, 1L, 3L, 2L, 4L))))
  expect_equal(uniq, list(1L, 2L, 3L, 4L))

  # 2. dataclasses / attrs style class decorators with defaults
  dc_mod <- eval_python("
from dataclasses import dataclass

@dataclass
class InventoryItem:
    name: str = 'widget'
    unit_price: float = 10.0
    quantity_on_hand: int = 0

    def total_cost(self) -> float:
        return self.unit_price * self.quantity_on_hand
")
  item <- pyrt::py_call(dc_mod$InventoryItem, list("gear", 2.5, 4L))
  expect_identical(pyrt::py_getattr(item, "name"), "gear")
  expect_equal(pyrt::py_getattr(item, "total_cost")(), 10.0)
})
