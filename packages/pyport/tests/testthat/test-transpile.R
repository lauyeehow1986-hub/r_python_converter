test_that("analyze_python detects scopes, globals, nonlocals, generators, and diagnostics", {
  src <- "
x = 10

def outer(a, *args, k=1, **kwargs):
    global x
    y = a + 1
    def inner():
        nonlocal y
        y += 1
        return y
    return inner

def gen(n):
    for i in range(n):
        yield i

class MetaUser(Base, metaclass=MyMeta):
    pass

def dyn(code):
    return eval(code)
"
  tree <- parse_python(src)
  info <- analyze_python(tree)
  expect_s3_class(info, "pyport_analysis")

  outer_scope <- info$scope$children[[1L]]
  expect_identical(outer_scope$name, "outer")
  expect_identical(outer_scope$globals, "x")
  expect_false(outer_scope$is_generator)
  expect_true(all(c("a", "args", "k", "kwargs", "y", "inner") %in% outer_scope$locals))

  inner_scope <- outer_scope$children[[1L]]
  expect_identical(inner_scope$name, "inner")
  expect_identical(inner_scope$nonlocals, "y")

  gen_scope <- info$scope$children[[2L]]
  expect_identical(gen_scope$name, "gen")
  expect_true(gen_scope$is_generator)

  codes <- vapply(info$diagnostics, `[[`, character(1), "code")
  expect_true("PYPORT_METACLASS" %in% codes)
  expect_true("PYPORT_DYNAMIC_EXEC" %in% codes)
})

test_that("transpile_python and eval_python execute expressions, numerics, f-strings, and unpacking", {
  mod <- eval_python("
a = -7 // 2
b = -7 % 2
big = (10 ** 35) + 42
bits = (-5 & 3, -5 | 3, -5 ^ 3, ~0, -8 >> 2, 1 << 40)

# Short-circuiting `and` / `or` returning operand values
sc_or = 0 or '' or 'fallback' or 99
sc_and = 1 and 'ok' and [10, 20]

# Chained comparison with single evaluation of middle term
calls = 0
def bump(v):
    global calls
    calls += 1
    return v

chain_true = 1 < bump(5) < 10
chain_false = 10 < bump(5) < bump(20)

# Unpacking and f-strings
first, *middle, last = [1, 2, 3, 4, 5]
msg = f'first={first}, mid={len(middle)}, pi={3.14159:.2f}, r={\"hi\"!r}'
")

  expect_identical(mod$a, -4L)
  expect_identical(mod$b, 1L)
  expect_identical(as.character(mod$big), "100000000000000000000000000000000042")
  expect_identical(
    lapply(mod$bits$items, as.character),
    list("3", "-5", "-8", "-1", "-2", "1099511627776")
  )
  expect_identical(mod$sc_or, "fallback")
  expect_identical(mod$sc_and$as_r_list(), list(10L, 20L))
  expect_true(mod$chain_true)
  expect_false(mod$chain_false)
  expect_identical(mod$calls, 2L)
  expect_identical(mod$first, 1L)
  expect_identical(mod$middle$as_r_list(), list(2L, 3L, 4L))
  expect_identical(mod$last, 5L)
  expect_identical(mod$msg, "first=1, mid=3, pi=3.14, r='hi'")
})

test_that("functions, *args, **kwargs, closures, nonlocal, global, and decorators work", {
  mod <- eval_python("
total = 100

def add_to_total(delta):
    global total
    total += delta
    return total

def make_counter(start=0):
    count = start
    def step(by=1):
        nonlocal count
        count += by
        return count
    return step

def flex(a, b=2, *args, scale=10, **kwargs):
    extra_pos = sum(args)
    extra_kw = kwargs['x'] if 'x' in kwargs else 0
    return (a + b + extra_pos + extra_kw) * scale

def double_result(fn):
    def wrapper(n):
        return fn(n) * 2
    return wrapper

@double_result
def square(n):
    '''Docstring that must not become return value.'''
    return n * n
")

  expect_identical(mod$add_to_total(25L), 125L)
  expect_identical(mod$total, 125L)

  ctr <- mod$make_counter(10L)
  expect_identical(ctr(), 11L)
  expect_identical(ctr(4L), 15L)

  expect_identical(mod$flex(1L, 3L, 4L, 5L, scale = 2L, x = 7L), 40L)
  expect_identical(mod$square(6L), 72L)
})

test_that("control flow, for/else, while/else, try/except/else/finally, with, and match/case work", {
  mod <- eval_python("
def find_divisor(n, candidates):
    for c in candidates:
        if c == 0:
            continue
        if n % c == 0:
            found = c
            break
    else:
        found = -1
    return found

def try_return_with_finally(log_list, x):
    try:
        if x < 0:
            raise ValueError('negative')
        return x * 10
    except ValueError as e:
        log_list.append('caught:' + str(e))
        return -1
    finally:
        log_list.append('finally')

def classify(item):
    match item:
        case 0:
            return 'zero'
        case [a, b] if a < b:
            return f'asc:{a},{b}'
        case {'kind': 'pt', 'x': x}:
            return f'pt:{x}'
        case _:
            return 'other'
")

  expect_identical(mod$find_divisor(21L, pyrt::py_list(list(0L, 2L, 3L, 7L))), 3L)
  expect_identical(mod$find_divisor(21L, pyrt::py_list(list(0L, 2L, 4L, 5L))), -1L)

  log1 <- pyrt::py_list()
  expect_identical(mod$try_return_with_finally(log1, 5L), 50L)
  expect_identical(log1$as_r_list(), list("finally"))

  log2 <- pyrt::py_list()
  expect_identical(mod$try_return_with_finally(log2, -3L), -1L)
  expect_identical(log2$as_r_list(), list("caught:negative", "finally"))

  expect_identical(mod$classify(0L), "zero")
  expect_identical(mod$classify(pyrt::py_list(list(2L, 9L))), "asc:2,9")
  expect_identical(mod$classify(pyrt::py_list(list(9L, 2L))), "other")
  expect_identical(mod$classify(pyrt::py_dict(list(list("kind", "pt"), list("x", 42L)))), "pt:42")
})

test_that("comprehensions and lazy CPS generators (including infinite generators and yield from) work", {
  mod <- eval_python("
squares = [x * x for x in range(6) if x % 2 == 1]
lookup = {x: x + 10 for x in range(3)}
uniq = {x % 3 for x in range(10)}

# Infinite generator: must evaluate lazily without hanging!
def count_from(start=0, step=1):
    n = start
    while True:
        yield n
        n += step

def take(k, iterable):
    it = iter(iterable)
    for _ in range(k):
        try:
            yield next(it)
        except StopIteration:
            return

def chain_two(a, b):
    yield from a
    yield from b

def accumulator(initial=0):
    total = initial
    while True:
        delta = yield total
        if delta is not None:
            total += delta
")

  expect_identical(mod$squares$as_r_list(), list(1L, 9L, 25L))
  expect_identical(mod$lookup$get(2L), 12L)
  expect_identical(mod$uniq$as_r_list(), list(0L, 1L, 2L))

  inf_gen <- mod$count_from(10L, 5L)
  first_four <- mod$take(4L, inf_gen)
  expect_identical(pyrt::py_collect(first_four), list(10L, 15L, 20L, 25L))
  # Next value from `inf_gen` is 30L, proving `inf_gen` only advanced 4 steps!
  expect_identical(pyrt::py_next(inf_gen), 30L)

  chained <- mod$chain_two(list(1L, 2L), list(3L, 4L))
  expect_identical(pyrt::py_collect(chained), list(1L, 2L, 3L, 4L))

  acc <- mod$accumulator(100L)
  expect_identical(pyrt::py_next(acc), 100L)
  expect_identical(acc$send(15L), 115L)
  expect_identical(acc$send(5L), 120L)
})

test_that("classes, multiple inheritance, 0-arg super(), properties, and dunders work", {
  mod <- eval_python("
class A:
    def path(self):
        return 'A'

class B(A):
    def path(self):
        return 'B->' + super().path()

class C(A):
    def path(self):
        return 'C->' + super().path()

class D(B, C):
    def path(self):
        return 'D->' + super().path()

class Box:
    scale = 2

    def __init__(self, v):
        self._v = v

    @property
    def value(self):
        return self._v * self.scale

    @value.setter
    def value(self, nv):
        self._v = nv // self.scale

    @staticmethod
    def tag():
        return 'box-tag'

    @classmethod
    def unit(cls):
        return cls(1)

    def __add__(self, other):
        return Box(self._v + other._v)

    def __repr__(self):
        return f'Box({self._v})'
")

  d <- mod$D()
  expect_identical(d$path(), "D->B->C->A")

  b1 <- mod$Box(5L)
  expect_identical(b1$value, 10L)
  b1$value <- 30L
  expect_identical(b1$`_v`, 15L)
  expect_identical(mod$Box$tag(), "box-tag")
  b_unit <- mod$Box$unit()
  expect_identical(b_unit$value, 2L)

  b_sum <- pyrt::py_add(b1, b_unit)
  expect_identical(pyrt::py_repr(b_sum), "Box(16)")
})

test_that("transpiled Python random module calls match CPython 3.12 bit-for-bit", {
  mod <- eval_python("
import random

def sample_draws(seed_val):
    random.seed(seed_val)
    return [random.randint(1, 1000) for _ in range(5)]
")
  expect_identical(
    mod$sample_draws(42L)$as_r_list(),
    list(655L, 115L, 26L, 760L, 282L)
  )
})
