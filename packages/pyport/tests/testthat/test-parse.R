test_that("vendored Python grammar loads and parses without external packages", {
  lang <- python_language()
  expect_s3_class(lang, "tree_sitter_language")
  expect_identical(lang$name, "python")
  expect_identical(lang$abi, 14L)
})

test_that("parse_python matches CPython ast.dump across Python 3.12 constructs", {
  cases <- list(
    list(
      "x = 1 + 2 * 3 - 4 / 5 // 6 % 7 ** 8 @ 9",
      "Module(body=[Assign(targets=[Name(id='x', ctx=Store())], value=BinOp(left=BinOp(left=Constant(value=1), op=Add(), right=BinOp(left=Constant(value=2), op=Mult(), right=Constant(value=3))), op=Sub(), right=BinOp(left=BinOp(left=BinOp(left=BinOp(left=Constant(value=4), op=Div(), right=Constant(value=5)), op=FloorDiv(), right=Constant(value=6)), op=Mod(), right=BinOp(left=Constant(value=7), op=Pow(), right=Constant(value=8))), op=MatMult(), right=Constant(value=9))))], type_ignores=[])"
    ),
    list(
      "x = (a << 1) | (b >> 2) ^ (c & ~d)",
      "Module(body=[Assign(targets=[Name(id='x', ctx=Store())], value=BinOp(left=BinOp(left=Name(id='a', ctx=Load()), op=LShift(), right=Constant(value=1)), op=BitOr(), right=BinOp(left=BinOp(left=Name(id='b', ctx=Load()), op=RShift(), right=Constant(value=2)), op=BitXor(), right=BinOp(left=Name(id='c', ctx=Load()), op=BitAnd(), right=UnaryOp(op=Invert(), operand=Name(id='d', ctx=Load()))))))], type_ignores=[])"
    ),
    list(
      "x = not a and b or c and d",
      "Module(body=[Assign(targets=[Name(id='x', ctx=Store())], value=BoolOp(op=Or(), values=[BoolOp(op=And(), values=[UnaryOp(op=Not(), operand=Name(id='a', ctx=Load())), Name(id='b', ctx=Load())]), BoolOp(op=And(), values=[Name(id='c', ctx=Load()), Name(id='d', ctx=Load())])]))], type_ignores=[])"
    ),
    list(
      "x = 1 < a <= b == c != d is None is not False in xs not in ys",
      "Module(body=[Assign(targets=[Name(id='x', ctx=Store())], value=Compare(left=Constant(value=1), ops=[Lt(), LtE(), Eq(), NotEq(), Is(), IsNot(), In(), NotIn()], comparators=[Name(id='a', ctx=Load()), Name(id='b', ctx=Load()), Name(id='c', ctx=Load()), Name(id='d', ctx=Load()), Constant(value=None), Constant(value=False), Name(id='xs', ctx=Load()), Name(id='ys', ctx=Load())]))], type_ignores=[])"
    ),
    list(
      "a = b = c = 42\nx: int = 1\ny: list[int]\n(z): str = 'hi'",
      "Module(body=[Assign(targets=[Name(id='a', ctx=Store()), Name(id='b', ctx=Store()), Name(id='c', ctx=Store())], value=Constant(value=42)), AnnAssign(target=Name(id='x', ctx=Store()), annotation=Name(id='int', ctx=Load()), value=Constant(value=1), simple=1), AnnAssign(target=Name(id='y', ctx=Store()), annotation=Subscript(value=Name(id='list', ctx=Load()), slice=Name(id='int', ctx=Load()), ctx=Load()), simple=1), AnnAssign(target=Name(id='z', ctx=Store()), annotation=Name(id='str', ctx=Load()), value=Constant(value='hi'), simple=0)], type_ignores=[])"
    ),
    list(
      "a, *b, c = 1, 2, 3, 4\n[u, (v, w)] = seq\nx += 1\ndel a, b[0], c.d",
      "Module(body=[Assign(targets=[Tuple(elts=[Name(id='a', ctx=Store()), Starred(value=Name(id='b', ctx=Store()), ctx=Store()), Name(id='c', ctx=Store())], ctx=Store())], value=Tuple(elts=[Constant(value=1), Constant(value=2), Constant(value=3), Constant(value=4)], ctx=Load())), Assign(targets=[List(elts=[Name(id='u', ctx=Store()), Tuple(elts=[Name(id='v', ctx=Store()), Name(id='w', ctx=Store())], ctx=Store())], ctx=Store())], value=Name(id='seq', ctx=Load())), AugAssign(target=Name(id='x', ctx=Store()), op=Add(), value=Constant(value=1)), Delete(targets=[Name(id='a', ctx=Del()), Subscript(value=Name(id='b', ctx=Load()), slice=Constant(value=0), ctx=Del()), Attribute(value=Name(id='c', ctx=Load()), attr='d', ctx=Del())])], type_ignores=[])"
    ),
    list(
      "y[1:2:3, :, ::2, :5, 0] = val",
      "Module(body=[Assign(targets=[Subscript(value=Name(id='y', ctx=Load()), slice=Tuple(elts=[Slice(lower=Constant(value=1), upper=Constant(value=2), step=Constant(value=3)), Slice(), Slice(step=Constant(value=2)), Slice(upper=Constant(value=5)), Constant(value=0)], ctx=Load()), ctx=Store())], value=Name(id='val', ctx=Load()))], type_ignores=[])"
    ),
    list(
      "f(1, 2, *args, k=3, **kw)\nf(x for x in xs if x > 0)",
      "Module(body=[Expr(value=Call(func=Name(id='f', ctx=Load()), args=[Constant(value=1), Constant(value=2), Starred(value=Name(id='args', ctx=Load()), ctx=Load())], keywords=[keyword(arg='k', value=Constant(value=3)), keyword(value=Name(id='kw', ctx=Load()))])), Expr(value=Call(func=Name(id='f', ctx=Load()), args=[GeneratorExp(elt=Name(id='x', ctx=Load()), generators=[comprehension(target=Name(id='x', ctx=Store()), iter=Name(id='xs', ctx=Load()), ifs=[Compare(left=Name(id='x', ctx=Load()), ops=[Gt()], comparators=[Constant(value=0)])], is_async=0)])], keywords=[]))], type_ignores=[])"
    ),
    list(
      "[x + y for x in xs if x > 0 async for y in ys if y != x]\n{k: v for k, v in items}\n{x for x in xs}",
      "Module(body=[Expr(value=ListComp(elt=BinOp(left=Name(id='x', ctx=Load()), op=Add(), right=Name(id='y', ctx=Load())), generators=[comprehension(target=Name(id='x', ctx=Store()), iter=Name(id='xs', ctx=Load()), ifs=[Compare(left=Name(id='x', ctx=Load()), ops=[Gt()], comparators=[Constant(value=0)])], is_async=0), comprehension(target=Name(id='y', ctx=Store()), iter=Name(id='ys', ctx=Load()), ifs=[Compare(left=Name(id='y', ctx=Load()), ops=[NotEq()], comparators=[Name(id='x', ctx=Load())])], is_async=1)])), Expr(value=DictComp(key=Name(id='k', ctx=Load()), value=Name(id='v', ctx=Load()), generators=[comprehension(target=Tuple(elts=[Name(id='k', ctx=Store()), Name(id='v', ctx=Store())], ctx=Store()), iter=Name(id='items', ctx=Load()), ifs=[], is_async=0)])), Expr(value=SetComp(elt=Name(id='x', ctx=Load()), generators=[comprehension(target=Name(id='x', ctx=Store()), iter=Name(id='xs', ctx=Load()), ifs=[], is_async=0)]))], type_ignores=[])"
    ),
    list(
      "{'a': 1, **d, 'b': 2}\n(1 if cond else 2, (w := 10))",
      "Module(body=[Expr(value=Dict(keys=[Constant(value='a'), None, Constant(value='b')], values=[Constant(value=1), Name(id='d', ctx=Load()), Constant(value=2)])), Expr(value=Tuple(elts=[IfExp(test=Name(id='cond', ctx=Load()), body=Constant(value=1), orelse=Constant(value=2)), NamedExpr(target=Name(id='w', ctx=Store()), value=Constant(value=10))], ctx=Load()))], type_ignores=[])"
    ),
    list(
      "def fn(a: int, b: int = 1, /, c: str = 'x', *args: tuple, d: float, e: bool = True, **kw: Any) -> None:\n    global g1, g2\n    nonlocal n1\n    return a",
      "Module(body=[FunctionDef(name='fn', args=arguments(posonlyargs=[arg(arg='a', annotation=Name(id='int', ctx=Load())), arg(arg='b', annotation=Name(id='int', ctx=Load()))], args=[arg(arg='c', annotation=Name(id='str', ctx=Load()))], vararg=arg(arg='args', annotation=Name(id='tuple', ctx=Load())), kwonlyargs=[arg(arg='d', annotation=Name(id='float', ctx=Load())), arg(arg='e', annotation=Name(id='bool', ctx=Load()))], kw_defaults=[None, Constant(value=True)], kwarg=arg(arg='kw', annotation=Name(id='Any', ctx=Load())), defaults=[Constant(value=1), Constant(value='x')]), body=[Global(names=['g1', 'g2']), Nonlocal(names=['n1']), Return(value=Name(id='a', ctx=Load()))], decorator_list=[], returns=Constant(value=None), type_params=[])], type_ignores=[])"
    ),
    list(
      "@dec1\n@dec2(arg)\nasync def afn(x):\n    await fut(x)\n    yield x\n    yield from ys\n    return",
      "Module(body=[AsyncFunctionDef(name='afn', args=arguments(posonlyargs=[], args=[arg(arg='x')], kwonlyargs=[], kw_defaults=[], defaults=[]), body=[Expr(value=Await(value=Call(func=Name(id='fut', ctx=Load()), args=[Name(id='x', ctx=Load())], keywords=[]))), Expr(value=Yield(value=Name(id='x', ctx=Load()))), Expr(value=YieldFrom(value=Name(id='ys', ctx=Load()))), Return()], decorator_list=[Name(id='dec1', ctx=Load()), Call(func=Name(id='dec2', ctx=Load()), args=[Name(id='arg', ctx=Load())], keywords=[])], type_params=[])], type_ignores=[])"
    ),
    list(
      "@cls_dec\nclass C(A, B, metaclass=M):\n    pass",
      "Module(body=[ClassDef(name='C', bases=[Name(id='A', ctx=Load()), Name(id='B', ctx=Load())], keywords=[keyword(arg='metaclass', value=Name(id='M', ctx=Load()))], body=[Pass()], decorator_list=[Name(id='cls_dec', ctx=Load())], type_params=[])], type_ignores=[])"
    ),
    list(
      "if a:\n    x = 1\nelif b:\n    x = 2\nelse:\n    x = 3\nwhile cond:\n    break\nelse:\n    continue\nfor i, j in pairs:\n    pass\nelse:\n    pass",
      "Module(body=[If(test=Name(id='a', ctx=Load()), body=[Assign(targets=[Name(id='x', ctx=Store())], value=Constant(value=1))], orelse=[If(test=Name(id='b', ctx=Load()), body=[Assign(targets=[Name(id='x', ctx=Store())], value=Constant(value=2))], orelse=[Assign(targets=[Name(id='x', ctx=Store())], value=Constant(value=3))])]), While(test=Name(id='cond', ctx=Load()), body=[Break()], orelse=[Continue()]), For(target=Tuple(elts=[Name(id='i', ctx=Store()), Name(id='j', ctx=Store())], ctx=Store()), iter=Name(id='pairs', ctx=Load()), body=[Pass()], orelse=[Pass()])], type_ignores=[])"
    ),
    list(
      "with open('a') as f, mgr() as (x, y):\n    pass\nasync with cm() as c:\n    pass",
      "Module(body=[With(items=[withitem(context_expr=Call(func=Name(id='open', ctx=Load()), args=[Constant(value='a')], keywords=[]), optional_vars=Name(id='f', ctx=Store())), withitem(context_expr=Call(func=Name(id='mgr', ctx=Load()), args=[], keywords=[]), optional_vars=Tuple(elts=[Name(id='x', ctx=Store()), Name(id='y', ctx=Store())], ctx=Store()))], body=[Pass()]), AsyncWith(items=[withitem(context_expr=Call(func=Name(id='cm', ctx=Load()), args=[], keywords=[]), optional_vars=Name(id='c', ctx=Store()))], body=[Pass()])], type_ignores=[])"
    ),
    list(
      "try:\n    risky()\nexcept (E1, E2) as e:\n    handle(e)\nexcept E3:\n    pass\nexcept:\n    fallback()\nelse:\n    ok()\nfinally:\n    cleanup()",
      "Module(body=[Try(body=[Expr(value=Call(func=Name(id='risky', ctx=Load()), args=[], keywords=[]))], handlers=[ExceptHandler(type=Tuple(elts=[Name(id='E1', ctx=Load()), Name(id='E2', ctx=Load())], ctx=Load()), name='e', body=[Expr(value=Call(func=Name(id='handle', ctx=Load()), args=[Name(id='e', ctx=Load())], keywords=[]))]), ExceptHandler(type=Name(id='E3', ctx=Load()), body=[Pass()]), ExceptHandler(body=[Expr(value=Call(func=Name(id='fallback', ctx=Load()), args=[], keywords=[]))])], orelse=[Expr(value=Call(func=Name(id='ok', ctx=Load()), args=[], keywords=[]))], finalbody=[Expr(value=Call(func=Name(id='cleanup', ctx=Load()), args=[], keywords=[]))])], type_ignores=[])"
    ),
    list(
      "try:\n    eg()\nexcept* EG1 as e:\n    pass\nexcept* EG2:\n    pass",
      "Module(body=[TryStar(body=[Expr(value=Call(func=Name(id='eg', ctx=Load()), args=[], keywords=[]))], handlers=[ExceptHandler(type=Name(id='EG1', ctx=Load()), name='e', body=[Pass()]), ExceptHandler(type=Name(id='EG2', ctx=Load()), body=[Pass()])], orelse=[], finalbody=[])], type_ignores=[])"
    ),
    list(
      "raise ValueError('bad') from cause\nraise\nassert x > 0, 'must be positive'",
      "Module(body=[Raise(exc=Call(func=Name(id='ValueError', ctx=Load()), args=[Constant(value='bad')], keywords=[]), cause=Name(id='cause', ctx=Load())), Raise(), Assert(test=Compare(left=Name(id='x', ctx=Load()), ops=[Gt()], comparators=[Constant(value=0)]), msg=Constant(value='must be positive'))], type_ignores=[])"
    ),
    list(
      "import os, sys as system, pkg.sub as ps\nfrom ..pkg.mod import a, b as c\nfrom . import *\nfrom __future__ import annotations",
      "Module(body=[Import(names=[alias(name='os'), alias(name='sys', asname='system'), alias(name='pkg.sub', asname='ps')]), ImportFrom(module='pkg.mod', names=[alias(name='a'), alias(name='b', asname='c')], level=2), ImportFrom(names=[alias(name='*')], level=1), ImportFrom(module='__future__', names=[alias(name='annotations')], level=0)], type_ignores=[])"
    ),
    list(
      "match subj:\n    case [1, *rest] | (2, _ as z) if z > 0:\n        pass\n    case {'k': v, **more}:\n        pass\n    case Pt(1, y=w):\n        pass\n    case True | False | None:\n        pass\n    case Color.RED:\n        pass\n    case _:\n        pass",
      "Module(body=[Match(subject=Name(id='subj', ctx=Load()), cases=[match_case(pattern=MatchOr(patterns=[MatchSequence(patterns=[MatchValue(value=Constant(value=1)), MatchStar(name='rest')]), MatchSequence(patterns=[MatchValue(value=Constant(value=2)), MatchAs(pattern=MatchAs(), name='z')])]), guard=Compare(left=Name(id='z', ctx=Load()), ops=[Gt()], comparators=[Constant(value=0)]), body=[Pass()]), match_case(pattern=MatchMapping(keys=[Constant(value='k')], patterns=[MatchAs(name='v')], rest='more'), body=[Pass()]), match_case(pattern=MatchClass(cls=Name(id='Pt', ctx=Load()), patterns=[MatchValue(value=Constant(value=1))], kwd_attrs=['y'], kwd_patterns=[MatchAs(name='w')]), body=[Pass()]), match_case(pattern=MatchOr(patterns=[MatchSingleton(value=True), MatchSingleton(value=False), MatchSingleton(value=None)]), body=[Pass()]), match_case(pattern=MatchValue(value=Attribute(value=Name(id='Color', ctx=Load()), attr='RED', ctx=Load())), body=[Pass()]), match_case(pattern=MatchAs(), body=[Pass()])])], type_ignores=[])"
    ),
    list(
      "type Pair[T: int, *Ts, **P] = tuple[T, T]",
      "Module(body=[TypeAlias(name=Name(id='Pair', ctx=Store()), type_params=[TypeVar(name='T', bound=Name(id='int', ctx=Load())), TypeVarTuple(name='Ts'), ParamSpec(name='P')], value=Subscript(value=Name(id='tuple', ctx=Load()), slice=Tuple(elts=[Name(id='T', ctx=Load()), Name(id='T', ctx=Load())], ctx=Load()), ctx=Load()))], type_ignores=[])"
    ),
    list(
      "f = f'val={x!r:>10s} nested={y:{w}.{p}f} braces={{ok}} debug={x=}'",
      "Module(body=[Assign(targets=[Name(id='f', ctx=Store())], value=JoinedStr(values=[Constant(value='val='), FormattedValue(value=Name(id='x', ctx=Load()), conversion=114, format_spec=JoinedStr(values=[Constant(value='>10s')])), Constant(value=' nested='), FormattedValue(value=Name(id='y', ctx=Load()), conversion=-1, format_spec=JoinedStr(values=[FormattedValue(value=Name(id='w', ctx=Load()), conversion=-1), Constant(value='.'), FormattedValue(value=Name(id='p', ctx=Load()), conversion=-1), Constant(value='f')])), Constant(value=' braces={ok} debug=x='), FormattedValue(value=Name(id='x', ctx=Load()), conversion=114)]))], type_ignores=[])"
    )
  )

  for (tc in cases) {
    tree <- parse_python(tc[[1L]])
    expect_true(is_py_ast(tree, "Module"))
    expect_identical(ast_dump(tree), tc[[2L]], info = tc[[1L]])
  }
})

test_that("arbitrary-precision integers, floats, complex, bytes, and unicode escapes are exact", {
  tree <- parse_python(c(
    "big = 123_456_789_012_345_678_901_234_567_890",
    "hx = 0xdead_beef",
    "oc = 0o755",
    "bn = 0b1010_0001",
    "fl = (0.0, -0.0, 1e-12, 3.141592653589793, 1j, 2.5J, ...)",
    "s = 'a\\n\\x41\\u0042\\U00000043\\101' 'Z'",
    "b = b'\\x00\\xff\\n' rb'\\n'"
  ))

  big_val <- tree$body[[1L]]$value$value
  expect_s3_class(big_val, "bigz")
  expect_identical(as.character(big_val), "123456789012345678901234567890")

  hx_val <- tree$body[[2L]]$value$value
  expect_s3_class(hx_val, "bigz")
  expect_identical(as.character(hx_val), "3735928559")

  expect_identical(tree$body[[3L]]$value$value, 493L)
  expect_identical(tree$body[[4L]]$value$value, 161L)

  expect_identical(
    ast_dump(tree$body[[5L]]),
    "Assign(targets=[Name(id='fl', ctx=Store())], value=Tuple(elts=[Constant(value=0.0), UnaryOp(op=USub(), operand=Constant(value=0.0)), Constant(value=1e-12), Constant(value=3.141592653589793), Constant(value=1j), Constant(value=2.5j), Constant(value=Ellipsis)], ctx=Load()))"
  )

  expect_identical(tree$body[[6L]]$value$value, "a\nABCAZ")
  expect_identical(tree$body[[7L]]$value$value, as.raw(c(0x00, 0xff, 0x0a, 0x5c, 0x6e)))
})

test_that("eval mode, source locations, formatting, and syntax errors work", {
  expr <- parse_python("a + b", mode = "eval")
  expect_true(is_py_ast(expr, "Expression"))
  expect_identical(
    ast_dump(expr, include_attributes = TRUE),
    "Expression(body=BinOp(left=Name(id='a', ctx=Load(), lineno=1, col_offset=0, end_lineno=1, end_col_offset=1), op=Add(), right=Name(id='b', ctx=Load(), lineno=1, col_offset=4, end_lineno=1, end_col_offset=5), lineno=1, col_offset=0, end_lineno=1, end_col_offset=5), lineno=1, col_offset=0, end_lineno=1, end_col_offset=5)"
  )
  expect_identical(ast_loc(expr$body)$col_offset, 0L)
  expect_identical(ast_loc(expr$body)$end_col_offset, 5L)
  expect_match(format(expr), "Expression\\(")

  tmp <- tempfile(fileext = ".py")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("def ok(x):\n    return x + 1\n", tmp)
  from_file <- parse_python_file(tmp)
  expect_true(is_py_ast(from_file, "Module"))

  expect_error(
    parse_python("def broken(:\n    pass", filename = "broken.py"),
    class = "pyport_syntax_error"
  )
})
