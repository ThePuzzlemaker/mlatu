# Copyright (C) 2021 (Caden Haustein) <mlatu@brightlysalty.33mail.com>
# This file is part of the Mlatu programming language.
#
# The Mlatu programming language is non-violent software: you can use, 
# redistribute, and/or modify it under the terms of the CNPLv6+ as found
# in the LICENSE file in the source code root directory or
# at <https://git.pixie.town/thufie/CNPL>.
#
# The Mlatu programming language comes with ABSOLUTELY NO WARRANTY, to the 
# extent permitted by applicable law.  See the CNPL for details.

import strutils, sequtils, tables
import scanner, parser, checker

type
  EvalError* = ref object of ValueError
    index*: int
    message*: string

  ValueKind = enum ValueLit, ValueQuot, ValueSym

  Value = object
    case kind: ValueKind:
      of ValueLit: lit: Lit
      of ValueQuot: terms: seq[Term]
      of ValueSym: word: string

  Stack* = seq[Value]

func `==`(a, b: Value): bool =
  case a.kind:
    of ValueLit: return b.kind == ValueLit and a.lit == b.lit
    of ValueQuot: return b.kind == ValueQuot and a.terms == b.terms
    of ValueSym: return b.kind == ValueSym and a.word == b.word

func new_stack*(): Stack = @[]

func push_val(stack: var Stack, value: Value) {.raises: [].} =
  stack.add value

func push_lit(stack: var Stack, lit: Lit) {.raises: [].} =
  stack.push_val Value(kind: ValueLit, lit: lit)

func push_num(stack: var Stack, value: int) {.raises: [].} =
  stack.push_lit Lit(kind: LitNum, int_val: value)

func push_bool(stack: var Stack, value: bool) {.raises: [].} =
  stack.push_lit Lit(kind: LitBool, bool_val: value)

func push_quot(stack: var Stack, terms: seq[Term]) {.raises: [].} =
  stack.push_val Value(kind: ValueQuot, terms: terms)

func push_sym(stack: var Stack, word: string) {.raises: [].} =
  stack.push_val Value(kind: ValueSym, word: word)

func pop_num(stack: var Stack, index: int): int {.raises: [EvalError].} =
  if stack.len > 0:
    let top = stack.pop
    if top.kind == ValueLit:
      let lit = top.lit
      if top.lit.kind == LitNum: return lit.int_val
  raise EvalError(index: index, message: "expected number on the stack")

func pop_quot(stack: var Stack, index: int): seq[Term] {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueQuot: return top.terms
      of ValueSym: return @[Term(kind: TermSym, word: top.word)]
      else: discard
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected quotation on the stack")

func pop_sym(stack: var Stack, index: int): string {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueSym: return top.word
      else: discard
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected symbol on the stack")

func pop_bool(stack: var Stack, index: int): bool {.raises: [EvalError].} =
  if stack.len > 0:
    let top = stack.pop
    if top.kind == ValueLit:
      let lit = top.lit
      if top.lit.kind == LitBool: return lit.bool_val
  raise EvalError(index: index, message: "expected boolean on the stack")

func pop_val(stack: var Stack, index: int): Value {.raises: [EvalError].} =
  try:
    return stack.pop
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected value on the stack")

func uneval(value: Value): Term {.raises: [].} =
  case value.kind:
    of ValueLit: Term(kind: TermLit, lit: value.lit)
    of ValueSym: Term(kind: TermSym, word: value.word)
    of ValueQuot: Term(kind: TermQuote, inner: value.terms)

func eval*(stack: var Stack, state: var WordTable, terms: seq[Term], called: int, caller: string) {.raises: [EvalError], tags: [].}

const RECURSION_LIMIT = 1000

func eval*(stack: var Stack, state: var WordTable, terms: seq[Term], called: int, caller: string) {.raises: [
    EvalError], tags: [].} =
  var index = 0
  while index < terms.len:
    let term = terms[index]
    index.inc
    case term.kind:
      of TermLit: stack.push_lit term.lit
      of TermSym: stack.push_sym term.word
      of TermQuote: stack.push_quot term.inner
      of TermWord:
        case term.word:
              of "def":
                let body = stack.pop_quot term.start
                let name = stack.pop_sym term.start
                state[name] = (body, body.infer(state))
              of "and":
                let a = stack.pop_bool term.start
                let b = stack.pop_bool term.start
                stack.push_bool(a and b)
              of "not":
                let a = stack.pop_bool term.start
                stack.push_bool(not a)
              of "or":
                let a = stack.pop_bool term.start
                let b = stack.pop_bool term.start
                stack.push_bool(a or b)
              of "gt":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_bool(b > a)
              of "geq":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_bool(b >= a)
              of "lt":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_bool(b < a)
              of "leq":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_bool(b <= a)
              of "eq":
                let a = stack.pop_val term.start
                let b = stack.pop_val term.start
                stack.push_bool(a == b)
              of "+":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_num(b + a)
              of "-":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_num(b - a)
              of "*":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_num(b * a)
              of "/":
                let a = stack.pop_num term.start
                let b = stack.pop_num term.start
                stack.push_num(b /% a)
              of "rollup":
                let a = stack.pop_val term.start
                let b = stack.pop_val term.start
                let c = stack.pop_val term.start
                stack.push_val a
                stack.push_val c
                stack.push_val b
              of "rolldown":
                let a = stack.pop_val term.start
                let b = stack.pop_val term.start
                let c = stack.pop_val term.start
                stack.push_val b
                stack.push_val a
                stack.push_val c
              of "rotate":
                let a = stack.pop_val term.start
                let b = stack.pop_val term.start
                let c = stack.pop_val term.start
                stack.push_val a
                stack.push_val b
                stack.push_val c
              of "if":
                let a = stack.pop_quot term.start
                let b = stack.pop_quot term.start
                let c = stack.pop_bool term.start

                if c:
                  stack.eval(state, b, 0, "")
                else:
                  stack.eval(state, a, 0, "")
              of "k":
                let a = stack.pop_quot term.start
                discard stack.pop_val term.start
                stack.eval state, a, 0, ""
              of "cake":
                let a = stack.pop_quot term.start
                let b = stack.pop_val(term.start).uneval
                stack.push_quot(@[b] & a)
                stack.push_quot(a & @[b])
              else:
                try:
                  if called > RECURSION_LIMIT: raise EvalError(index: term.start, message: "recursion limit reached")
                  let (body, _) = state[term.word]
                  stack.eval(state, body, if caller == term.word: called + 1 else: 0, term.word)
                except KeyError:
                  raise EvalError(index: term.start, message: "Unknown word `" &
                      term.word & "`")

func display_stack*(stack: Stack): string =
  stack.map_it($it.uneval).join(" ")

proc eval_prelude(): WordTable {.raises: [], tags: [].} =
  var stack: Stack = @[]
  let toks = "prelude.mlt".static_read.scan
  try:
    let terms = toks.parse
    stack.eval result, terms, 0, ""
  except ParseError as e:
    let message = "Prelude is ill-formed: parsing raised exception at " & $e.index & " (" & e.message & ")"
    raise newException(Defect, message)
  except EvalError as e:
    let message = "Prelude is ill-formed: evaluation raised exception (" & e.message & ")"
    raise newException(Defect, message)


const PRELUDE* = eval_prelude()
