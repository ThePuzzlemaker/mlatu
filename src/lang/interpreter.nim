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
    origin*: Origin
    message*: string

  ValueKind = enum ValueLit, ValueQuot

  Value = object
    case kind: ValueKind:
      of ValueLit: lit: Lit
      of ValueQuot: terms: seq[Term]

  Stack* = seq[Value]

func normalize(terms: seq[Term], state: WordTable): seq[Term] {.raises: [], tags: [].}

func is_equal(a, b: Value, state: WordTable): bool =
  case a.kind:
    of ValueLit: return b.kind == ValueLit and a.lit == b.lit
    of ValueQuot: return b.kind == ValueQuot and normalize(a.terms, state) == normalize(b.terms, state)

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

func pop_num(stack: var Stack, origin: Origin): int {.raises: [EvalError].} =
  if stack.len > 0:
    let top = stack.pop
    if top.kind == ValueLit:
      let lit = top.lit
      if top.lit.kind == LitNum: return lit.int_val
  raise EvalError(origin: origin, message: "expected number on the stack")

func pop_quot(stack: var Stack, origin: Origin): seq[Term] {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueQuot: return top.terms
      else: discard
  except IndexDefect: discard
  raise EvalError(origin: origin, message: "expected quotation on the stack")

func pop_bool(stack: var Stack, origin: Origin): bool {.raises: [EvalError].} =
  if stack.len > 0:
    let top = stack.pop
    if top.kind == ValueLit:
      let lit = top.lit
      if top.lit.kind == LitBool: return lit.bool_val
  raise EvalError(origin: origin, message: "expected boolean on the stack")

func pop_val(stack: var Stack, origin: Origin): Value {.raises: [EvalError].} =
  try:
    return stack.pop
  except IndexDefect: discard
  raise EvalError(origin: origin, message: "expected value on the stack")

func uneval(value: Value): Term {.raises: [].} =
  case value.kind:
    of ValueLit: Term(kind: TermLit, lit: value.lit)
    of ValueQuot: Term(kind: TermQuote, inner: value.terms)

const RECURSION_LIMIT = 1000

func eval*(stack: var Stack, state: var WordTable, terms: seq[Term],
    called: int, caller: string, should_normalize = true) {.raises: [EvalError], tags: [].} =
  var index = 0
  while index < terms.len:
    let term = terms[index]
    index.inc
    case term.kind:
      of TermLit: stack.push_lit term.lit
      of TermQuote: stack.push_quot term.inner
      of TermDef: 
        if should_normalize:
          state[term.name] = (normalize(term.body, state), term.body.infer(state))
        else:
          state[term.name] = (term.body, term.body.infer(state))
      of TermWord:
        case term.word:
          of "and":
            let a = stack.pop_bool term.origin
            let b = stack.pop_bool term.origin
            stack.push_bool(a and b)
          of "not":
            let a = stack.pop_bool term.origin
            stack.push_bool(not a)
          of "or":
            let a = stack.pop_bool term.origin
            let b = stack.pop_bool term.origin
            stack.push_bool(a or b)
          of "gt":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_bool(b > a)
          of "geq":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_bool(b >= a)
          of "lt":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_bool(b < a)
          of "leq":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_bool(b <= a)
          of "eq":
            let a = stack.pop_val term.origin
            let b = stack.pop_val term.origin
            stack.push_bool(is_equal(a, b, state))
          of "+":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_num(b + a)
          of "-":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_num(b - a)
          of "*":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_num(b * a)
          of "/":
            let a = stack.pop_num term.origin
            let b = stack.pop_num term.origin
            stack.push_num(b /% a)
          of "if":
            let a = stack.pop_quot term.origin
            let b = stack.pop_quot term.origin
            let c = stack.pop_bool term.origin

            if c:
              stack.eval(state, b, 0, "")
            else:
              stack.eval(state, a, 0, "")
          of "k":
            let a = stack.pop_quot term.origin
            discard stack.pop_val term.origin
            stack.eval state, a, 0, ""
          of "cake":
            let a = stack.pop_quot term.origin
            let b = stack.pop_val(term.origin).uneval
            stack.push_quot(normalize(@[b] & a, state))
            stack.push_quot(normalize(a & @[b], state))
          else:
            try:
              if called > RECURSION_LIMIT:
                raise EvalError(origin: term.origin,
                    message: "recursion limit reached")
              let (body, _) = state[term.word]
              stack.eval(state, body, if caller == term.word: called +
                  1 else: 0, term.word)
            except KeyError:
              raise EvalError(origin: term.origin, message: "Unknown word `" &
                  term.word & "`")

func normalize(terms: seq[Term], state: WordTable): seq[Term] {.raises: [], tags: [].} =
  result = terms
  var stack: Stack = @[]
  var state = state
  try:
    stack.eval state, terms, 0, ""
    result = stack.map(uneval)
  except EvalError: discard
  return

func display_stack*(stack: Stack): string =
  stack.map_it($it.uneval).join(" ")

proc eval_prelude(): WordTable {.raises: [], tags: [].} =
  var stack: Stack = @[]
  let toks = "prelude.mlt".static_read.scan
  try:
    let terms = toks.parse
    stack.eval result, terms, 0, "", should_normalize = false
  except ParseError as e:
    let message = "Prelude is ill-formed: parsing raised exception at " &
        $e.origin & " (" & e.message & ")"
    raise newException(Defect, message)
  except EvalError as e:
    let message = "Prelude is ill-formed: evaluation raised exception (" &
        e.message & ")"
    raise newException(Defect, message)


const PRELUDE* = eval_prelude()
