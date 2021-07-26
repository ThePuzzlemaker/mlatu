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

import strutils, sequtils, tables, algorithm

type
  EvalError* = ref object of ValueError
    index*: int
    message*: string

  TokKind = enum TokWord, TokLeftParen, TokRightParen, TokNum, TokSym

  Tok = object
    start: int
    stop: int
    case kind: TokKind
      of TokWord, TokSym: word: string
      of TokNum: num: int
      of TokLeftParen, TokRightParen:
        depth: int

  ValueKind = enum ValueNum, ValueBool, ValueQuot, ValueSym

  Value = object
    case kind: ValueKind:
      of ValueNum: num: int
      of ValueBool: bool: bool
      of ValueQuot: toks: seq[Tok]
      of ValueSym: word: string

  Stack* = seq[Value]

func `==`(a, b: Tok): bool =
  case a.kind:
    of TokWord: return b.kind == TokWord and a.word == b.word
    of TokNum: return b.kind == TokNum and a.num == b.num
    of TokSym: return b.kind == TokSym and a.word == b.word
    of TokLeftParen: return b.kind == TokLeftParen
    of TokRightParen: return b.kind == TokRightParen

func `==`(a, b: Value): bool =
  case a.kind:
    of ValueNum: return b.kind == ValueNum and a.num == b.num
    of ValueQuot: return b.kind == ValueQuot and a.toks == b.toks
    of ValueBool: return b.kind == ValueBool and a.bool == b.bool
    of ValueSym: return b.kind == ValueSym and a.word == b.word

func parse_word(input: string, start: int, stop: int): Tok =
  try: Tok(kind: TokNum, num: input.parse_int, start: start, stop: stop)
  except ValueError: 
    if input[0] == ':': Tok(kind: TokSym, word: input[1..input.high], start: start, stop: stop)
    else: Tok(kind: TokWord, word: input, start: start, stop: stop)

func parse*(input: string): seq[Tok] =
  var acc: string
  var acc_index: int = 0
  var depth: int = 0
  var index: int = 0
  while index < input.len:
    let c = input[index]
    if c in {' ', '\t', '\v', '\c', '\n', '\f', '(', ')'}:
      if acc.len > 0:
        result.add acc.parse_word(acc_index, index)
        acc = ""
      if c == '(':
        result.add Tok(kind: TokLeftParen, depth: depth, start: index, stop: index)
        depth.inc
      elif c == ')':
        depth.dec
        result.add Tok(kind: TokRightParen, depth: depth, start: index, stop: index)
    else:
      if acc == "": acc_index = index
      acc.add c
    index.inc
  if acc.len > 0: result.add acc.parse_word(acc_index, index)
  while depth > 0:
    depth.dec
    result.add Tok(kind: TokRightParen, depth: depth, start: index, stop: index)

func new_stack*(): Stack = @[]

func push_num(stack: var Stack, value: int) {.raises: [].} =
  stack.add Value(kind: ValueNum, num: value)

func push_bool(stack: var Stack, value: bool) {.raises: [].} =
  stack.add Value(kind: ValueBool, bool: value)

func push_quot(stack: var Stack, toks: seq[Tok]) {.raises: [].} =
  stack.add Value(kind: ValueQuot, toks: toks)

func push_sym(stack: var Stack, word: string) {.raises: [].} =
  stack.add Value(kind: ValueSym, word: word)

func push_val(stack: var Stack, value: Value) {.raises: [].} =
  stack.add value

func pop_num(stack: var Stack, index: int): int {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueNum: return top.num
      else: discard
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected number on the stack")

func pop_quot(stack: var Stack, index: int): seq[Tok] {.raises: [EvalError].} =
  try:
    let top = stack.pop
    case top.kind:
      of ValueQuot: return top.toks
      of ValueSym: return @[Tok(kind: TokSym, word: top.word)]
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
  try:
    let top = stack.pop
    case top.kind:
      of ValueBool: return top.bool
      else: discard
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected boolean on the stack")

func pop_val(stack: var Stack, index: int): Value {.raises: [EvalError].} =
  try:
    return stack.pop
  except IndexDefect: discard
  raise EvalError(index: index, message: "expected value on the stack")

func unparse(value: Value): seq[Tok] {.raises: [].} =
  case value.kind:
    of ValueNum: result.add Tok(kind: TokNum, num: value.num)
    of ValueBool: result.add Tok(kind: TokWord, word: $value.bool)
    of ValueSym: result.add Tok(kind: TokSym, word: value.word)
    of ValueQuot:
      result.add Tok(kind: TokLeftParen)
      result &= value.toks
      result.add Tok(kind: TokRightParen)

type
  EvalState* = Table[string, seq[Tok]]

  EvalModeKind = enum EvalTop, EvalQuot
  EvalMode = object
    case kind: EvalModeKind:
      of EvalTop: discard
      of EvalQuot:
        toks: seq[Tok]
        depth: int

const RECURSION_LIMIT = 1000

func eval*(stack: var Stack, state: var EvalState, toks: seq[Tok], called: int, caller: string) {.raises: [
    EvalError], tags: [].} =
  var mode = EvalMode(kind: EvalTop)
  var toks = toks.reversed()
  while toks.len > 0:
    let tok = toks.pop
    case mode.kind:
      of EvalTop:
        case tok.kind:
          of TokLeftParen:
            mode = EvalMode(kind: EvalQuot, toks: @[], depth: tok.depth)
          of TokRightParen: raise EvalError(index: tok.start,
              message: "Expected `(` before `)`")
          of TokNum: stack.push_num tok.num
          of TokSym: stack.push_sym tok.word
          of TokWord:
            case tok.word:
              of "def":
                let body = stack.pop_quot tok.start
                let name = stack.pop_sym tok.start
                state[name] =  body
              of "true":
                stack.push_bool true
              of "false":
                stack.push_bool false
              of "and":
                let a = stack.pop_bool tok.start
                let b = stack.pop_bool tok.start
                stack.push_bool(a and b)
              of "not":
                let a = stack.pop_bool tok.start
                stack.push_bool(not a)
              of "or":
                let a = stack.pop_bool tok.start
                let b = stack.pop_bool tok.start
                stack.push_bool(a or b)
              of "gt":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_bool(b > a)
              of "geq":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_bool(b >= a)
              of "lt":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_bool(b < a)
              of "leq":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_bool(b <= a)
              of "eq":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                stack.push_bool(a == b)
              of "+":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b + a)
              of "-":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b - a)
              of "*":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b * a)
              of "/":
                let a = stack.pop_num tok.start
                let b = stack.pop_num tok.start
                stack.push_num(b /% a)
              of "dup":
                let a = stack.pop_val tok.start
                stack.push_val a
                stack.push_val a
              of "pop": discard stack.pop_val tok.start
              of "swap":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                stack.push_val a
                stack.push_val b
              of "dip":
                let a = stack.pop_quot tok.start
                let b = stack.pop_val tok.start
                stack.eval state, a, 0, ""
                stack.push_val b
              of "rollup":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                let c = stack.pop_val tok.start
                stack.push_val a
                stack.push_val c
                stack.push_val b
              of "rolldown":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                let c = stack.pop_val tok.start
                stack.push_val b
                stack.push_val a
                stack.push_val c
              of "rotate":
                let a = stack.pop_val tok.start
                let b = stack.pop_val tok.start
                let c = stack.pop_val tok.start
                stack.push_val a
                stack.push_val b
                stack.push_val c
              of "i":
                stack.eval(state, stack.pop_quot tok.start, 0, "")
              of "if":
                let a = stack.pop_quot tok.start
                let b = stack.pop_quot tok.start
                let c = stack.pop_bool tok.start

                if c:
                  stack.eval(state, b, 0, "")
                else:
                  stack.eval(state, a, 0, "")
              of "cons":
                let a = stack.pop_quot tok.start
                let b = stack.pop_val(tok.start).unparse
                stack.push_quot(b & a)
              else:
                try:
                  if called > RECURSION_LIMIT: raise EvalError(index: tok.start, message: "recursion limit reached")
                  stack.eval(state, state[tok.word], if caller == tok.word: called + 1 else: 0, tok.word)
                except KeyError:
                  raise EvalError(index: tok.start, message: "Unknown word `" &
                      tok.word & "`")
      of EvalQuot:
        if tok.kind == TokRightParen and tok.depth == mode.depth:
          stack.push_quot mode.toks
          mode = EvalMode(kind: EvalTop)
        else:
          mode.toks.add tok
  if mode.kind == EvalQuot:
    stack.push_quot mode.toks

func `$`(tok: Tok): string =
  case tok.kind:
    of TokWord: tok.word
    of TokSym: ":" & tok.word
    of TokNum: $tok.num
    of TokLeftParen: "("
    of TokRightParen: ")"

func display_stack*(stack: Stack): string =
  var acc: seq[Tok] = @[]
  for val in stack:
    acc &= val.unparse
  return acc.map_it($it).join(" ")

proc eval_prelude(): EvalState {.raises: [], tags: [].} =
  var stack: Stack = @[]
  let toks = "../prelude.mlt".static_read.parse
  try:
    stack.eval result, toks, 0, ""
    if stack.len > 0:
      raise newException(Defect, ("Prelude is ill-formed: stack contained items after evaluation (" &
          stack.display_stack & ")"))
  except EvalError as e:
    raise newException(Defect, ("Prelude is ill-formed: evaluation raised exception (" &
        e.message & ")"))

const PRELUDE* = eval_prelude()
