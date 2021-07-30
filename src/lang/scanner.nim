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

import strutils

type
  Origin* = object
    col*: int
    line*: int

  LitKind* = enum LitBool, LitNum

  Lit* = object
    case kind*: LitKind
      of LitBool: bool_val*: bool
      of LitNum: int_val*: int

  TokKind* = enum TokWord, TokLeftParen, TokRightParen, TokLit, TokBang

  Tok* = object
    origin*: Origin
    case kind*: TokKind
      of TokWord: word*: string
      of TokLit: lit*: Lit
      of TokBang: name*: string
      of TokLeftParen, TokRightParen:
        depth*: int

func `==`*(a, b: Lit): bool =
  case a.kind:
    of LitBool: return b.kind == LitBool and a.bool_val == b.bool_val
    of LitNum: return b.kind == LitNum and a.int_val == b.int_val

func `==`*(a, b: Tok): bool =
  case a.kind:
    of TokWord: return b.kind == TokWord and a.word == b.word
    of TokLit: return b.kind == TokLit and a.lit == b.lit
    of TokLeftParen: return b.kind == TokLeftParen
    of TokRightParen: return b.kind == TokRightParen
    of TokBang: return b.kind == TokBang and a.name == b.name

func parse_word(input: string, origin: Origin): Tok =
  try:
    Tok(kind: TokLit, lit: Lit(kind: LitNum, int_val: input.parse_int),
        origin: origin)
  except ValueError:
    if input == "true":
      Tok(kind: TokLit, lit: Lit(kind: LitBool, bool_val: true), origin: origin)
    elif input == "false":
      Tok(kind: TokLit, lit: Lit(kind: LitBool, bool_val: false),
          origin: origin)
    elif input.starts_with "!":
      Tok(kind: TokBang, name: input[1..input.high], origin: origin)
    else:
      Tok(kind: TokWord, word: input, origin: origin)

func scan*(input: string, start: Origin = Origin(line: 0, col: 0)): seq[Tok] =
  var acc: string
  var acc_origin: Origin
  var origin: Origin = start
  var depth: int = 0
  var index: int = 0
  while index < input.len:
    let c = input[index]
    if c in {' ', '\t', '\v', '\c', '\n', '\f', '(', ')'}:
      if acc.len > 0:
        result.add acc.parse_word(acc_origin)
        acc = ""
      if c == '(':
        result.add Tok(kind: TokLeftParen, depth: depth, origin: origin)
        depth.inc
      elif c == ')':
        depth.dec
        result.add Tok(kind: TokRightParen, depth: depth, origin: origin)
    else:
      if acc == "": acc_origin = origin
      acc.add c
    index.inc
    if c == '\n': origin.line.inc
    else: origin.col.inc
  if acc.len > 0: result.add acc.parse_word(acc_origin)
  while depth > 0:
    depth.dec
    result.add Tok(kind: TokRightParen, depth: depth, origin: origin)

func `$`*(origin: Origin): string =
  $origin.line & ":" & $origin.col

func `$`*(lit: Lit): string =
  case lit.kind:
    of LitNum: $lit.int_val
    of LitBool: $lit.bool_val

func `$`*(tok: Tok): string =
  case tok.kind:
    of TokWord: tok.word
    of TokLit: $tok.lit
    of TokLeftParen: "("
    of TokRightParen: ")"
    of TokBang: "!" & tok.name
