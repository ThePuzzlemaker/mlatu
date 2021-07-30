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

import scanner, strutils, sequtils

type
  ParseError* = ref object of ValueError
    origin*: Origin
    message*: string

  TermKind* = enum TermLit, TermQuote, TermWord, TermDef

  Term* = object
    origin*: Origin
    case kind*: TermKind:
      of TermLit: lit*: Lit
      of TermQuote: inner*: seq[Term]
      of TermWord: word*: string
      of TermDef:
        name*: string
        body*: seq[Term]

func `==`*(a, b: Term): bool =
  case a.kind:
    of TermWord: return b.kind == TermWord and a.word == b.word
    of TermLit: return b.kind == TermLit and a.lit == b.lit
    of TermQuote: return b.kind == TermQuote and a.inner == b.inner
    of TermDef: return b.kind == TermDef and a.name == b.name and a.body == b.body

func parse_quot(toks: seq[Tok]): (seq[Term], seq[Tok]) =
  var index = 0
  var toks = toks
  var acc: seq[Term]
  while index < toks.len:
    let tok = toks[index]
    index.inc
    case tok.kind
      of TokLeftParen:
        let (inner, new_toks) = parse_quot(toks[index..toks.high])
        index = 0
        toks = new_toks
        acc.add Term(kind: TermQuote, inner: inner, origin: tok.origin)
      of TokRightParen:
        return (acc, toks[index..toks.high])
      of TokLit: acc.add Term(kind: TermLit, lit: tok.lit, origin: tok.origin)
      of TokWord: acc.add Term(kind: TermWord, word: tok.word,
          origin: tok.origin)
      of TokBang: raise ParseError(origin: tok.origin, message: "Did not expect word definition in quotation")
  return (acc, @[])

func parse*(toks: seq[Tok]): seq[Term] {.raises: [ParseError].} =
  var index = 0
  var toks = toks
  while index < toks.len:
    let tok = toks[index]
    index.inc
    case tok.kind:
      of TokLeftParen:
        let (inner, new_toks) = parse_quot(toks[index..toks.high])
        index = 0
        toks = new_toks
        result.add Term(kind: TermQuote, inner: inner, origin: tok.origin)
      of TokRightParen:
        raise ParseError(origin: tok.origin, message: "Expected `(` before `)`")
      of TokLit: result.add Term(kind: TermLit, lit: tok.lit,
          origin: tok.origin)
      of TokWord: result.add Term(kind: TermWord, word: tok.word, origin: tok.origin)
      of TokBang: 
        if toks[index].kind == TokLeftParen:
          let (inner, new_toks) = parse_quot(toks[(index + 1)..toks.high])
          index = 0
          toks = new_toks
          result.add Term(kind: TermDef, name: tok.name, body: inner, origin: tok.origin)
        else:
          raise ParseError(origin: tok.origin, message: "Expected `(` after `!<word>`")

func `$`*(term: Term): string =
  case term.kind:
    of TermWord: return term.word
    of TermLit: return $term.lit
    of TermDef: 
      result &= "!"
      result &= term.name
      result &= " ("
      if term.body.len > 0:
        result &= " "
        result &= term.body.map(`$`).join(" ")
      result &= " )"
    of TermQuote:
      result &= "("
      if term.inner.len > 0: 
        result &= " "
        result &= term.inner.map(`$`).join(" ")
      result &= " )"