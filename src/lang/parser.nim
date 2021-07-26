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
    index*: int
    message*: string

  TermKind* = enum TermLit, TermQuote, TermWord, TermSym

  Term* = object
    start*: int
    stop*: int
    case kind*: TermKind:
      of TermLit: lit*: Lit
      of TermQuote: inner*: seq[Term]
      of TermWord, TermSym: word*: string

func `==`*(a, b: Term): bool =
  case a.kind:
    of TermWord: return b.kind == TermWord and a.word == b.word
    of TermLit: return b.kind == TermLit and a.lit == b.lit
    of TermSym: return b.kind == TermSym and a.word == b.word
    of TermQuote: return b.kind == TermQuote and a.inner == b.inner

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
        acc.add Term(kind: TermQuote, inner: inner)
      of TokRightParen:
        return (acc, toks[index..toks.high])
      of TokLit: acc.add Term(kind: TermLit, lit: tok.lit, start: tok.start, stop: tok.stop)
      of TokSym: acc.add Term(kind: TermSym, word: tok.word, start: tok.start, stop: tok.stop)
      of TokWord: acc.add Term(kind: TermWord, word: tok.word, start: tok.start, stop: tok.stop)
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
        result.add Term(kind: TermQuote, inner: inner)
      of TokRightParen:
        raise ParseError(index: tok.start, message: "Expected `(` before `)`")
      of TokLit: result.add Term(kind: TermLit, lit: tok.lit, start: tok.start, stop: tok.stop)
      of TokSym: result.add Term(kind: TermSym, word: tok.word, start: tok.start, stop: tok.stop)
      of TokWord: result.add Term(kind: TermWord, word: tok.word, start: tok.start, stop: tok.stop)

func `$`*(term: Term): string =
  case term.kind:
    of TermWord: return term.word
    of TermSym: return "( " & term.word & " )"
    of TermLit: return $term.lit
    of TermQuote:
      result &= "( "
      result &= term.inner.map(`$`).join(" ")
      result &= " )"