
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

import parser, strutils, sequtils, options, sugar, tables

type
  SpecError* = ref object of ValueError
    index*: int
    message*: string

  Spec = Option[int]

  WordTable* = Table[string, (seq[Term], (Spec, Spec))]

func `$`*(spec: Spec): string =
  if spec.is_some:
    "(" & cycle("x", spec.get).join(" ") & ")"
  else: "(?)"

func infer(term: Term, state: WordTable): (Spec, Spec) =
  case term.kind:
    of TermLit, TermSym, TermQuote: 
      return (some(0), some(1))
    of TermWord: 
      case term.word:
        of "def": 
          return (some(2), some(0))
        of "and", "or", "gt", "geq", "lt", "leq", "eq", "+", "-", "*", "/":
          return (some(2), some(1))
        of "k":
          return (some(2), none(int))
        of "cake":
          return (some(2), some(2))
        of "if":
          return (some(3), none(int))
        else:
          try:
            let (_, spec) = state[term.word]
            return spec
          except KeyError:
            return (none(int), none(int))

func infer*(terms: seq[Term], state: WordTable): (Spec, Spec) =
    var before = some(0)
    var after = some(0)
    for term in terms:
      if before.is_some:
        if after.is_some:
          let (b, a) = term.infer state
          if b.is_some: # before.is_some, after.is_some, b.is_none
            if after.get < b.get:
              let diff = b.get - after.get
              before = before.map(b_val => b_val + diff)
              after = a.map(a_val => a_val + diff)
            elif after.get > b.get:
              after = a.map(a_val => a_val + after.get - b.get)
            else:
              after = a
          else: # before.is_some, after.is_some, b.is_none, a.is_none
            assert a.is_none
            after = none(int)
      else: # before.is_none, after.is_none
        assert after.is_none
    return (before, after)

func unify*(actual, expected: Spec, term: Term) {.raises: [SpecError].} =
  if actual.is_some and expected.is_some:
    if actual.get == expected.get: discard
    elif actual.get + 1 == expected.get:
      raise SpecError(index: term.start, message: "'" & $term & "' expects an additional item on the stack")
    else:
      raise SpecError(index: term.start, message: "'" & $term & "' expects " & $expected & " here but got " & $actual)
      
