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
import termdiff, window_manager, repl, std/exitprocs, parseopt, options
import lang/scanner, lang/parser, lang/checker, lang/interpreter

proc repl() =
  setup_term()
  add_exit_proc quit_app
  var cur_screen = make_term_screen()
  var window_constructors = @[
    make_window_constructor("REPL", make_repl)
  ]
  var app = window_constructors.make_app
  var root_pane = Pane(kind: PaneWindow, window: app.make_repl)

  app.root_pane = root_pane
  block:
    var ren = cur_screen.make_term_renderer
    app.render ren
    cur_screen.show_all
  while true:
    let key = read_key()
    if key.kind == KeyMouse:
      app.process_mouse read_mouse()
    else:
      if app.process_key key:
        quit_app()
        break
    var screen = make_term_screen()
    var ren = screen.make_term_renderer
    app.render ren
    cur_screen.apply screen
    cur_screen = screen


when isMainModule:
  var filename: string
  for kind, key, val in getopt():
    case kind:
      of cmdArgument:
        filename = key
      of cmdLongOption, cmdShortOption: discard
      of cmdEnd: false.assert
  if filename != "":
    let text = filename.read_file
    let tokens = text.scan
    try:
      let terms = tokens.parse
      var state = PRELUDE
      if terms.len > 0:
        let (f, _) = terms.infer(state)
        some(0).unify f, terms[terms.high]
      var stack = new_stack()
      stack.eval(state, terms, 0, "")
      echo stack.display_stack
    except ParseError as e:
      echo "At ", e.origin, ": ", e.message
    except SpecError as e:
      echo "At ", e.origin, ": ", e.message
    except EvalError as e:
      echo "At ", e.origin, ": ", e.message
  else: repl()