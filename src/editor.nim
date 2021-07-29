# This file is part of the Mlatu programming language.
#
# The Mlatu programming language is non-violent software: you can use,
# redistribute, and/or modify it under the terms of the CNPLv6+ as found
# in the LICENSE file in the source code root directory or
# at <https://git.pixie.town/thufie/CNPL>.
#
# The Mlatu programming language comes with ABSOLUTELY NO WARRANTY, to the
# extent permitted by applicable law.  See the CNPL for details.

import sequtils, strutils, os, sugar, unicode, tables
import utils, ui_utils, os_utils, termdiff, buffer, window_manager

type
  PromptField = object
    title: string
    entry: Entry

  PromptKind = enum PromptNone, PromptActive, PromptInactive, PromptInfo

  PromptCallbackProc = proc (editor: Editor, inputs: seq[seq[Rune]]) {.tags: [
      ReadDirEffect, WriteIOEffect].}

  Prompt = object
    case kind: PromptKind:
      of PromptNone: discard
      of PromptActive, PromptInactive:
        title: string
        fields: seq[PromptField]
        selected_field: int
        callback: PromptCallbackProc
      of PromptInfo:
        lines: seq[string]

  FileEntry = object
    name: string
    path: string
    changed: bool

  QuickOpen = ref object
    entry: Entry
    files: seq[FileEntry]
    shown_files: seq[FileEntry]
    list: List

  DialogKind = enum DialogNone, DialogQuickOpen

  Dialog = object
    case kind: DialogKind:
      of DialogNone: discard
      of DialogQuickOpen:
        quick_open: QuickOpen

  Editor* = ref object of Window
    buffer*: Buffer
    prompt: Prompt
    dialog: Dialog
    app: App
    scroll: Index2d
    detach_scroll: bool
    cursors*: seq[Cursor]
    jump_stack: seq[int]
    window_size: Index2d

proc index_files(dir: string): seq[string] {.tags: [ReadDirEffect].} =
  for item in dir.walk_dir:
    case item.kind:
      of pcFile: result.add item.path
      of pcDir: result &= item.path.index_files
      else: discard

proc index_project(dir: string): seq[FileEntry] {.tags: [ReadDirEffect].} =
  dir.index_files.map_it(FileEntry(path: it, name: os_utils.relative_path(it, dir)))

func display_name(entry: FileEntry): string =
  result = entry.name
  if entry.changed:
    result &= "*"

proc index_project(): seq[FileEntry] {.tags: [ReadDirEffect].} =
  get_current_dir().index_project

func update_list(quick_open: QuickOpen) =
  quick_open.shown_files = quick_open.files.filter_it((
      $quick_open.entry.text).to_lower in it.name.to_lower)
  quick_open.list.items = quick_open.shown_files.map_it(
      it.display_name.to_runes)
  quick_open.list.selected = quick_open.list.selected.clamp(0,
      quick_open.list.items.high)

proc load_file(editor: Editor, path: string) {.tags: [ReadIOEffect].}

proc open_selected(quick_open: QuickOpen, editor: Editor) {.tags: [
    ReadIOEffect].} =
  if quick_open.list.selected in 0..<quick_open.shown_files.len:
    let path = quick_open.shown_files[quick_open.list.selected].path
    editor.load_file path
    editor.dialog = Dialog(kind: DialogNone)

proc process_mouse(quick_open: QuickOpen, editor: Editor,
    mouse: Mouse): bool {.tags: [ReadIOEffect].} =
  var mouse_rel = mouse
  mouse_rel.x -= "Search: ".len
  mouse_rel.y -= 2

  if mouse_rel.y == -1:
    quick_open.entry.process_mouse mouse_rel
  elif mouse_rel.y == -2:
    if mouse_rel.x < 0: return true
  else:
    case mouse.kind:
      of MouseUp:
        if quick_open.list.process_mouse mouse_rel:
          quick_open.open_selected editor
      else:
        discard quick_open.list.process_mouse(mouse_rel)

proc process_key(quick_open: QuickOpen, editor: Editor, key: Key) =
  case key.kind:
    of KeyReturn:
      quick_open.open_selected editor
    of LIST_KEYS:
      quick_open.list.process_key key
    else:
      quick_open.entry.process_key key
      quick_open.update_list

proc render(quick_open: QuickOpen, box: Box, ren: var TermRenderer) =
  ren.move_to box.min.x, box.min.y
  let label = "Search:"
  let title = " ".repeat(label.len + 1) & strutils.align_left("Quick Open",
      box.size.x - label.len - 1)
  ren.put title
  ren.move_to box.min.x, box.min.y + 1
  ren.put label
  ren.put " "
  quick_open.entry.render ren

  for y in 2..<box.size.y:
    ren.move_to box.min.x, y + box.min.y
    ren.put " ".repeat(label.len)
    ren.put " "

  quick_open.list.render Box(
    min: box.min + Index2d(x: label.len + 1, y: 2),
    max: box.max
  ), ren

proc make_quick_open(app: App): owned QuickOpen {.tags: [ReadDirEffect].} =
  let files = index_project().map_it(FileEntry(path: it.path, name: it.name,
      changed: app.is_changed(it.path)))
  return QuickOpen(entry: app.copy_buffer.make_entry, files: files,
      shown_files: files, list: files.map(display_name).make_list)

proc process_key(dialog: Dialog, editor: Editor, key: Key) =
  case dialog.kind:
    of DialogNone: discard
    of DialogQuickOpen: dialog.quick_open.process_key editor, key

proc process_mouse(dialog: Dialog, editor: Editor, mouse: Mouse): bool {.tags: [
    ReadIOEffect].} =
  case dialog.kind:
    of DialogNone: discard
    of DialogQuickOpen:
      return dialog.quick_open.process_mouse(editor, mouse)

proc render(dialog: Dialog, box: Box, ren: var TermRenderer) =
  case dialog.kind:
    of DialogNone: discard
    of DialogQuickOpen: dialog.quick_open.render box, ren

func compute_size(prompt: Prompt): int =
  case prompt.kind:
    of PromptNone: 0
    of PromptActive, PromptInactive: prompt.fields.len + 1
    of PromptInfo: prompt.lines.len

func process_key(prompt: var Prompt, key: Key) =
  if prompt.kind == PromptActive:
    case key.kind:
      of KeyArrowUp:
        if prompt.selected_field > 0:
          prompt.selected_field.dec
      of KeyArrowDown:
        if prompt.selected_field < prompt.fields.high:
          prompt.selected_field.inc
      else:
        prompt.fields[prompt.selected_field].entry.process_key key

func get_inputs(prompt: Prompt): seq[seq[Rune]] =
  prompt.fields.map_it(it.entry.text)

func show_info(editor: Editor, lines: seq[string]) =
  editor.prompt = Prompt(kind: PromptInfo, lines: lines)

func show_prompt(editor: Editor, title: string, fields: seq[string],
    callback: PromptCallbackProc = nil) =
  editor.prompt = Prompt(
    kind: PromptActive,
    title: title,
    selected_field: 0,
    fields: fields.map_it(PromptField(title: it,
        entry: editor.app.copy_buffer.make_entry)),
    callback: callback
  )

func hide_prompt(editor: Editor) =
  editor.prompt = Prompt(kind: PromptNone)

func primary_cursor(editor: Editor): Cursor =
  editor.cursors[editor.cursors.high]

func merge_cursors(editor: Editor) =
  editor.cursors = editor.cursors.merge_cursors

func update_cursor(editor: Editor, index: int, pos_raw: int, shift: bool) =
  let pos = pos_raw.clamp(0, editor.buffer.len)
  case editor.cursors[index].kind:
    of CursorInsert:
      if shift:
        editor.cursors[index] = Cursor(kind: CursorSelection,
            start: editor.cursors[index].pos, stop: pos)
      else:
        editor.cursors[index].pos = pos
    of CursorSelection:
      if shift:
        editor.cursors[index].stop = pos
      else:
        editor.cursors[index] = Cursor(kind: CursorInsert, pos: pos)

func is_under_cursor(editor: Editor, pos: int): bool =
  result = false
  for cursor in editor.cursors:
    if cursor.is_under pos:
      return true

func update_scroll(editor: Editor, size: Index2d, detach: bool) =
  let pos = editor.buffer.to_2d editor.primary_cursor.get_pos
  if not detach:
    while (pos.y - editor.scroll.y) < 4:
      editor.scroll.y -= 1
    while (pos.y - editor.scroll.y) >= size.y - 4:
      editor.scroll.y += 1
  editor.scroll.y = editor.scroll.y.clamp(0, editor.buffer.lines.len)

func jump(editor: Editor, to: int) =
  editor.jump_stack.add editor.primary_cursor.get_pos
  editor.cursors = @[Cursor(kind: CursorInsert, pos: to)]

func jump(editor: Editor, start, stop: int) =
  editor.jump_stack.add editor.primary_cursor.get_pos
  editor.cursors = @[Cursor(kind: CursorSelection, start: start, stop: stop)]

func goto_line(editor: Editor, inputs: seq[seq[Rune]]) =
  try:
    var line = parse_int($inputs[0]) - 1
    if line == -1 or line >= editor.buffer.lines.len:
      editor.show_info @["Invalid line: " & $(line + 1)]
    elif line < -1:
      editor.jump editor.buffer.lines[line]
      editor.hide_prompt
  except ValueError:
    editor.show_info @["Not a number: " & $inputs[0]]

func find_pattern(editor: Editor, inputs: seq[seq[Rune]]) =
  var pos = editor.buffer.text.find(inputs[0], editor.primary_cursor.get_pos + 1)
  if pos == -1:
    pos = editor.buffer.text.find inputs[0]
  if pos != -1:
    editor.jump pos

proc save_as(editor: Editor, inputs: seq[seq[Rune]]) =
  if inputs[0].len > 0:
    let path = ($inputs[0]).absolute_path
    let (dir, _) = path.split_path
    if dir.dir_exists:
      if editor.buffer.file_path != "" and editor.buffer.file_path in
          editor.app.buffers:
        editor.app.buffers.del editor.buffer.file_path
      editor.buffer.set_path path
      editor.app.buffers[path] = editor.buffer
      try:
        editor.buffer.save
        editor.hide_prompt
      except IOError as err:
        editor.show_info @["Error: " & err.msg]
    else:
      editor.show_info @[dir & " does not exist"]

func select_all(editor: Editor) =
  editor.cursors = @[Cursor(kind: CursorSelection, start: 0,
      stop: editor.buffer.len)]

func delete_selections(editor: Editor) =
  for it, cursor in editor.cursors:
    if cursor.kind == CursorSelection:
      let cur = cursor.sort
      editor.buffer.delete cur.start, cur.stop
      editor.cursors[it] = Cursor(kind: CursorInsert, pos: cur.start)

func copy(editor: Editor) =
  for cursor in editor.cursors:
    if cursor.kind == CursorSelection:
      let cur = cursor.sort
      let text = editor.buffer.slice(cur.start, cur.stop)
      editor.app.copy_buffer.copy text

func insert(editor: Editor, chr: Rune) =
  for it, cursor in editor.cursors:
    case cursor.kind:
      of CursorInsert:
        editor.buffer.insert cursor.pos, chr
        editor.cursors[it] = Cursor(kind: CursorInsert, pos: cursor.pos + 1)
      of CursorSelection:
        let cur = cursor.sort
        editor.cursors[it] = Cursor(kind: CursorInsert, pos: cur.stop)
        editor.buffer.replace cur.start, cur.stop, @[chr]

func insert(editor: Editor, str: seq[Rune]) =
  for it, cursor in editor.cursors:
    case cursor.kind:
      of CursorInsert:
        editor.buffer.insert cursor.pos, str
        editor.cursors[it] = Cursor(kind: CursorInsert, pos: cursor.pos + str.len)
      of CursorSelection:
        let cur = cursor.sort
        editor.cursors[it] = Cursor(kind: CursorInsert, pos: cur.stop)
        editor.buffer.replace cur.start, cur.stop, str

proc load_file(editor: Editor, path: string) {.tags: [ReadIOEffect].} =
  editor.buffer = editor.app.make_buffer path
  editor.hide_prompt
  editor.cursors = @[Cursor(kind: CursorInsert, pos: 0)]

func new_buffer(editor: Editor) =
  editor.buffer = make_buffer()
  editor.hide_prompt
  editor.cursors = @[Cursor(kind: CursorInsert, pos: 0)]

func compute_line_numbers_width(editor: Editor): int {.tags: [].} =
  var max_line_number = editor.buffer.lines.len
  while max_line_number != 0:
    result += 1
    max_line_number = max_line_number div 10

method process_mouse(editor: Editor, mouse: Mouse): bool =
  if editor.dialog.kind == DialogNone:
    editor.detach_scroll = true
    let line_numbers_width = editor.compute_line_numbers_width + 1
    let prompt_size = editor.prompt.compute_size
    let pos = editor.scroll + Index2d(x: mouse.x - line_numbers_width - 1,
        y: mouse.y - 1)
    if mouse.y >= editor.window_size.y - prompt_size:
      let field = mouse.y - (editor.window_size.y - prompt_size) - 1
      if editor.prompt.kind == PromptActive or editor.prompt.kind == PromptInActive:
        case mouse.kind:
          of MouseDown:
            editor.prompt = Prompt(kind: PromptActive,
                fields: editor.prompt.fields,
                selected_field: editor.prompt.selected_field,
                callback: editor.prompt.callback)
            if mouse.button == 0 and field in 0 ..< editor.prompt.fields.len:
              editor.prompt.selected_field = field
              var mouse_rel = mouse
              mouse_rel.x -= editor.prompt.fields[field].title.len
              mouse_rel.y = 0
              editor.prompt.fields[field].entry.process_mouse mouse_rel
          of MouseMove, MouseUp:
            let selected = editor.prompt.selected_field
            var mouse_rel = mouse
            mouse_rel.x -= editor.prompt.fields[selected].title.len
            mouse_rel.y = 0
            editor.prompt.fields[selected].entry.process_mouse mouse_rel
          else: discard
    elif mouse.y == 0:
      if mouse.x < line_numbers_width: return true
    else:
      case mouse.kind:
        of MouseDown:
          if mouse.button == 0:
            case mouse.clicks:
              of 1:
                editor.jump(editor.buffer.display_to_index(Index2d(x: pos.x.max(
                    0), y: pos.y.clamp(0, editor.buffer.lines.len - 1))))
              of 2:
                let index = editor.buffer.display_to_index(Index2d(x: pos.x.max(
                    0), y: pos.y.clamp(0, editor.buffer.lines.len - 1)))
                let (start, stop) = editor.buffer.word_range(index)
                editor.jump start, stop
              of 3:
                let line = pos.y.clamp(0, editor.buffer.lines.len - 1)
                let (start, stop) = editor.buffer.line_range line
                if start != -1 and stop != -1:
                  editor.jump start, stop
              else: discard
            if editor.prompt.kind == PromptActive:
              editor.prompt = Prompt(kind: PromptInactive,
                  fields: editor.prompt.fields,
                  selected_field: editor.prompt.selected_field,
                  callback: editor.prompt.callback)
        of MouseUp, MouseMove:
          if (mouse.kind == MouseUp and mouse.button == 0 and mouse.clicks ==
              1) or (mouse.kind == MouseMove and mouse.buttons[0]):
            let stop = editor.buffer.display_to_index Index2d(x: pos.x.max(0),
                y: pos.y.clamp(0, editor.buffer.lines.len - 1))
            case editor.primary_cursor.kind:
              of CursorInsert:
                let start = editor.primary_cursor.pos
                if start != stop:
                  editor.cursors = @[Cursor(kind: CursorSelection, start: start, stop: stop)]
              of CursorSelection:
                let start = editor.primary_cursor.start
                editor.cursors = @[Cursor(kind: CursorSelection, start: start, stop: stop)]
        of MouseScroll:
          editor.scroll.y += mouse.delta * 2
        else: discard
  else:
    return editor.dialog.process_mouse(editor, mouse)

func select_next(editor: Editor) =
  if editor.primary_cursor.kind == CursorSelection:
    let cur = editor.primary_cursor.sort
    let text = editor.buffer.slice(cur.start, cur.stop)
    var pos = editor.buffer.text.find(text, cur.stop)
    if pos == -1:
      pos = editor.buffer.text.find text
    if pos != -1 and pos != cur.start:
      editor.cursors.add Cursor(kind: CursorSelection, start: pos, stop: pos + text.len)
      editor.merge_cursors

func jump_back(editor: Editor) =
  if editor.jump_stack.len > 0:
    editor.cursors = @[Cursor(kind: CursorInsert, pos: editor.jump_stack.pop)]

func cut(editor: Editor) =
  editor.copy
  editor.delete_selections

func paste(editor: Editor) =
  editor.delete_selections
  editor.insert editor.app.copy_buffer.paste

func show_find(editor: Editor) =
  editor.show_prompt "Find", @["Pattern: "], find_pattern

func show_goto(editor: Editor) =
  editor.show_prompt "Go to Line", @["Line: "], goto_line

proc show_save_as(editor: Editor) =
  editor.show_prompt(
    "Save As",
    @["File Name:"],
    callback = proc(editor: Editor, inputs: seq[seq[Rune]]) =
    let path = $inputs[0]
    if path.file_exists:
      editor.show_prompt(
        "A file named " & path &
        " already exists. Do you want to replace it?",
        @["Replace (y/n): "],
        callback = proc(editor: Editor, replace_inputs: seq[seq[
            Rune]]) {.tags: [ReadDirEffect, WriteIOEffect].} =
        if ($replace_inputs[0]).to_lower.starts_with("y"):
          editor.save_as inputs
        else:
          editor.hide_prompt
      )
    else:
      editor.save_as inputs
  )

proc save(editor: Editor) =
  if editor.buffer.file_path.len > 0:
    try:
      editor.buffer.save
      editor.show_info @["File saved."]
    except IOError as err:
      editor.show_info @["Error: " & err.msg]
  else:
    editor.show_save_as

proc show_quick_open(editor: Editor) {.tags: [ReadDirEffect, WriteIOEffect].} =
  editor.dialog = Dialog(kind: DialogQuickOpen,
      quick_open: editor.app.make_quick_open)

func only_primary_cursor(editor: Editor) =
  var cur = editor.primary_cursor
  editor.cursors = @[cur]

func jump_to_matching_brackets(editor: Editor, select: bool = false) =
  for cursor in editor.cursors.mitems:
    if cursor.kind == CursorInsert:
      let match_pos = editor.buffer.match_bracket(cursor.pos)
      if match_pos != -1:
        if select:
          cursor = Cursor(kind: CursorSelection, start: cursor.pos,
              stop: match_pos)
          if cursor.start < cursor.stop:
            cursor.stop += 1
          else:
            cursor.start += 1
        else:
          cursor = Cursor(kind: CursorInsert, pos: match_pos)

func process_arrow_left(editor: Editor, ctrl: bool, shift: bool) =
  for it, cursor in editor.cursors:
    var delta = -1
    if ctrl:
      delta = (editor.buffer.skip(cursor.get_pos - 1, -1) + 1) - cursor.get_pos
    editor.cursors[it].update(delta, editor.buffer.len, shift)

func process_arrow_right(editor: Editor, ctrl: bool, shift: bool) =
  for it, cursor in editor.cursors:
    var delta = 1
    if ctrl:
      delta = (editor.buffer.skip(cursor.get_pos, 1)) - cursor.get_pos
    editor.cursors[it].update(delta, editor.buffer.len, shift)

func process_arrow_up(editor: Editor, alt: bool, ctrl: bool, shift: bool) =
  if alt and shift:
    var index = editor.buffer.to_2d(editor.primary_cursor.get_pos)
    while true:
      index.y -= 1
      if index.y < 0:
        break
      let cursor = Cursor(kind: CursorInsert, pos: editor.buffer.to_index(index))
      if cursor notin editor.cursors:
        editor.cursors.add cursor
        break
  elif not ctrl or not alt:
    for it, cursor in editor.cursors:
      var index = editor.buffer.to_2d cursor.get_pos
      index.y = max(0, index.y - 1)
      editor.update_cursor it, editor.buffer.to_index(index), shift

func process_arrow_down(editor: Editor, alt: bool, ctrl: bool, shift: bool) =
  if alt and shift:
    var index = editor.buffer.to_2d(editor.primary_cursor.get_pos)
    while true:
      index.y += 1
      if index.y > editor.buffer.lines.len - 1:
        break
      let cursor = Cursor(kind: CursorInsert, pos: editor.buffer.to_index(index))
      if cursor notin editor.cursors:
        editor.cursors.add cursor
        break
  elif not ctrl or not alt:
    for it, cursor in editor.cursors:
      var index = editor.buffer.to_2d cursor.get_pos
      index.y = min(index.y + 1, editor.buffer.lines.len - 1)
      editor.update_cursor it, editor.buffer.to_index(index), shift

func process_return(editor: Editor) =
  for it, cursor in editor.cursors:
    if cursor.kind == CursorInsert:
      let runes = editor.buffer.newline_style.to_runes
      editor.buffer.insert(cursor.pos, runes)
      editor.cursors[it] = Cursor(kind: CursorInsert, pos: cursor.pos + runes.len)

func process_page_down(editor: Editor, shift: bool) =
  for it, cursor in editor.cursors:
    var pos = editor.buffer.to_2d cursor.get_pos
    pos.y = min(pos.y + editor.window_size.y, editor.buffer.lines.len - 1)
    editor.update_cursor it, editor.buffer.to_index(pos), shift

func process_page_up(editor: Editor, shift: bool) =
  for it, cursor in editor.cursors:
    var pos = editor.buffer.to_2d cursor.get_pos
    pos.y = max(pos.y - editor.window_size.y, 0)
    editor.update_cursor it, editor.buffer.to_index(pos), shift

func process_home(editor: Editor, shift: bool) =
  for it, cursor in editor.cursors:
    var index = editor.buffer.to_2d cursor.get_pos
    if index.y == editor.buffer.lines.len - 1:
      editor.cursors[it] = Cursor(kind: CursorInsert, pos: editor.buffer.len)
    else:
      index.y = min(index.y - 1, editor.buffer.lines.len - 1)
      index.x = 0
      let pos = max(editor.buffer.to_index(index) - 1, 0)
      editor.update_cursor(it, pos, shift)

func process_backspace(editor: Editor) =
  for it, cursor in editor.cursors:
    case cursor.kind:
      of CursorSelection:
        let cur = cursor.sort()
        editor.buffer.delete cur.start, cur.stop
        editor.cursors[it] = Cursor(kind: CursorInsert, pos: cur.start)
      of CursorInsert:
        if cursor.pos > 0:
          editor.buffer.delete(cursor.pos - 1, cursor.pos)
          editor.cursors[it] = Cursor(kind: CursorInsert, pos: cursor.pos - 1)

func process_delete(editor: Editor) =
  for it, cursor in editor.cursors:
    case cursor.kind:
      of CursorSelection:
        let cur = cursor.sort
        editor.buffer.delete cur.start, cur.stop
        editor.cursors[it] = Cursor(kind: CursorInsert, pos: cur.start)
      of CursorInsert:
        if cursor.pos < editor.buffer.len:
          editor.buffer.delete(cursor.pos, cursor.pos + 1)

method process_key(editor: Editor, key: Key) =
  if key.kind != KeyUnknown and key.kind != KeyNone:
    editor.detach_scroll = false
  if key.kind == KeyChar and key.ctrl and key.chr == 'e'.Rune:
    if editor.dialog.kind != DialogNone:
      editor.dialog = Dialog(kind: DialogNone)
    elif editor.prompt.kind != PromptNone:
      editor.hide_prompt
  else:
    if editor.dialog.kind != DialogNone:
      editor.dialog.process_key editor, key
    elif editor.prompt.kind == PromptActive:
      if key.kind == KeyReturn:
        editor.prompt.callback editor, editor.prompt.get_inputs
      else:
        editor.prompt.process_key key
    else:
      defer: editor.buffer.finish_undo_frame

      case key.kind:
        of KeyArrowLeft:
          editor.process_arrow_left key.ctrl, key.shift
          editor.merge_cursors
        of KeyArrowRight:
          editor.process_arrow_right key.ctrl, key.shift
          editor.merge_cursors
        of KeyArrowUp:
          editor.process_arrow_up key.alt, key.ctrl, key.shift
          editor.merge_cursors
        of KeyArrowDown:
          editor.process_arrow_down key.alt, key.ctrl, key.shift
          editor.merge_cursors
        of KeyReturn:
          editor.process_return
          editor.merge_cursors
        of KeyPageDown:
          editor.process_page_down key.shift
          editor.merge_cursors
        of KeyPageUp:
          editor.process_page_up key.shift
          editor.merge_cursors
        of KeyHome:
          editor.process_home key.shift
          editor.merge_cursors
        of KeyBackspace:
          editor.process_backspace
          editor.merge_cursors
        of KeyDelete:
          editor.process_delete
          editor.merge_cursors
        of KeyEscape:
          editor.only_primary_cursor
          editor.merge_cursors
        of KeyPaste:
          editor.insert key.text
          editor.merge_cursors
        of KeyChar:
          if key.ctrl:
            case key.chr:
              of Rune('a'): editor.select_all
              of Rune('t'): editor.show_quick_open
              of Rune('s'): editor.save
              of Rune('n'): editor.new_buffer
              of Rune('f'): editor.show_find
              of Rune('g'): editor.show_goto
              of Rune('v'): editor.paste
              of Rune('c'): editor.copy
              of Rune('x'): editor.cut
              of Rune('b'): editor.jump_back
              of Rune('d'): editor.select_next
              of Rune('u'): editor.only_primary_cursor
              of Rune('z'): editor.buffer.undo
              of Rune('y'): editor.buffer.redo
              of Rune('o'):
                editor.jump_to_matching_brackets key.shift
              else: discard
          else:
            editor.insert key.chr
          editor.merge_cursors
        else: discard


method list_commands(editor: Editor): seq[Command] =
  return @[
    Command(
      name: "Undo",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('z'))],
      cmd: () => editor.buffer.undo()
    ),
    Command(
      name: "Redo",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('y'))],
      cmd: () => editor.buffer.redo()
    ),
    Command(
      name: "Select All",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('a'))],
      cmd: () => editor.select_all()
    ),
    Command(
      name: "Quick Open",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('t'))],
      cmd: () => editor.show_quick_open()
    ),
    Command(
      name: "Save",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('s'))],
      cmd: () => editor.save()
    ),
    Command(
      name: "Save As",
      cmd: () => editor.show_save_as()
    ),
    Command(
      name: "New Buffer",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('n'))],
      cmd: () => editor.new_buffer()
    ),
    Command(
      name: "Find",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('f'))],
      cmd: () => editor.show_find()
    ),
    Command(
      name: "Go to Line",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('g'))],
      cmd: () => editor.show_goto()
    ),
    Command(
      name: "Paste",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('v'))],
      cmd: () => editor.paste()
    ),
    Command(
      name: "Copy",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('c'))],
      cmd: () => editor.copy()
    ),
    Command(
      name: "Cut",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('x'))],
      cmd: () => editor.cut()
    ),
    Command(
      name: "Jump Back",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('b'))],
      cmd: () => editor.jump_back()
    ),
    Command(
      name: "Select Next",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('d'))],
      cmd: () => editor.select_next()
    ),
    Command(
      name: "Only Primary Cursor",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('u'))],
      cmd: () => editor.only_primary_cursor()
    ),
    Command(
      name: "Jump to Matching Brackets",
      shortcut: @[Key(kind: KeyChar, ctrl: true, chr: Rune('o'))],
      cmd: () => editor.jump_to_matching_brackets()
    ),
    Command(
      name: "Select Brackets",
      shortcut: @[Key(kind: KeyChar, ctrl: true, shift: true, chr: Rune('o'))],
      cmd: () => editor.jump_to_matching_brackets(true)
    )
  ]

method render(editor: Editor, box: Box, ren: var TermRenderer) =
  if editor.dialog.kind == DialogNone:
    let line_numbers_width = editor.compute_line_numbers_width + 1
    let prompt_size = editor.prompt.compute_size
    editor.window_size = box.size
    editor.update_scroll(box.size - Index2d(x: line_numbers_width + 1,
        y: prompt_size), editor.detach_scroll)
    ren.move_to box.min
    let title = editor.buffer.display_file_name
    let title_aligned = strutils.align_left(title, max(0, box.size.x - 1 -
        line_numbers_width))
    let title_output = repeat(' ', line_numbers_width + 1) & title_aligned
    ren.put title_output, reverse = true

    for y in 0 ..< (box.size.y - prompt_size - 1):
      ren.move_to(box.min.x, box.min.y + y + 1)
      let it = y + editor.scroll.y
      if it >= editor.buffer.lines.len:
        ren.put repeat(' ', line_numbers_width), reverse = true
      else:
        ren.put align($(it + 1), line_numbers_width, padding = ' '), reverse = true

    var bracket_matches: seq[int] = @[]
    for cursor in editor.cursors:
      if cursor.kind == CursorInsert:
        let match = editor.buffer.match_bracket cursor.pos
        if match != -1: bracket_matches.add match

    var shifts: Table[int, int]
    var display_shifts: Table[int, int]

    block:
      let width = box.size.x - line_numbers_width - 1
      if width <= 0:
        break
      for cursor in editor.cursors:
        var pos = editor.buffer.to_display_2d(cursor.get_pos)
        shifts[pos.y] = 0
        display_shifts[pos.y] = 0
        var stopped = false
        while pos.x >= width and not stopped:
          let initial_x = pos.x
          while abs(pos.x - initial_x) < width:
            let index = editor.buffer.lines[pos.y] + shifts[pos.y]
            if index > editor.buffer.text.len:
              stopped = true
              break
            let chr = editor.buffer.text[index]
            case chr:
              of '\t': pos.x -= 2
              of '\n':
                stopped = true
                break
              else: pos.x -= 1
            shifts[pos.y] += 1
          display_shifts[pos.y] += abs(pos.x - initial_x)
    for y in 0..<(box.size.y - prompt_size - 1):
      let it = y + editor.scroll.y
      if it >= editor.buffer.lines.len:
        break
      var index = editor.buffer.lines[it]
      var reached_end = false
      var x = 0

      var index_shift = 0
      if it in shifts:
        index_shift = shifts[it]
      index += index_shift
      x += index_shift

      if index_shift != 0:
        ren.move_to(line_numbers_width + box.min.x, y + box.min.y + 1)
        ren.put('<', fg = red())
      else:
        ren.move_to(line_numbers_width + 1 + box.min.x, y + box.min.y + 1)

      while index < editor.buffer.len and editor.buffer[index] != '\n':
        if index - editor.buffer.lines[it] + line_numbers_width + 1 -
            index_shift >= box.size.x:
          reached_end = true
          break

        let chr = editor.buffer[index]

        if chr == '\t':
          if editor.is_under_cursor(index):
            ren.put("  ", reverse = true)
          else:
            ren.put "│»"
          index += 1
          x += 2
        elif chr == '\r':
          if editor.is_under_cursor index:
            ren.put(' ', reverse = true)
          else:
            ren.put "←"
          index += 1
          x += 1
        elif chr == '\0':
          if editor.is_under_cursor index:
            ren.put('0', fg = bright_red(), reverse = true)
          else:
            ren.put('0', fg = bright_red())
          index += 1
          x += 1
        elif editor.is_under_cursor index:
          ren.put chr, reverse = true
          index += 1
          x += 1
        elif index in bracket_matches:
          ren.put chr, fg = bright_red()
          index += 1
          x += 1
        else:
          ren.put chr
          index += 1
          x += 1

      if index - editor.buffer.lines[it] + line_numbers_width + 1 -
          index_shift >= box.size.x:
        reached_end = true

      if editor.is_under_cursor(index) and (not reached_end):
        ren.put(' ', reverse = true)

    case editor.prompt.kind:
      of PromptInfo:
        for it, line in editor.prompt.lines:
          ren.move_to(box.min.x, box.min.y + box.size.y - prompt_size + it)
          ren.put strutils.repeat(' ', line_numbers_width + 1) &
              strutils.align_left(editor.prompt.title, max(box.size.x -
              line_numbers_width - 1, 0))
      of PromptActive, PromptInactive:
        ren.move_to(box.min.x, box.min.y + box.size.y - prompt_size)
        ren.put strutils.repeat(' ', line_numbers_width + 1) &
            strutils.align_left(editor.prompt.title, box.size.x -
            line_numbers_width - 1)

        for it, field in editor.prompt.fields:
          ren.move_to(box.min.x, box.min.y + box.size.y - prompt_size + it + 1)
          ren.put field.title
          if it == editor.prompt.selected_field:
            field.entry.render ren
          else:
            ren.put field.entry.text
      else: discard
  else: editor.dialog.render box, ren


func make_editor*(app: App, buffer: Buffer): Editor =
  return Editor(buffer: buffer, scroll: Index2d(x: 0, y: 0), cursors: @[Cursor(
      kind: CursorInsert, pos: 0)], app: app)

func make_editor*(app: App): Window =
  app.make_editor make_buffer()

proc make_editor*(app: App, path: string): Window =
  app.make_editor app.make_buffer(path)
