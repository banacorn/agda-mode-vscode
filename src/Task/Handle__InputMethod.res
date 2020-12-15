open Belt

open! Task

module EditorIM = {
  let handle = (state: State.t, output) => {
    open IM.Output
    let handle = kind =>
      switch kind {
      | UpdateView(sequence, translation, index) =>
        Promise.resolved(list{ViewEvent(InputMethod(Update(sequence, translation, index)))})
      | Rewrite(replacements, resolve) =>
        let document = state.editor->VSCode.TextEditor.document
        let replacements = replacements->Array.map(((interval, text)) => {
          let range = document->IM.fromInterval(interval)
          (range, text)
        })
        Editor.Text.batchReplace(document, replacements)->Promise.map(_ => {
          resolve()
          list{}
        })
      | Activate => Promise.resolved(list{ViewEvent(InputMethod(Activate))})
      | Deactivate => Promise.resolved(list{ViewEvent(InputMethod(Deactivate))})
      }
    output->Array.map(handle)->Util.oneByOne->Promise.map(List.concatMany)
  }

  let runAndHandle = (state: State.t, action) =>
    handle(state, IM.run(state.editorIM, Some(state.editor), action))

  let activate = (state: State.t): Promise.t<list<Task.t>> => {
    // activated the input method with cursors positions
    let document = VSCode.TextEditor.document(state.editor)
    let intervals: array<(int, int)> =
      Editor.Selection.getMany(state.editor)->Array.map(IM.toInterval(document))
    runAndHandle(state, Activate(intervals))
  }

  let deactivate = (state: State.t) => runAndHandle(state, Deactivate)
}

module PromptIM = {
  // we need to know what the <input> looks that
  // so that we can calculate what has been changed
  let previous = ref("")

  let handle = output => {
    open IM.Output
    let handle = kind =>
      switch kind {
      | UpdateView(sequence, translation, index) => list{
          ViewEvent(InputMethod(Update(sequence, translation, index))),
        }
      | Rewrite(rewrites, f) =>
        // TODO, postpone calling f
        f()

        // iterate through an array of `rewrites`
        let replaced = ref(previous.contents)
        let delta = ref(0)
        let replace = (((start, end_), t)) => {
          replaced :=
            replaced.contents->Js.String2.slice(~from=0, ~to_=delta.contents + start) ++
            t ++
            replaced.contents->Js.String2.sliceToEnd(~from=delta.contents + end_)
          delta := delta.contents + Js.String.length(t) - (end_ - start)
        }

        rewrites->Array.forEach(replace)

        // update stored <input>
        previous.contents = replaced.contents
        list{ViewEvent(PromptIMUpdate(replaced.contents))}
      | Activate => list{
          ViewEvent(InputMethod(Activate)),
          ViewEvent(PromptIMUpdate(previous.contents)),
        }
      | Deactivate => list{ViewEvent(InputMethod(Deactivate))}
      }
    output->Array.map(handle)->List.concatMany
  }

  let runAndHandle = (state: State.t, action) =>
    IM.run(state.promptIM, None, action)->handle->Promise.resolved

  // devise the "change" made to the input box
  let deviseChange = (previous, next): option<IM.Input.t> => {
    let inputLength = String.length(next)

    // helper funcion
    let init = s => Js.String.substring(~from=0, ~to_=String.length(s) - 1, s)
    let last = s => Js.String.substringToEnd(~from=String.length(s) - 1, s)

    if init(next) == previous {
      // Insertion
      Some(
        IM.Input.Change([
          {
            offset: inputLength - 1,
            insertedText: last(next),
            replacedTextLength: 0,
          },
        ]),
      )
    } else if next == init(previous) {
      // Backspacing
      Some(
        IM.Input.Change([
          {
            offset: inputLength,
            insertedText: "",
            replacedTextLength: 1,
          },
        ]),
      )
    } else {
      None
    }
  }

  let change = (state: State.t, next) => {
    let output = switch deviseChange(previous.contents, next) {
    | None => [IM.Output.Deactivate]
    | Some(input) => IM.run(state.promptIM, None, input)
    }

    // update stored <input>
    previous.contents = next

    output->handle->Promise.resolved
  }

  let insertChar = (state: State.t, char) => change(state, previous.contents ++ char)

  let activate = (state: State.t, input) => {
    // remove the ending backslash "\"
    let cursorOffset = String.length(input) - 1
    let input = Js.String.substring(~from=0, ~to_=cursorOffset, input)

    // update stored <input>
    previous.contents = input

    runAndHandle(state, Activate([(cursorOffset, cursorOffset)]))
  }

  let deactivate = (state: State.t) => runAndHandle(state, Deactivate)
}

type activated = Editor | Prompt | None

let isActivated = (state: State.t) =>
  if IM.isActivated(state.editorIM) {
    Editor
  } else if IM.isActivated(state.promptIM) {
    Prompt
  } else {
    None
  }

let deactivate = (state: State.t): Promise.t<list<Task.t>> =>
  switch isActivated(state) {
  | Editor => EditorIM.deactivate(state)
  | Prompt => PromptIM.deactivate(state)
  | None => Promise.resolved(list{})
  }

let activateEditorIM = (state: State.t): Promise.t<list<Task.t>> =>
  switch isActivated(state) {
  | Editor =>
    // already activated, insert backslash "\" instead
    Editor.Cursor.getMany(state.editor)->Array.forEach(point =>
      Editor.Text.insert(VSCode.TextEditor.document(state.editor), point, "\\")->ignore
    )
    // and then deactivate it
    EditorIM.deactivate(state)
  | Prompt =>
    // deactivate the prompt IM
    PromptIM.deactivate(state)->Promise.flatMap(tasks1 => {
      // activate the editor IM
      EditorIM.activate(state)->Promise.map(tasks2 => {
        List.concat(tasks1, tasks2)
      })
    })
  | None =>
    // activate the editor IM
    EditorIM.activate(state)
  }

// activate the prompt IM when the user typed a backslash "/"
let shouldActivatePromptIM = input => Js.String.endsWith("\\", input)

let activatePromptIM = (state: State.t, input) =>
  switch isActivated(state) {
  | Editor =>
    if shouldActivatePromptIM(input) {
      // deactivate the editor IM
      EditorIM.deactivate(state)->Promise.flatMap(tasks1 => {
        // activate the prompt IM
        PromptIM.activate(state, input)->Promise.map(tasks2 => List.concat(tasks1, tasks2))
      })
    } else {
      list{ViewEvent(PromptIMUpdate(input))}->Promise.resolved
    }
  | Prompt => PromptIM.change(state, input)
  | None =>
    if shouldActivatePromptIM(input) {
      PromptIM.activate(state, input)
    } else {
      list{ViewEvent(PromptIMUpdate(input))}->Promise.resolved
    }
  }

let select = (state: State.t, offset) => {
  switch isActivated(state) {
  | Editor => EditorIM.runAndHandle(state, Select(offset))
  | Prompt => PromptIM.runAndHandle(state, Select(offset))
  | None => Promise.resolved(list{})
  }
}

let chooseSymbol = (state: State.t, symbol): Promise.t<list<Task.t>> =>
  // deactivate after passing `Candidate(ChooseSymbol(symbol))` to the IM
  switch isActivated(state) {
  | Editor =>
    EditorIM.runAndHandle(state, Candidate(ChooseSymbol(symbol)))->Promise.flatMap(tasks1 =>
      deactivate(state)->Promise.map(tasks2 => List.concat(tasks1, tasks2))
    )
  | Prompt =>
    PromptIM.runAndHandle(state, Candidate(ChooseSymbol(symbol)))->Promise.flatMap(tasks1 =>
      deactivate(state)->Promise.map(tasks2 => List.concat(tasks1, tasks2))
    )
  | None => Promise.resolved(list{})
  }

let insertChar = (state: State.t, char) =>
  switch isActivated(state) {
  | Editor =>
    let char = Js.String.charAt(0, char)
    Editor.Cursor.getMany(state.editor)->Array.forEach(point =>
      Editor.Text.insert(VSCode.TextEditor.document(state.editor), point, char)->ignore
    )
    Promise.resolved(list{})
  | Prompt => PromptIM.insertChar(state, char)
  | None => Promise.resolved(list{})
  }

let moveUp = (state: State.t) =>
  switch isActivated(state) {
  | Editor => EditorIM.runAndHandle(state, Candidate(BrowseUp))
  | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseUp))
  | None => Promise.resolved(list{})
  }

let moveDown = (state: State.t) =>
  switch isActivated(state) {
  | Editor => EditorIM.runAndHandle(state, Candidate(BrowseDown))
  | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseDown))
  | None => Promise.resolved(list{})
  }

let moveLeft = (state: State.t) =>
  switch isActivated(state) {
  | Editor => EditorIM.runAndHandle(state, Candidate(BrowseLeft))
  | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseLeft))
  | None => Promise.resolved(list{})
  }

let moveRight = (state: State.t) =>
  switch isActivated(state) {
  | Editor => EditorIM.runAndHandle(state, Candidate(BrowseRight))
  | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseRight))
  | None => Promise.resolved(list{})
  }

// from Editor Command to Tasks
let handle = x =>
  switch x {
  | Command.InputMethod.Activate => list{WithStateP(state => activateEditorIM(state))}
  | InsertChar(char) => list{WithStateP(state => insertChar(state, char))}
  | BrowseUp => list{WithStateP(moveUp)}
  | BrowseDown => list{WithStateP(moveDown)}
  | BrowseLeft => list{WithStateP(moveLeft)}
  | BrowseRight => list{WithStateP(moveRight)}
  }
