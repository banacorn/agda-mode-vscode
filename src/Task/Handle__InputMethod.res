open Belt

open! Task

module EditorIM = {
  let activate = (state: State.t) => {
    // activated the input method with cursors positions
    let document = VSCode.TextEditor.document(state.editor)
    let intervals: array<(int, int)> =
      Editor.Selection.getMany(state.editor)->Array.map(range => (
        document->VSCode.TextDocument.offsetAt(VSCode.Range.start(range)),
        document->VSCode.TextDocument.offsetAt(VSCode.Range.end_(range)),
      ))
    IM.activate(state.editorIM, Some(state.editor), intervals)
  }

  let deactivate = (state: State.t) => IM.deactivate(state.editorIM)

  let handle = (state: State.t, output) => {
    open IM.Output
    let handle = kind =>
      switch kind {
      | UpdateView(sequence, translation, index) =>
        Promise.resolved(list{ViewEvent(InputMethod(Update(sequence, translation, index)))})
      | Rewrite(replacements, resolve) =>
        let document = state.editor->VSCode.TextEditor.document
        let replacements = replacements->Array.map(((interval, text)) => {
          let range = VSCode.Range.make(
            document->VSCode.TextDocument.positionAt(fst(interval)),
            document->VSCode.TextDocument.positionAt(snd(interval)),
          )
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
}

module PromptIM = {
  let previous = ref("")
  let current = ref("")
  let activate = (state: State.t, input) => {
    // remove the ending backslash "\"
    let cursorOffset = String.length(input) - 1
    let input = Js.String.substring(~from=0, ~to_=cursorOffset, input)

    // save the input
    previous.contents = input
    IM.activate(state.promptIM, None, [(cursorOffset, cursorOffset)])
  }

  let deactivate = (state: State.t) => state.promptIM->IM.deactivate

  let change = (state: State.t, input) => {
    current.contents = input
    switch IM.deviseChange(state.promptIM, previous.contents, input) {
    | None => Promise.resolved([IM.Output.Deactivate])
    | Some(input) => IM.run(state.promptIM, None, input)
    }
  }

  let insertChar = (state: State.t, char) => change(state, previous.contents ++ char)

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
        let replaced = ref(current.contents)
        let delta = ref(0)
        let replace = (((start, end_), t)) => {
          replaced :=
            replaced.contents->Js.String2.slice(~from=0, ~to_=delta.contents + start) ++
            t ++
            replaced.contents->Js.String2.sliceToEnd(~from=delta.contents + end_)
          delta := delta.contents + Js.String.length(t) - (end_ - start)
        }

        rewrites->Array.forEach(replace)

        list{ViewEvent(PromptIMUpdate(replaced.contents))}
      | Activate => list{
          ViewEvent(InputMethod(Activate)),
          ViewEvent(PromptIMUpdate(previous.contents)),
        }
      | Deactivate => list{ViewEvent(InputMethod(Deactivate))}
      }
    output->Array.map(handle)->List.concatMany
  }
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
  | Editor => EditorIM.handle(state, EditorIM.deactivate(state))
  | Prompt => PromptIM.handle(PromptIM.deactivate(state))->Promise.resolved
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
    EditorIM.handle(state, EditorIM.deactivate(state))
  | Prompt =>
    // deactivate the prompt IM
    let tasks1 = PromptIM.handle(PromptIM.deactivate(state))
    // activate the editor IM
    EditorIM.handle(state, EditorIM.activate(state))->Promise.map(tasks2 => {
      List.concat(tasks1, tasks2)
    })
  | None =>
    // activate the editor IM
    EditorIM.handle(state, EditorIM.activate(state))
  }

// activate the prompt IM when the user typed a backslash "/"
let shouldActivatePromptIM = input => Js.String.endsWith("\\", input)

let activatePromptIM = (state: State.t, input) =>
  switch isActivated(state) {
  | Editor =>
    if shouldActivatePromptIM(input) {
      // deactivate the editor IM
      EditorIM.handle(state, EditorIM.deactivate(state))->Promise.map(tasks1 => {
        // activate the prompt IM
        let tasks2 = PromptIM.handle(PromptIM.activate(state, input))
        List.concat(tasks1, tasks2)
      })
    } else {
      list{ViewEvent(PromptIMUpdate(input))}->Promise.resolved
    }
  | Prompt => PromptIM.change(state, input)->Promise.map(PromptIM.handle)
  | None =>
    if shouldActivatePromptIM(input) {
      PromptIM.handle(PromptIM.activate(state, input))->Promise.resolved
    } else {
      list{ViewEvent(PromptIMUpdate(input))}->Promise.resolved
    }
  }

let select = (state: State.t, offset) => {
  switch isActivated(state) {
  | Editor =>
    IM.run(state.editorIM, Some(state.editor), Select(offset))->Promise.flatMap(
      EditorIM.handle(state),
    )
  | Prompt => IM.run(state.promptIM, None, Select(offset))->Promise.flatMap(EditorIM.handle(state))
  | None => Promise.resolved(list{})
  }
}

let chooseSymbol = (state: State.t, symbol): Promise.t<list<Task.t>> =>
  // deactivate after passing `Candidate(ChooseSymbol(symbol))` to the IM
  switch isActivated(state) {
  | Editor =>
    IM.run(state.editorIM, Some(state.editor), Candidate(ChooseSymbol(symbol)))
    ->Promise.flatMap(EditorIM.handle(state))
    ->Promise.flatMap(tasks1 =>
      deactivate(state)->Promise.map(tasks2 => List.concat(tasks1, tasks2))
    )
  | Prompt =>
    IM.run(state.promptIM, None, Candidate(ChooseSymbol(symbol)))
    ->Promise.map(PromptIM.handle)
    ->Promise.flatMap(tasks1 =>
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
  | Prompt => PromptIM.insertChar(state, char)->Promise.map(PromptIM.handle)
  | None => Promise.resolved(list{})
  }

let moveUp = (state: State.t) =>
  switch isActivated(state) {
  | Editor =>
    IM.run(state.editorIM, Some(state.editor), Candidate(BrowseUp))->Promise.flatMap(
      EditorIM.handle(state),
    )
  | Prompt => IM.run(state.promptIM, None, Candidate(BrowseUp))->Promise.map(PromptIM.handle)
  | None => Promise.resolved(list{})
  }

let moveDown = (state: State.t) =>
  switch isActivated(state) {
  | Editor =>
    IM.run(state.editorIM, Some(state.editor), Candidate(BrowseDown))->Promise.flatMap(
      EditorIM.handle(state),
    )
  | Prompt => IM.run(state.promptIM, None, Candidate(BrowseDown))->Promise.map(PromptIM.handle)
  | None => Promise.resolved(list{})
  }

let moveLeft = (state: State.t) =>
  switch isActivated(state) {
  | Editor =>
    IM.run(state.editorIM, Some(state.editor), Candidate(BrowseLeft))->Promise.flatMap(
      EditorIM.handle(state),
    )
  | Prompt => IM.run(state.promptIM, None, Candidate(BrowseLeft))->Promise.map(PromptIM.handle)
  | None => Promise.resolved(list{})
  }

let moveRight = (state: State.t) =>
  switch isActivated(state) {
  | Editor =>
    IM.run(state.editorIM, Some(state.editor), Candidate(BrowseRight))->Promise.flatMap(
      EditorIM.handle(state),
    )
  | Prompt => IM.run(state.promptIM, None, Candidate(BrowseRight))->Promise.map(PromptIM.handle)
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
