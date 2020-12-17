module type Module = {
  // shared by both EditorIM / PromptIM
  let deactivate: State.t => Promise.t<unit>

  let select: (State.t, array<IM.interval>) => Promise.t<unit>
  let insertChar: (State.t, string) => Promise.t<unit>
  let chooseSymbol: (State.t, string) => Promise.t<unit>
  let moveUp: State.t => Promise.t<unit>
  let moveDown: State.t => Promise.t<unit>
  let moveLeft: State.t => Promise.t<unit>
  let moveRight: State.t => Promise.t<unit>

  // EditorIM
  let activateEditorIM: State.t => Promise.t<unit>
  let keyUpdateEditorIM: (State.t, array<Buffer.change>) => Promise.t<unit>

  // PromptIM
  let keyUpdatePromptIM: (State.t, string) => Promise.t<unit>
}

module Module: Module = {
  open Belt

  open! Task

  module EditorIM = {
    let handle = (state: State.t, output): Promise.t<unit> => {
      open IM.Output
      let handle = kind =>
        switch kind {
        | UpdateView(sequence, translation, index) =>
          viewEvent(state, InputMethod(Update(sequence, translation, index)))
        | Rewrite(replacements, resolve) =>
          let document = state.editor->VSCode.TextEditor.document
          let replacements = replacements->Array.map(((interval, text)) => {
            let range = document->IM.fromInterval(interval)
            (range, text)
          })
          Editor.Text.batchReplace(document, replacements)->Promise.map(_ => {
            resolve()
            ()
          })
        | Activate => viewEvent(state, InputMethod(Activate))
        | Deactivate => viewEvent(state, InputMethod(Deactivate))
        }
      output->Array.map(handle)->Util.oneByOne->Promise.map(_ => ())
    }

    let runAndHandle = (state: State.t, action) =>
      handle(state, IM.run(state.editorIM, Some(state.editor), action))

    let keyUpdate = (state: State.t, changes) =>
      handle(state, IM.run(state.editorIM, Some(state.editor), KeyUpdate(changes)))

    let activate = (state: State.t) => {
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

    let handle = (state, output) => {
      open IM.Output
      let handle = kind =>
        switch kind {
        | UpdateView(sequence, translation, index) =>
          viewEvent(state, InputMethod(Update(sequence, translation, index)))
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
          {viewEvent(state, PromptIMUpdate(replaced.contents))}
        | Activate =>
          viewEvent(state, InputMethod(Activate))->Promise.flatMap(() =>
            viewEvent(state, PromptIMUpdate(previous.contents))
          )
        | Deactivate => viewEvent(state, InputMethod(Deactivate))
        }
      output->Array.map(handle)->Util.oneByOne->Promise.map(_ => ())
    }

    let runAndHandle = (state: State.t, action): Promise.t<unit> =>
      handle(state, IM.run(state.promptIM, None, action))

    let keyUpdate = (state: State.t, next) => {
      // devise the "change" made to the input box
      let deviseChange = (previous, next): IM.Input.t => {
        let inputLength = String.length(next)

        // helper funcion
        let init = s => Js.String.substring(~from=0, ~to_=String.length(s) - 1, s)
        let last = s => Js.String.substringToEnd(~from=String.length(s) - 1, s)

        if init(next) == previous {
          // Insertion
          IM.Input.KeyUpdate([
            {
              offset: inputLength - 1,
              insertedText: last(next),
              replacedTextLength: 0,
            },
          ])
        } else if next == init(previous) {
          // Backspacing
          IM.Input.KeyUpdate([
            {
              offset: inputLength,
              insertedText: "",
              replacedTextLength: 1,
            },
          ])
        } else {
          Deactivate
        }
      }

      let input = deviseChange(previous.contents, next)
      let output = IM.run(state.promptIM, None, input)

      // update stored <input>
      previous.contents = next

      handle(state, output)
    }

    let insertChar = (state: State.t, char) => keyUpdate(state, previous.contents ++ char)

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

  let deactivate = (state: State.t): Promise.t<unit> =>
    switch isActivated(state) {
    | Editor => EditorIM.deactivate(state)
    | Prompt => PromptIM.deactivate(state)
    | None => Promise.resolved()
    }

  let activateEditorIM = (state: State.t): Promise.t<unit> =>
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
      PromptIM.deactivate(state)->Promise.flatMap(() => {
        // activate the editor IM
        EditorIM.activate(state)
      })
    | None =>
      // activate the editor IM
      EditorIM.activate(state)
    }

  // activate the prompt IM when the user typed a backslash "/"
  let shouldActivatePromptIM = input => Js.String.endsWith("\\", input)

  let keyUpdatePromptIM = (state: State.t, input) =>
    switch isActivated(state) {
    | Editor =>
      if shouldActivatePromptIM(input) {
        // deactivate the editor IM
        EditorIM.deactivate(state)->Promise.flatMap(() =>
          // activate the prompt IM
          PromptIM.activate(state, input)
        )
      } else {
        {viewEvent(state, PromptIMUpdate(input))}
      }
    | Prompt => PromptIM.keyUpdate(state, input)
    | None =>
      if shouldActivatePromptIM(input) {
        PromptIM.activate(state, input)
      } else {
        {viewEvent(state, PromptIMUpdate(input))}
      }
    }

  let keyUpdateEditorIM = (state: State.t, changes) =>
    switch isActivated(state) {
    | Editor => EditorIM.keyUpdate(state, changes)
    | Prompt => Promise.resolved()
    | None => Promise.resolved()
    }

  let select = (state: State.t, intervals) => {
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, MouseSelect(intervals))
    | Prompt => PromptIM.runAndHandle(state, MouseSelect(intervals))
    | None => Promise.resolved()
    }
  }

  let chooseSymbol = (state: State.t, symbol): Promise.t<unit> =>
    // deactivate after passing `Candidate(ChooseSymbol(symbol))` to the IM
    switch isActivated(state) {
    | Editor =>
      EditorIM.runAndHandle(state, Candidate(ChooseSymbol(symbol)))->Promise.flatMap(() =>
        deactivate(state)
      )
    | Prompt =>
      PromptIM.runAndHandle(state, Candidate(ChooseSymbol(symbol)))->Promise.flatMap(() =>
        deactivate(state)
      )
    | None => Promise.resolved()
    }

  let insertChar = (state: State.t, char) =>
    switch isActivated(state) {
    | Editor =>
      let char = Js.String.charAt(0, char)
      let positions = Editor.Cursor.getMany(state.editor)
      let document = state.editor->VSCode.TextEditor.document

      document->Editor.Text.batchInsert(positions, char)->Promise.map(_ => {
        Editor.focus(document)
      })
    | Prompt => PromptIM.insertChar(state, char)
    | None => Promise.resolved()
    }

  let moveUp = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseUp))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseUp))
    | None => Promise.resolved()
    }

  let moveDown = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseDown))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseDown))
    | None => Promise.resolved()
    }

  let moveLeft = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseLeft))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseLeft))
    | None => Promise.resolved()
    }

  let moveRight = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseRight))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseRight))
    | None => Promise.resolved()
    }
}

include Module
