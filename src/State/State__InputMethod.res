open Common
module type Module = {
  // shared by both EditorIM / PromptIM
  let deactivate: State.t => promise<unit>

  let select: (State.t, array<Interval.t>) => promise<unit>
  let insertChar: (State.t, string) => promise<unit>
  let chooseSymbol: (State.t, string) => promise<unit>
  let moveUp: State.t => promise<unit>
  let moveDown: State.t => promise<unit>
  let moveLeft: State.t => promise<unit>
  let moveRight: State.t => promise<unit>

  // EditorIM
  let activateEditorIM: State.t => promise<unit>
  let keyUpdateEditorIM: (State.t, array<Buffer.change>) => promise<unit>

  // PromptIM
  let keyUpdatePromptIM: (State.t, string) => promise<unit>
}

module Module: Module = {
  module EditorIM = {
    let handle = async (state: State.t, output): unit => {
      open IM.Output
      let handle = async kind =>
        switch kind {
        | UpdateView(sequence, translation, index) =>
          await State.View.Panel.updateIM(state, Update(sequence, translation, index))
        | Rewrite(replacements, resolve) =>
          let _ = await Editor.Text.batchReplace(state.document, replacements)
          resolve()
        | Activate =>
          State.Context.setIM(true)
          await State.View.Panel.updateIM(state, Activate)
        | Deactivate =>
          State.Context.setIM(false)
          await State.View.Panel.updateIM(state, Deactivate)
        }
      let _ = await output->Array.map(handle)->Util.oneByOne
    }

    let runAndHandle = (state: State.t, action) =>
      handle(state, IM.run(state.editorIM, Some(state.editor), action))

    let keyUpdate = (state: State.t, changes) =>
      handle(state, IM.run(state.editorIM, Some(state.editor), KeyUpdate(changes)))

    let activate = (state: State.t) => {
      // activated the input method with cursors positions
      let intervals: array<Interval.t> =
        Editor.Selection.getMany(state.editor)->Array.map(
          Interval.fromVSCodeRange(state.document, ...)
        )
      runAndHandle(state, Activate(intervals))
    }

    let deactivate = (state: State.t) => runAndHandle(state, Deactivate)
  }

  module PromptIM = {
    // we need to know what the <input> looks that
    // so that we can calculate what has been changed
    let previous = ref("")

    let handle = async (state, output) => {
      open IM.Output
      let handle = async kind =>
        switch kind {
        | UpdateView(sequence, translation, index) =>
          await State.View.Panel.updateIM(state, Update(sequence, translation, index))
        | Rewrite(rewrites, f) =>
          // TODO, postpone calling f
          f()

          // iterate through an array of `rewrites`
          let replaced = ref(previous.contents)
          let delta = ref(0)
          let replace = ((range, t)) => {
            let (start, end_) = Common.Interval.fromVSCodeRange(state.document, range)
            replaced :=
              replaced.contents->String.slice(~start=0, ~end=delta.contents + start) ++
              t ++
              replaced.contents->String.sliceToEnd(~start=delta.contents + end_)
            delta := delta.contents + String.length(t) - (end_ - start)
          }

          rewrites->Array.forEach(replace)

          // update the stored content and notify the view
          previous.contents = replaced.contents
          await State.View.Panel.updatePromptIM(state, replaced.contents)
        | Activate =>
          await State.View.Panel.updateIM(state, Activate)
          await State.View.Panel.updatePromptIM(state, previous.contents)
        | Deactivate => await State.View.Panel.updateIM(state, Deactivate)
        }
      let _ = await output->Array.map(handle)->Util.oneByOne
    }

    let runAndHandle = (state: State.t, action): promise<unit> =>
      handle(state, IM.run(state.promptIM, None, action))

    let keyUpdate = (state: State.t, next) => {
      // devise the "change" made to the input box
      let deviseChange = (previous, next): IM.Input.t => {
        let inputLength = String.length(next)

        // helper funcion
        let init = s => String.substring(~start=0, ~end=String.length(s) - 1, s)
        let last = s => String.substringToEnd(~start=String.length(s) - 1, s)

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
      let input = String.substring(~start=0, ~end=cursorOffset, input)

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

  let deactivate = (state: State.t): promise<unit> =>
    switch isActivated(state) {
    | Editor => EditorIM.deactivate(state)
    | Prompt => PromptIM.deactivate(state)
    | None => Promise.resolve()
    }

  let activationKey = Config.InputMethod.getActivationKey()

  let activateEditorIM = async (state: State.t): unit =>
    switch isActivated(state) {
    | Editor =>
      // Input method is already activated.
      // If buffer is empty, insert the activation key (default: "\")
      if state.editorIM->IM.bufferIsEmpty {
        Editor.Cursor.getMany(state.editor)->Array.forEach(point =>
          Editor.Text.insert(state.document, point, activationKey)->ignore
        )
      }
      // else reactivate it
      await EditorIM.deactivate(state)
      await EditorIM.activate(state)
    | Prompt =>
      // deactivate the prompt IM
      await PromptIM.deactivate(state)
      // activate the editor IM
      await EditorIM.activate(state)
    | None =>
      // activate the editor IM
      await EditorIM.activate(state)
    }

  // activate the prompt IM when the user typed the activation key (default: "\")
  let shouldActivatePromptIM = input => String.endsWith(activationKey, input)

  let keyUpdatePromptIM = async (state: State.t, input) =>
    switch isActivated(state) {
    | Editor =>
      if shouldActivatePromptIM(input) {
        // deactivate the editor IM
        await EditorIM.deactivate(state)
        // activate the prompt IM
        await PromptIM.activate(state, input)
      } else {
        await State.View.Panel.updatePromptIM(state, input)
      }
    | Prompt => await PromptIM.keyUpdate(state, input)
    | None =>
      if shouldActivatePromptIM(input) {
        await PromptIM.activate(state, input)
      } else {
        await State.View.Panel.updatePromptIM(state, input)
      }
    }

  let keyUpdateEditorIM = (state: State.t, changes) =>
    switch isActivated(state) {
    | Editor => EditorIM.keyUpdate(state, changes)
    | Prompt => Promise.resolve()
    | None => Promise.resolve()
    }

  let select = (state: State.t, intervals) => {
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, MouseSelect(intervals))
    | Prompt => PromptIM.runAndHandle(state, MouseSelect(intervals))
    | None => Promise.resolve()
    }
  }

  let chooseSymbol = async (state: State.t, symbol): unit =>
    // deactivate after passing `Candidate(ChooseSymbol(symbol))` to the IM
    switch isActivated(state) {
    | Editor =>
      await EditorIM.runAndHandle(state, Candidate(ChooseSymbol(symbol)))
      await deactivate(state)
    | Prompt =>
      await PromptIM.runAndHandle(state, Candidate(ChooseSymbol(symbol)))
      await deactivate(state)
    | None => ()
    }

  let insertChar = async (state: State.t, char) =>
    switch isActivated(state) {
    | Editor =>
      let char = String.charAt(char, 0)
      let positions = Editor.Cursor.getMany(state.editor)

      let _ = await state.document->Editor.Text.batchInsert(positions, char)
      Editor.focus(state.document)
    | Prompt => await PromptIM.insertChar(state, char)
    | None => ()
    }

  let moveUp = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseUp))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseUp))
    | None => Promise.resolve()
    }

  let moveDown = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseDown))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseDown))
    | None => Promise.resolve()
    }

  let moveLeft = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseLeft))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseLeft))
    | None => Promise.resolve()
    }

  let moveRight = (state: State.t) =>
    switch isActivated(state) {
    | Editor => EditorIM.runAndHandle(state, Candidate(BrowseRight))
    | Prompt => PromptIM.runAndHandle(state, Candidate(BrowseRight))
    | None => Promise.resolve()
    }
}

include Module
