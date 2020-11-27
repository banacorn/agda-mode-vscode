open Belt
open! BsMocha.Mocha
open Test__Util

module Js' = Js
open Promise
module Js = Js'

type setup = {
  editor: VSCode.TextEditor.t,
  chan: Chan.t<EditorIM.event>,
}

let activateExtension = (fileName): Promise.t<setup> => {
  let disposables = []
  let extensionPath = Path.extensionPath()
  let chan = Main.activateWithoutContext(disposables, extensionPath)
  VSCode.Window.showTextDocumentWithUri(VSCode.Uri.file(fileName), None)->map(editor => {
    editor: editor,
    chan: chan,
  })
}

let acquire = setup =>
  switch setup.contents {
  | None => resolved(Error(Util.Error("Cannot acquire the setup")))
  | Some(setup) => resolved(Ok(setup))
  }

let cleanup = setup => {
  let range = VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(100, 0))
  setup.editor->VSCode.TextEditor.document->Editor.Text.replace(range, "")
}

let insertChar = (setup, char) => {
  let promise = setup.chan->Chan.once

  let positions = Editor.Cursor.getMany(setup.editor)

  setup.editor
  ->VSCode.TextEditor.document
  ->Editor.Text.batchInsert(positions, char)
  ->flatMap(_ => promise)
  ->map(x => Ok(x))
}

let backspace = setup => {
  let promise = setup.chan->Chan.once
  let end_ = Editor.Cursor.get(setup.editor)
  let start = end_->VSCode.Position.translate(0, -1)
  let range = VSCode.Range.make(start, end_)
  setup.editor
  ->VSCode.TextEditor.document
  ->Editor.Text.delete(range)
  ->flatMap(_ => promise)
  ->map(x => Ok(x))
}

module IM = {
  let activate = (setup, ~positions=?, ()) => {
    let promise = setup.chan->Chan.once
    let positions = positions->Option.getWithDefault(Editor.Cursor.getMany(setup.editor))
    Editor.Cursor.setMany(setup.editor, positions)
    VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
    ->map(x => Ok(x))
    // document->Editor.insertTexts(points, "\\")->flatMap(_ => promise);
  }

  let deactivate = setup => {
    let promise = setup.chan->Chan.once
    VSCode.Commands.executeCommand0("agda-mode.escape")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
    ->map(x => Ok(x))
  }
}

describe("Input Method (Editor)", () => {
  let setup = ref(None)

  Q.before(() => activateExtension(Path.asset("InputMethod.agda"))->map(value => {
      setup := Some(value)
      Ok()
    }))

  Q.after_each(() => acquire(setup)->mapOk(cleanup))

  describe("Insertion", () => {
    Q.it(j`should translate "lambdabar" to "λ"`, () => acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)
        IM.activate(setup, ())
        ->flatMapOk(A.equal(EditorIM.Activate))
        ->flatMapOk(() => insertChar(setup, "l"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "a"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←a`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "m"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←am`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "b"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←amb`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "d"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←ambd`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "a"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`λ`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "b"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`λb`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "a"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`λba`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "r"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`ƛ`, Editor.Text.getAll(document)))
      }))
    Q.it(j`should translate "bn" to "𝕟"`, () => acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)
        IM.activate(setup, ())
        ->flatMapOk(A.equal(EditorIM.Activate))
        ->flatMapOk(() => insertChar(setup, "b"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`♭`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "n"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`𝕟`, Editor.Text.getAll(document)))
        // ->flatMapOk(() => IM.deactivate(setup))
        // ->flatMapOk(A.equal(EditorIM.Deactivate))
      }))
  })
  describe("Backspacing", () =>
    Q.it(j`should work just fine`, () => acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)
        IM.activate(setup, ())
        ->flatMapOk(A.equal(EditorIM.Activate))
        ->flatMapOk(() => insertChar(setup, "l"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "a"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←a`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "m"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←am`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "b"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←amb`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "d"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`←ambd`, Editor.Text.getAll(document)))
        ->flatMapOk(() => insertChar(setup, "a"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`λ`, Editor.Text.getAll(document)))
        ->flatMapOk(() => backspace(setup))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() => A.equal(j`lambd`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.deactivate(setup))
        ->flatMapOk(A.equal(EditorIM.Deactivate))
      }))
  )
  describe("Multiple cursors at once", () => {
    let positions = [
      VSCode.Position.make(0, 0),
      VSCode.Position.make(1, 0),
      VSCode.Position.make(2, 0),
      VSCode.Position.make(3, 0),
    ]
    Q.it(j`should work just fine`, () => {
      let replaceCRLF = Js.String.replaceByRe(%re("/\\r\\n/g"), "\n")

      acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)

        document
        ->Editor.Text.insert(VSCode.Position.make(0, 0), "\n\n\n")
        ->flatMap(_ => IM.activate(setup, ~positions, ()))
        ->flatMapOk(A.equal(EditorIM.Activate))
        ->flatMapOk(() => insertChar(setup, "b"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() =>
          A.equal(j`♭\\n♭\\n♭\\n♭`, replaceCRLF(Editor.Text.getAll(document)))
        )
        ->flatMapOk(() => insertChar(setup, "n"))
        ->flatMapOk(A.equal(EditorIM.Change))
        ->flatMapOk(() =>
          A.equal(j`𝕟\\n𝕟\\n𝕟\\n𝕟`, replaceCRLF(Editor.Text.getAll(document)))
        )
      })
    })
  })
})
