open Belt
open! BsMocha.Mocha
open Test__Util

module Js' = Js
open Promise
module Js = Js'

type setup = {
  editor: VSCode.TextEditor.t,
  chan: Chan.t<IM.Log.t>,
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

module IM = {
  include IM

  let equal = (xs: IM.Log.t) => A.equal(xs)
  let deep_equal = (xs: IM.Log.t) => A.deep_equal(xs)

  let wait = setup => setup.chan->Chan.once->Promise.map(x => Ok(x))
  let wait2nd = setup =>
    setup.chan->Chan.once->Promise.flatMap(_ => setup.chan->Chan.once)->Promise.map(x => Ok(x))

  let activate = (setup, ~positions=?, ()) => {
    let promise = wait(setup)
    let positions = positions->Option.getWithDefault(Editor.Cursor.getMany(setup.editor))
    Editor.Cursor.setMany(setup.editor, positions)
    VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
  }

  let deactivate = setup => {
    let promise = wait(setup)
    VSCode.Commands.executeCommand0("agda-mode.escape")
    ->flatMap(result => result)
    ->flatMap(_ => promise)
  }

  let insertChar = (setup, char) => {
    let promise1 = wait(setup)
    let promise2 = wait2nd(setup)

    let positions = Editor.Cursor.getMany(setup.editor)

    setup.editor
    ->VSCode.TextEditor.document
    ->Editor.Text.batchInsert(positions, char)
    ->map(succeed => succeed ? Ok() : Error(Js.Exn.raiseError("Failed to insert " ++ char)))
    ->flatMapOk(() => promise1)
    ->flatMapOk(result1 => promise2->Promise.mapOk(result2 => Array.concat(result1, result2)))
  }

  let backspace = setup => {
    let promise1 = wait(setup)
    let promise2 = wait2nd(setup)
    let end_ = Editor.Cursor.get(setup.editor)
    let start = end_->VSCode.Position.translate(0, -1)
    let range = VSCode.Range.make(start, end_)
    setup.editor
    ->VSCode.TextEditor.document
    ->Editor.Text.delete(range)
    ->map(succeed => succeed ? Ok() : Error(Js.Exn.raiseError("Failed to backspace")))
    ->flatMapOk(() => promise1)
    ->flatMapOk(result1 => promise2->Promise.mapOk(result2 => Array.concat(result1, result2)))
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
        ->flatMapOk(IM.deep_equal([Activate]))
        ->flatMapOk(() => IM.insertChar(setup, "l"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 1), j`←`)]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "a"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←a`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "m"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←am`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "b"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←amb`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "d"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←ambd`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "a"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 6), j`λ`)]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`λ`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "b"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`λb`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "a"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`λba`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "r"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 4), j`ƛ`)]), Deactivate, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`ƛ`, Editor.Text.getAll(document)))
      }))
    Q.it(j`should translate "bn" to "𝕟"`, () => acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)
        IM.activate(setup, ())
        ->flatMapOk(IM.deep_equal([Activate]))
        ->flatMapOk(() => IM.insertChar(setup, "b"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 1), j`♭`)]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`♭`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "n"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 2), j`𝕟`)]), Deactivate, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`𝕟`, Editor.Text.getAll(document)))
      }))
    Q.it(j`should abort nicely`, () => acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)
        IM.activate(setup, ())
        ->flatMapOk(IM.deep_equal([Activate]))
        ->flatMapOk(() => IM.insertChar(setup, "b"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 1), j`♭`)]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`♭`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.deactivate(setup))
        ->flatMapOk(IM.deep_equal([Deactivate]))
        ->flatMapOk(() => A.equal(j`♭`, Editor.Text.getAll(document)))
      }))
  })
  describe("Backspacing", () =>
    Q.it(j`should work just fine`, () => acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)
        IM.activate(setup, ())
        ->flatMapOk(IM.deep_equal([Activate]))
        ->flatMapOk(() => IM.insertChar(setup, "l"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 1), j`←`)]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "a"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←a`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.backspace(setup))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 1), j`←`)]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "a"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←a`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "m"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←am`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "b"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←amb`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "d"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`←ambd`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.insertChar(setup, "a"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([((0, 6), j`λ`)]), UpdateView, RewriteApplied]))
        ->flatMapOk(() => A.equal(j`λ`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.backspace(setup))
        ->flatMapOk(
          IM.deep_equal([RewriteIssued([((0, 0), j`lambd`)]), UpdateView, RewriteApplied]),
        )
        ->flatMapOk(() => A.equal(j`lambd`, Editor.Text.getAll(document)))
        ->flatMapOk(() => IM.deactivate(setup))
        ->flatMapOk(IM.deep_equal([Deactivate]))
        ->flatMapOk(() => A.equal(j`lambd`, Editor.Text.getAll(document)))
      }))
  )

  describe("Multiple cursors at once", () => {
    let replaceCRLF = Js.String.replaceByRe(%re("/\\r\\n/g"), "\n")
    Q.it(j`should work just fine (𝕟)`, () => {
      let positions = [
        VSCode.Position.make(0, 0),
        VSCode.Position.make(1, 0),
        VSCode.Position.make(2, 0),
        VSCode.Position.make(3, 0),
      ]

      acquire(setup)->flatMapOk(setup => {
        let document = VSCode.TextEditor.document(setup.editor)

        document
        ->Editor.Text.insert(VSCode.Position.make(0, 0), "\n\n\n")
        ->flatMap(_ => IM.activate(setup, ~positions, ()))
        ->flatMapOk(IM.deep_equal([Activate]))
        ->flatMapOk(() => IM.insertChar(setup, "b"))
        ->flatMapOk(
          IM.deep_equal([
            RewriteIssued([((0, 1), j`♭`), ((2, 3), j`♭`), ((4, 5), j`♭`), ((6, 7), j`♭`)]),
            UpdateView,
            RewriteApplied,
          ]),
        )
        ->flatMapOk(() =>
          A.equal(j`♭\\n♭\\n♭\\n♭`, replaceCRLF(Editor.Text.getAll(document)))
        )
        ->flatMapOk(() => IM.insertChar(setup, "n"))
        ->flatMapOk(
          IM.deep_equal([
            RewriteIssued([
              ((0, 2), j`𝕟`),
              ((3, 5), j`𝕟`),
              ((6, 8), j`𝕟`),
              ((9, 11), j`𝕟`),
            ]),
            Deactivate,
            RewriteApplied,
          ]),
        )
        ->flatMapOk(() =>
          A.equal(j`𝕟\\n𝕟\\n𝕟\\n𝕟`, replaceCRLF(Editor.Text.getAll(document)))
        )
      })
    })
    Q.it(j`should work just fine (∧)`, () => {
      acquire(setup)->flatMapOk(setup => {
        let positions = [
          VSCode.Position.make(0, 0),
          VSCode.Position.make(1, 1),
          VSCode.Position.make(2, 2),
          VSCode.Position.make(3, 3),
        ]
        let document = VSCode.TextEditor.document(setup.editor)

        document
        ->Editor.Text.insert(VSCode.Position.make(0, 0), "123\n123\n123\n123")
        ->flatMap(_ => IM.activate(setup, ~positions, ()))
        ->flatMapOk(IM.deep_equal([Activate]))
        ->flatMapOk(() => IM.insertChar(setup, "a"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() =>
          A.equal(j`a123\\n1a23\\n12a3\\n123a`, replaceCRLF(Editor.Text.getAll(document)))
        )
        ->flatMapOk(() => IM.insertChar(setup, "n"))
        ->flatMapOk(IM.deep_equal([RewriteIssued([]), UpdateView, RewriteApplied]))
        ->flatMapOk(() =>
          A.equal(j`an123\\n1an23\\n12an3\\n123an`, replaceCRLF(Editor.Text.getAll(document)))
        )
        ->flatMapOk(() => IM.insertChar(setup, "d"))
        ->flatMapOk(
          IM.deep_equal([
            RewriteIssued([
              ((0, 3), j`∧`),
              ((8, 11), j`∧`),
              ((16, 19), j`∧`),
              ((24, 27), j`∧`),
            ]),
            UpdateView,
            RewriteApplied,
          ]),
        )
        ->flatMapOk(() =>
          A.equal(j`∧123\\n1∧23\\n12∧3\\n123∧`, replaceCRLF(Editor.Text.getAll(document)))
        )
      })
    })
  })
})
