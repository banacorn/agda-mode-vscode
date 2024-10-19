open Mocha
open Test__Util

module Js' = Js
open Promise
module Js = Js'

// type setup = {
//   editor: VSCode.TextEditor.t,
//   channels: State__Type.channels,
// }

// let acquire = async setup =>
//   switch setup.contents {
//   | None => Error(Util.Error("Cannot acquire the setup"))
//   | Some(setup) => Ok(setup)
//   }

// let cleanup = setup => {
//   let range = VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(100, 0))
//   setup.editor->VSCode.TextEditor.document->Editor.Text.replace(range, "")
// }

// module IM = {
//   include IM

//   let equal = (xs: IM.Log.t) => A.equal(xs)
//   let deepEqual = (xs: IM.Log.t) => A.deepEqual(xs)

//   let wait = setup => setup.channels.inputMethod->Chan.once->Promise.map(x => Ok(x))
//   let wait2nd = setup =>
//     setup.channels.inputMethod
//     ->Chan.once
//     ->Promise.flatMap(_ => setup.channels.inputMethod->Chan.once)
//     ->Promise.map(x => Ok(x))

//   let activate = (setup, ~positions=?, ()) => {
//     let promise = wait(setup)
//     let positions = positions->Option.getWithDefault(Editor.Cursor.getMany(setup.editor))
//     Editor.Cursor.setMany(setup.editor, positions)
//     VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
//     ->flatMap(result => result)
//     ->flatMap(_ => promise)
//   }

//   let deactivate = setup => {
//     let promise = wait(setup)
//     VSCode.Commands.executeCommand0("agda-mode.escape")
//     ->flatMap(result => result)
//     ->flatMap(_ => promise)
//   }

//   let insertChar = (setup, char) => {
//     let promise1 = wait(setup)
//     let promise2 = wait2nd(setup)

//     let positions = Editor.Cursor.getMany(setup.editor)

//     setup.editor
//     ->VSCode.TextEditor.document
//     ->Editor.Text.batchInsert(positions, char)
//     ->map(succeed => succeed ? Ok() : Error(Js.Exn.raiseError("Failed to insert " ++ char)))
//     ->flatMapOk(() => promise1)
//     ->flatMapOk(result1 => promise2->Promise.mapOk(result2 => Array.concat(result1, result2)))
//   }

//   let backspace = setup => {
//     let promise1 = wait(setup)
//     let promise2 = wait2nd(setup)
//     let end_ = Editor.Cursor.get(setup.editor)
//     let start = end_->VSCode.Position.translate(0, -1)
//     let range = VSCode.Range.make(start, end_)
//     setup.editor
//     ->VSCode.TextEditor.document
//     ->Editor.Text.delete(range)
//     ->map(succeed => succeed ? Ok() : Error(Js.Exn.raiseError("Failed to backspace")))
//     ->flatMapOk(() => promise1)
//     ->flatMapOk(result1 => promise2->Promise.mapOk(result2 => Array.concat(result1, result2)))
//   }

//   let select = (setup, intervals) => {
//     let ranges =
//       intervals->Array.map(Editor.Range.fromInterval(setup.editor->VSCode.TextEditor.document))
//     Editor.Selection.setMany(setup.editor, ranges)
//     Promise.resolved(Ok())
//   }
//   let selectAndWait = (setup, intervals) => {
//     let promise = wait(setup)
//     let ranges =
//       intervals->Array.map(Editor.Range.fromInterval(setup.editor->VSCode.TextEditor.document))
//     Editor.Selection.setMany(setup.editor, ranges)
//     promise
//   }
// }

// describe("Input Method (Editor)", () => {
//   let setup = ref(None)

//   Q.before(() => {
//     activateExtensionAndOpenFile(Path.asset("InputMethod.agda"))->map(
//       ((editor, channels)) => {
//         setup := Some({editor, channels})
//         Ok()
//       },
//     )
//   })

//   Q.after_each(() => acquire(setup)->mapOk(cleanup))

//   describe("Insertion", () => {
//     Q.it(
//       `should translate "lambdabar" to "λ"`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(_ => IM.insertChar(setup, "l"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 1), `←`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`←`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "m"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←am`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "b"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←amb`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "d"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←ambd`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 6), `λ`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`λ`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "b"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`λb`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`λba`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "r"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 4), `ƛ`)]), Deactivate, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`ƛ`, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//     Q.it(
//       `should translate "bn" to "𝕟"`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "b"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 1), `♭`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`♭`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "n"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 2), `𝕟`)]), Deactivate, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`𝕟`, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//     Q.it(
//       `Issue #55, should not deactivate when size of candidate symbols > 1`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "s"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`as`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "t"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 3), `∗`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`∗`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "e"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`∗e`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "r"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`∗er`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "i"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`∗eri`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "s"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`∗eris`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "k"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 6), `⁎`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`⁎`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.deactivate(setup))
//             ->flatMapOk(IM.deepEqual([Deactivate]))
//             ->flatMapOk(() => A.equal(`⁎`, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//   })
//   describe("Backspacing", () =>
//     Q.it(
//       `should work just fine`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "l"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 1), `←`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`←`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.backspace(setup))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 1), `←`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`←`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "m"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←am`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "b"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←amb`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "d"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`←ambd`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 6), `λ`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`λ`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.backspace(setup))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 0), `lambd`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`lambd`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.deactivate(setup))
//             ->flatMapOk(IM.deepEqual([Deactivate]))
//             ->flatMapOk(() => A.equal(`lambd`, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//   )

//   describe("Abortion", () => {
//     Q.it(
//       `should abort after hitting escape`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "b"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 1), `♭`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`♭`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.deactivate(setup))
//             ->flatMapOk(IM.deepEqual([Deactivate]))
//             ->flatMapOk(() => A.equal(`♭`, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//     Q.it(
//       `should abort after typing the wrong sequence`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "d"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), Deactivate, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`ad`, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//     Q.it(
//       `should abort after backspacing to much`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.backspace(setup))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([((0, 0), ``)]), Deactivate, RewriteApplied]))
//             ->flatMapOk(() => A.equal(``, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//   })

//   describe("Cursor", () => {
//     Q.it(
//       `should not abort when the cursor is placed inside the buffer`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)
//             IM.activate(setup, ())
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "n"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`an`, Editor.Text.getAll(document)))
//             // messing with the cursor
//             ->flatMapOk(() => IM.select(setup, [(0, 0)]))
//             ->flatMapOk(() => IM.select(setup, [(1, 1)]))
//             ->flatMapOk(() => IM.select(setup, [(2, 2)]))
//             ->flatMapOk(() => IM.select(setup, [(0, 1), (1, 2)]))
//             ->flatMapOk(() => IM.select(setup, [(0, 2)]))
//             // resume insertion
//             ->flatMapOk(() => IM.insertChar(setup, "d"))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 3), `∧`)]), UpdateView, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`∧`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "="))
//             ->flatMapOk(
//               IM.deepEqual([RewriteIssued([((0, 2), `≙`)]), Deactivate, RewriteApplied]),
//             )
//             ->flatMapOk(() => A.equal(`≙`, Editor.Text.getAll(document)))
//           },
//         ),
//     )
//     Q.it(
//       `should abort when the cursor is placed outside the buffer`,
//       () =>
//         acquire(setup)->flatMapOk(
//           setup => {
//             let positions = [VSCode.Position.make(0, 3)]

//             let document = VSCode.TextEditor.document(setup.editor)

//             document
//             ->Editor.Text.insert(VSCode.Position.make(0, 0), "123")
//             ->flatMap(_ => IM.activate(setup, ~positions, ()))
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`123a`, Editor.Text.getAll(document)))
//             ->flatMapOk(() => IM.insertChar(setup, "n"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(() => A.equal(`123an`, Editor.Text.getAll(document)))
//             // messing with the cursor
//             ->flatMapOk(() => IM.selectAndWait(setup, [(1, 1)]))
//             ->flatMapOk(IM.deepEqual([Deactivate]))
//           },
//         ),
//     )
//   })

//   describe("Multiple cursors at once", () => {
//     let replaceCRLF = Js.String.replaceByRe(%re("/\r\n/g"), "\n") // RegEx updated to v10.1.4
//     Q.it(
//       `should work just fine (𝕟)`,
//       () => {
//         let positions = [
//           VSCode.Position.make(0, 0),
//           VSCode.Position.make(1, 0),
//           VSCode.Position.make(2, 0),
//           VSCode.Position.make(3, 0),
//         ]

//         acquire(setup)->flatMapOk(
//           setup => {
//             let document = VSCode.TextEditor.document(setup.editor)

//             document
//             ->Editor.Text.insert(VSCode.Position.make(0, 0), "\n\n\n")
//             ->flatMap(_ => IM.activate(setup, ~positions, ()))
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "b"))
//             ->flatMapOk(
//               actual =>
//                 if onUnix {
//                   IM.deepEqual(
//                     [
//                       RewriteIssued([
//                         ((0, 1), `♭`),
//                         ((2, 3), `♭`),
//                         ((4, 5), `♭`),
//                         ((6, 7), `♭`),
//                       ]),
//                       UpdateView,
//                       RewriteApplied,
//                     ],
//                     actual,
//                   )
//                 } else {
//                   IM.deepEqual(
//                     [
//                       RewriteIssued([
//                         ((0, 1), `♭`),
//                         ((3, 4), `♭`),
//                         ((6, 7), `♭`),
//                         ((9, 10), `♭`),
//                       ]),
//                       UpdateView,
//                       RewriteApplied,
//                     ],
//                     actual,
//                   )
//                 },
//             )
//             ->flatMapOk(
//               () => A.equal(`♭\n♭\n♭\n♭`, replaceCRLF(Editor.Text.getAll(document))), // string literal updated to v10.1.4
//             )
//             ->flatMapOk(() => IM.insertChar(setup, "n"))
//             ->flatMapOk(
//               actual =>
//                 if onUnix {
//                   IM.deepEqual(
//                     [
//                       RewriteIssued([
//                         ((0, 2), `𝕟`),
//                         ((3, 5), `𝕟`),
//                         ((6, 8), `𝕟`),
//                         ((9, 11), `𝕟`),
//                       ]),
//                       Deactivate,
//                       RewriteApplied,
//                     ],
//                     actual,
//                   )
//                 } else {
//                   IM.deepEqual(
//                     [
//                       RewriteIssued([
//                         ((0, 2), `𝕟`),
//                         ((4, 6), `𝕟`),
//                         ((8, 10), `𝕟`),
//                         ((12, 14), `𝕟`),
//                       ]),
//                       Deactivate,
//                       RewriteApplied,
//                     ],
//                     actual,
//                   )
//                 },
//             )
//             ->flatMapOk(
//               () => A.equal(`𝕟\n𝕟\n𝕟\n𝕟`, replaceCRLF(Editor.Text.getAll(document))), // string literal updated to v10.1.4
//             )
//           },
//         )
//       },
//     )
//     Q.it(
//       `should work just fine (∧)`,
//       () => {
//         acquire(setup)->flatMapOk(
//           setup => {
//             let positions = [
//               VSCode.Position.make(0, 0),
//               VSCode.Position.make(1, 1),
//               VSCode.Position.make(2, 2),
//               VSCode.Position.make(3, 3),
//             ]
//             let document = VSCode.TextEditor.document(setup.editor)

//             document
//             ->Editor.Text.insert(VSCode.Position.make(0, 0), "123\n123\n123\n123")
//             ->flatMap(_ => IM.activate(setup, ~positions, ()))
//             ->flatMapOk(IM.deepEqual([Activate]))
//             ->flatMapOk(() => IM.insertChar(setup, "a"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(
//               () => A.equal(`a123\n1a23\n12a3\n123a`, replaceCRLF(Editor.Text.getAll(document))), // string literal updated to v10.1.4
//             )
//             ->flatMapOk(() => IM.insertChar(setup, "n"))
//             ->flatMapOk(IM.deepEqual([RewriteIssued([]), UpdateView, RewriteApplied]))
//             ->flatMapOk(
//               () =>
//                 A.equal(`an123\n1an23\n12an3\n123an`, replaceCRLF(Editor.Text.getAll(document))), // string literal updated to v10.1.4
//             )
//             ->flatMapOk(() => IM.insertChar(setup, "d"))
//             ->flatMapOk(
//               actual =>
//                 if onUnix {
//                   IM.deepEqual(
//                     [
//                       RewriteIssued([
//                         ((0, 3), `∧`),
//                         ((8, 11), `∧`),
//                         ((16, 19), `∧`),
//                         ((24, 27), `∧`),
//                       ]),
//                       UpdateView,
//                       RewriteApplied,
//                     ],
//                     actual,
//                   )
//                 } else {
//                   IM.deepEqual(
//                     [
//                       RewriteIssued([
//                         ((0, 3), `∧`),
//                         ((9, 12), `∧`),
//                         ((18, 21), `∧`),
//                         ((27, 30), `∧`),
//                       ]),
//                       UpdateView,
//                       RewriteApplied,
//                     ],
//                     actual,
//                   )
//                 },
//             )
//             ->flatMapOk(
//               () =>
//                 A.equal(
//                   `∧123\n1∧23\n12∧3\n123∧`, // string literal updated to v10.1.4
//                   replaceCRLF(Editor.Text.getAll(document)),
//                 ),
//             )
//           },
//         )
//       },
//     )
//   })
// })
