// open Test__Util
// open Mocha

// type setup = {
//   editor: VSCode.TextEditor.t,
//   channels: State__Type.channels,
// }

// let acquire = setup =>
//   switch setup.contents {
//   | None => raise(Exn("Setup is not initialized"))
//   | Some(setup) => setup
//   }

// let cleanup = async setup => {
//   let range = VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(100, 0))
//   let _ = await setup.editor->VSCode.TextEditor.document->Editor.Text.replace(range, "")
// }

// module IM = {
//   include IM

//   let wait = async setup => await setup.channels.inputMethod->Chan.once
//   let wait2nd = async setup => {
//     let _ = await setup.channels.inputMethod->Chan.once
//     await setup.channels.inputMethod->Chan.once
//   }

//   let activate = async (setup: setup, ~positions=?, ()) => {
//     let positions = positions->Option.getOr(Editor.Cursor.getMany(setup.editor))
//     Editor.Cursor.setMany(setup.editor, positions)
//     let commandLoad = wait(setup)
//     let commandComputeNormalForm = wait2nd(setup)
//     // load
//     let _ = await VSCode.Commands.executeCommand0("agda-mode.load")
//     // compute normal form
//     let _ = await VSCode.Commands.executeCommand0("agda-mode.compute-normal-form[DefaultCompute]")

//     // view->WebviewPanel.sendRequest

//     let _ = await commandLoad
//     await commandComputeNormalForm
//   }
// }

// describe_only("Input Method (Prompt)", () => {
//   let setup = ref(None)

//   // initialize the setup before all tests
//   Async.before(async () => {
//     let (editor, channels) = await activateExtensionAndOpenFile(Path.asset("InputMethod.agda"))
//     setup := Some({editor, channels})
//   })

//   // cleanup the editor after each test
//   Async.afterEach(async () => {
//     let setup = acquire(setup)
//     await cleanup(setup)
//   })

//   describe("Activation", () => {
//     Async.it(`should be successful`, async () => {
//         let setup = acquire(setup)
//         let log = await IM.activate(setup, ())
//         Assert.deepEqual([IM.Log.Activate], log)
//       },
//     )
//   })
// })

// // module Js' = Js
// // open Promise
// // module Js = Js'

// // let testPromptIMUpdate = (self, ~input, ~output, ~command=?, ()) => {
// //   let result = self->State__InputMethod.PromptIM.update(input)
// //   switch result {
// //   | None => Assert.fail("shouldn't be deactivated after \"" ++ (input ++ "\""))
// //   | Some((output', command')) =>
// //     Assert.equal(output', output)
// //     switch command {
// //     | None => ()
// //     | Some(command) => Assert.equal(command', command)
// //     }
// //   }
// // }

// type setup = {
//   view: WebviewPanel.t,
//   chan: Chan.t<IM.Log.t>,
// }

// // let acquire = setup =>
// //   switch setup.contents {
// //   | None => raise(Exn("Setup is not initialized"))
// //   | Some(setup) => setup
// //   }

// // let cleanup = async setup => {
// //   let range = VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(100, 0))
// //   let _ = await setup.editor->VSCode.TextEditor.document->Editor.Text.replace(range, "")
// // }

// let activateExtensionAndLoad = async (): setup => {
//     let disposables = []
//     let extensionPath = Path.extensionPath
//     let chan = Main.activateWithoutContext(disposables, extensionPath, ...)

//     let promise = await VSCode.Commands.executeCommand0("agda-mode.load")
//     let _ = await promise

//     let view = await VSCode.Window.showTextDocumentWithUri(VSCode.Uri.file(Path.asset("InputMethod.agda")),None,)

//     {
//       view: view,
//       chan: chan,
//     }
// }

// // let acquire = setup =>
// //   switch setup.contents {
// //   | None => resolved(Error(Util.Error("Cannot acquire the setup")))
// //   | Some(setup) => resolved(Ok(setup))
// //   }

// describe_only("Input Method in Prompt", () => {
//   describe("Insertion", () => {
//     it(`test`, () => {
//       activateExtensionAndLoad()->get(chan => {
//         ()
//         // let document = VSCode.TextEditor.document(setup.editor)
//         // IM.activate(setup, ())
//       })
//     })

//     // it(`should translate "\\bn" to "𝕟"`, () => {
//     //   let promptIM = PromptIM.make()

//     //   promptIM->PromptIM.activate("")

//     //   promptIM->testPromptIMUpdate(~input=`b`, ~output=`♭`, ())
//     //   promptIM->testPromptIMUpdate(~input=`♭n`, ~output=`𝕟`, ~command=Deactivate, ())
//     // })

//     // it(`should translate "garbage \\\\bn" to "garbage 𝕟"`, () => {
//     //   let promptIM = PromptIM.make()

//     //   promptIM->PromptIM.activate("garbage ")

//     //   promptIM->testPromptIMUpdate(~input=`garbage b`, ~output=`garbage ♭`, ())

//     //   promptIM->testPromptIMUpdate(
//     //     ~input=`garbage ♭n`,
//     //     ~output=`garbage 𝕟`,
//     //     ~command=Deactivate,
//     //     (),
//     //   )
//     // })
//     ()
//   })

//   //   describe("Backspacing", () => it(`should work just fine`, () => {
//   //       let promptIM = PromptIM.make()

//   //       promptIM->PromptIM.activate("")

//   //       promptIM->testPromptIMUpdate(~input=`l`, ~output=`←`, ())
//   //       promptIM->testPromptIMUpdate(~input=`←a`, ~output=`←a`, ())
//   //       promptIM->testPromptIMUpdate(~input=`←am`, ~output=`←am`, ())
//   //       promptIM->testPromptIMUpdate(~input=`←amb`, ~output=`←amb`, ())
//   //       promptIM->testPromptIMUpdate(~input=`←ambd`, ~output=`←ambd`, ())
//   //       promptIM->testPromptIMUpdate(~input=`←ambda`, ~output=`λ`, ())
//   //       promptIM->testPromptIMUpdate(~input=``, ~output=`lambd`, ())
//   //       promptIM->testPromptIMUpdate(~input=`lamb`, ~output=`lamb`, ())
//   //       promptIM->testPromptIMUpdate(~input=`lambd`, ~output=`lambd`, ())
//   //       promptIM->testPromptIMUpdate(~input=`lambda`, ~output=`λ`, ())
//   //       promptIM->testPromptIMUpdate(~input=`λb`, ~output=`λb`, ())
//   //       promptIM->testPromptIMUpdate(~input=`λba`, ~output=`λba`, ())
//   //       promptIM->testPromptIMUpdate(~input=`λbar`, ~output=`ƛ`, ~command=Deactivate, ())
//   //     }))
// })
