// // open Belt
// // open! BsMocha.Mocha
// // open Test__Util

// // module Js' = Js
// // open Promise
// // module Js = Js'

// // let testPromptIMUpdate = (self, ~input, ~output, ~command=?, ()) => {
// //   let result = self->Editor.update(input)
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

// // let activateExtensionAndLoad = (): Promise.t<setup> => {
// //   //   let disposables = []
// //   //   let extensionPath = Path.extensionPath()
// //   //   let chan = Main.activateWithoutContext(disposables, extensionPath)

// // //   VSCode.Commands.executeCommand0("agda-mode.load")
// //   //   VSCode.Commands.executeCommand0("agda-mode.load")->flatMap(result => {
// //   //     result
// //   //   })=

// //   //   VSCode.Window.showTextDocumentWithUri(
// //   //     VSCode.Uri.file(Path.asset("InputMethod.agda")),
// //   //     None,
// //   //   )->map(editor => {
// //   //     editor: editor,
// //   //     chan: chan,
// //   //   })
// // }

// // let acquire = setup =>
// //   switch setup.contents {
// //   | None => resolved(Error(Util.Error("Cannot acquire the setup")))
// //   | Some(setup) => resolved(Ok(setup))
// //   }

// describe_only("Input Method (Prompt)", () => {
//   describe("Insertion", () => {
//     // it(`test`, () => {
//     //   activateExtensionAndLoad()->get(chan => {
//     //     ()
//     //     // let document = VSCode.TextEditor.document(setup.editor)
//     //     // IM.activate(setup, ())
//     //   })
//     // })

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

