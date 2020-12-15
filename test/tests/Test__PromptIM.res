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
//   view: View__Controller.t,
//   chan: Chan.t<IM.Log.t>,
// }

// // let activateExtensionAndLoad = (): Promise.t<setup> => {
// //   //   let disposables = []
// //   //   let extensionPath = Path.extensionPath()
// //   //   let chan = Main.activateWithoutContext(disposables, extensionPath)

// // //   VSCode.Commands.executeCommand0("agda-mode.load")->tap(result => Js.log("!!!!"))
// //   //   VSCode.Commands.executeCommand0("agda-mode.load")->flatMap(result => {
// //   //     Js.log("!!!!!!!!!!")
// //   //     result
// //   //   })->tap(Js.log2("!!!"))

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
//     // it(j`test`, () => {
//     //   activateExtensionAndLoad()->get(chan => {
//     //     Js.log("chan")
//     //     ()
//     //     // let document = VSCode.TextEditor.document(setup.editor)
//     //     // IM.activate(setup, ())
//     //   })
//     // })

//     // it(j`should translate "\\bn" to "𝕟"`, () => {
//     //   let promptIM = PromptIM.make()

//     //   promptIM->PromptIM.activate("")

//     //   promptIM->testPromptIMUpdate(~input=j`b`, ~output=j`♭`, ())
//     //   promptIM->testPromptIMUpdate(~input=j`♭n`, ~output=j`𝕟`, ~command=Deactivate, ())
//     // })

//     // it(j`should translate "garbage \\\\bn" to "garbage 𝕟"`, () => {
//     //   let promptIM = PromptIM.make()

//     //   promptIM->PromptIM.activate("garbage ")

//     //   promptIM->testPromptIMUpdate(~input=j`garbage b`, ~output=j`garbage ♭`, ())

//     //   promptIM->testPromptIMUpdate(
//     //     ~input=j`garbage ♭n`,
//     //     ~output=j`garbage 𝕟`,
//     //     ~command=Deactivate,
//     //     (),
//     //   )
//     // })
//     ()
//   })

//   //   describe("Backspacing", () => it(j`should work just fine`, () => {
//   //       let promptIM = PromptIM.make()

//   //       promptIM->PromptIM.activate("")

//   //       promptIM->testPromptIMUpdate(~input=j`l`, ~output=j`←`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`←a`, ~output=j`←a`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`←am`, ~output=j`←am`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`←amb`, ~output=j`←amb`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`←ambd`, ~output=j`←ambd`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`←ambda`, ~output=j`λ`, ())
//   //       promptIM->testPromptIMUpdate(~input=j``, ~output=j`lambd`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`lamb`, ~output=j`lamb`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`lambd`, ~output=j`lambd`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`lambda`, ~output=j`λ`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`λb`, ~output=j`λb`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`λba`, ~output=j`λba`, ())
//   //       promptIM->testPromptIMUpdate(~input=j`λbar`, ~output=j`ƛ`, ~command=Deactivate, ())
//   //     }))
// })

