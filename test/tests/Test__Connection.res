open! BsMocha.Mocha
open Test__Util

// @module("vscode") @scope("commands")
// external executeCommand: string => Promise.t<option<State.t>> = "executeCommand"

// let activateExtension = (fileName): Promise.t<VSCode.TextEditor.t> => {
//   // 1. activate the extension
//   let disposables = []
//   let extensionPath = Path.extensionPath()
//   let globalStoragePath = Path.globalStoragePath()
//   let _ = Main.activateWithoutContext(disposables, extensionPath, globalStoragePath)
//   // 2. open some editor
//   VSCode.Window.showTextDocumentWithUri(VSCode.Uri.file(fileName), None)
// }
describe("Connection", ~timeout=10000, () => {
  Q.it("should download the language server", () => {
    let globalStoragePath = Path.globalStoragePath()
    let useLSP = true

    Connection.start(globalStoragePath, useLSP, _ => ())->Promise.mapError(e => {
      let (header, body) = Connection.Error.toString(e)
      Exn(header ++ "\n" ++ body)
    })
    // ->Promise.tapOk(status => {
    //   let msg = switch status {
    //   | Emacs(version, _) => "Emacs v" ++ version
    //   | LSP(version, ViaCommand(_, _, _, LanguageServerMule.Method.FromGitHub(_, release, _))) =>
    //     "ALS prebuilt " ++ release.tagName ++ " (Agda v" ++ version ++ ")"
    //   | LSP(version, ViaCommand(_)) => "ALS v" ++ version
    //   | LSP(_, ViaTCP(_)) => "ALS (TCP)"
    //   }
    //   Js.log(msg)
    // })
  })

  // Q.it_skip("should download the language server", () => {
  //   activateExtension(Path.asset("InputMethod.agda"))->Promise.flatMap(_editor => {
  //     executeCommand("agda-mode.load")->Promise.map(_state => {
  //       // switch result {
  //       // | None => Js.log("None")
  //       // | Some(state) => Js.log(state.globalStoragePath)
  //       // }
  //       Ok()
  //     })
  //   })
  // })
})
