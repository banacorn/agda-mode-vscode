open Mocha
open Test__Util

type typeArgs = {"text": string}
@module("vscode") @scope("commands")
external typeCommand: (@as("type") _, typeArgs) => promise<unit> = "executeCommand"

// Deterministic regression test for #300 ("TextEditor is closed/disposed"
// warnings). Runs the smallest sequence that used to trigger the warning:
// load a real Agda file (giving `Tokens` non-empty decorations), make its
// captured editor stale by switching away and back (VS Code hands the
// extension a fresh `TextEditor` on switch-back, while the old one stays
// captured by that state's listener closures), then make one small text
// edit. The `onDidChangeTextDocument` listener is now document-scoped and
// uses the current `state.editor` instead of the initialization-time
// captured editor, so the sequence no longer produces the exact warning.
//   AGDA_TEST_GLOB="Test__StaleEditorDecorationWarning*.js" npm test
describe("stale editor decoration warning (#300)", () => {
  Async.it("a text edit after staling the captured editor produces no warning", async () => {
    let warningPhrase = "TextEditor is closed/disposed"

    // open a file and trigger `Main.initialize` for it without a real
    // `agda-mode.load`, via the input-method activate/escape round trip
    let openNoLoad = async path => {
      let (editor, _channels) = await activateExtensionAndOpenFile(Path.asset(path), None)
      let _ = await VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
      let _ = await VSCode.Commands.executeCommand0("agda-mode.escape")
      editor
    }
    let _ = await openNoLoad("InputMethod.agda")

    // real load gives `Tokens` non-empty decorations to apply
    let _ = await AgdaMode.makeAndLoad("Goals.agda")

    // stale the captured editor for Goals.agda: switch to another file, then
    // back, so VS Code hands the extension a fresh `TextEditor` for
    // Goals.agda while the old one -- still captured by that state's
    // listener closures -- goes stale
    let _ = await File.open_(Path.asset("InputMethod.agda"))
    let _ = await File.open_(Path.asset("Goals.agda"))

    let before = ExtHostLog.countLinesContaining(warningPhrase)

    // smallest stimulus that used to reproduce: one character typed, then deleted
    let _ = await typeCommand({"text": "x"})
    let _ = await VSCode.Commands.executeCommand0("deleteLeft")

    let after = ExtHostLog.countLinesContaining(warningPhrase)

    Assert.equal(0, after - before)
  })
})
