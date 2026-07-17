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
  // restore Goals.agda after each test: loading expands its "?" holes to
  // "{!   !}" in the buffer, and a later `Load` would save that to disk
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

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

// Cross-talk regression for #300: with a state loaded for one Agda file,
// edits made in a *different* file must not touch that state's bookkeeping.
// Before the fix, the `onDidChangeTextDocument` listener in `Main.initialize`
// was unfiltered, so an edit in any document would shift this state's token
// positions and goal ranges by the other document's change deltas.
describe("cross-document event routing (#300)", () => {
  // same restoration as above: this describe also loads Goals.agda
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

  Async.it("an edit in another file leaves this state's tokens and goals untouched", async () => {
    // a real load gives the state non-empty token and goal bookkeeping
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")

    // snapshot by value: serialized tokens, goal offsets, and delta count.
    // NOTE: goals are snapshotted as offsets via `getGoalPositionByIndex`
    // instead of `serializeGoals`, because the latter renders offsets against
    // `VSCode.Window.activeTextEditor` -- which this test changes on purpose
    let snapshotGoals = () =>
      Array.fromInitializer(~length=Goals.size(ctx.state.goals), index =>
        Goals.getGoalPositionByIndex(ctx.state.goals, index)
      )
    let tokensBefore = ctx.state.tokens->Tokens.toTokenArray->Array.map(Token.toString)
    let goalsBefore = snapshotGoals()
    Assert.deepStrictEqual(ctx.state.tokens->Tokens.deltasLength, 0)

    // edit another Agda file: insert a line at the very start -- the stimulus
    // with the largest blast radius, since it shifts every offset in that
    // document -- then remove it to restore the buffer for other tests
    let editorB = await File.open_(Path.asset("InputMethod.agda"))
    let documentB = editorB->VSCode.TextEditor.document
    let _ = await Editor.Text.insert(documentB, VSCode.Position.make(0, 0), "-- cross-talk\n")
    let _ = await Editor.Text.delete(
      documentB,
      VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(1, 0)),
    )
    // goal rescanning runs asynchronously in the listener; give a misrouted
    // event time to land before asserting
    await wait(100)

    // this state's bookkeeping must be untouched
    Assert.deepStrictEqual(ctx.state.tokens->Tokens.deltasLength, 0)
    Assert.deepStrictEqual(
      ctx.state.tokens->Tokens.toTokenArray->Array.map(Token.toString),
      tokensBefore,
    )
    Assert.deepStrictEqual(snapshotGoals(), goalsBefore)
  })
})
