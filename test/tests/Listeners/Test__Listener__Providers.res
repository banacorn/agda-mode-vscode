open Mocha
open Test__Util

// Invoke the provider through the real VS Code built-in command, not
// `Editor.Provider.registerDefinitionProvider`'s callback directly -- this
// is what actually exercises `ExtensionEvents.registerDefinitionProvider`'s
// `Registry.get(document)` routing (Main.res), which a direct call to
// `Tokens.goToDefinition` (as in Test__Tokens.res) bypasses entirely.
@module("vscode") @scope("commands")
external executeDefinitionProvider: (
  @as("vscode.executeDefinitionProvider") _,
  VSCode.Uri.t,
  VSCode.Position.t,
) => promise<array<VSCode.LocationLink.t>> = "executeCommand"

// Routing test for `ExtensionEvents.registerDefinitionProvider` (Main.res) --
// registered exactly once at activation, it must resolve each request
// against the state whose document the request came from. Before
// centralization this provider was registered once per loaded file, so a
// request against one file could be served by another file's stale
// registration.
describe("definition provider routing", () => {
  Async.it(
    "resolves within the requesting file's own state, not another loaded file's",
    async () => {
      let libCtx = await AgdaMode.makeAndLoad("Lib.agda")
      let gotoCtx = await AgdaMode.makeAndLoad("GotoDefinition.agda")

      // `targetUri` is asserted via `fsPath`, not whole-object
      // `deepStrictEqual` -- the real `vscode.Uri` returned by the built-in
      // command hasn't had its lazily-cached `_fsPath`/`_formatted` fields
      // computed yet, while a `Uri.file(...)` constructed locally has, so a
      // raw structural comparison fails on that cache state alone.
      let libUri = libCtx.state.document->VSCode.TextDocument.uri
      let libResult = await executeDefinitionProvider(libUri, VSCode.Position.make(12, 27))
      Assert.deepStrictEqual(
        libResult->Array.map(link => (
          link.originSelectionRange,
          link.targetRange,
          link.targetSelectionRange,
          link.targetUri->VSCode.Uri.fsPath,
        )),
        [
          (
            Some(VSCode.Range.make(VSCode.Position.make(12, 26), VSCode.Position.make(12, 27))),
            VSCode.Range.make(VSCode.Position.make(12, 22), VSCode.Position.make(12, 22)),
            None,
            libUri->VSCode.Uri.fsPath,
          ),
        ],
      )

      let gotoUri = gotoCtx.state.document->VSCode.TextDocument.uri
      let gotoResult = await executeDefinitionProvider(gotoUri, VSCode.Position.make(5, 6))
      Assert.deepStrictEqual(
        gotoResult->Array.map(link => (
          link.originSelectionRange,
          link.targetRange,
          link.targetSelectionRange,
          link.targetUri->VSCode.Uri.fsPath,
        )),
        [
          (
            Some(VSCode.Range.make(VSCode.Position.make(5, 6), VSCode.Position.make(5, 7))),
            VSCode.Range.make(VSCode.Position.make(1, 5), VSCode.Position.make(1, 5)),
            None,
            gotoUri->VSCode.Uri.fsPath,
          ),
        ],
      )
    },
  )
})
