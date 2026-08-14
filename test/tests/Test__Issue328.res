open Mocha
open Test__Util

type typeArgs = {"text": string}

// Regression test for #328. VSCodeVim overrides the `type` command and, in
// insert mode, delegates to `default:type` from the extension host. This
// makes work performed synchronously by document-change listeners part of
// the key-handling path.
//
// agda-mode currently handles each edit by rebuilding every decoration and
// semantic token, then emitting `Tokens.onUpdate`, before the intercepted
// `type` command can return. On a large loaded file that blocks Vim's task
// queue for long enough to make each typed character take seconds.
//
// The test installs only the relevant command-shaped boundary; it does not
// depend on VSCodeVim itself.
describe("issue #328: intercepted typing after highlighting", () => {
  This.timeout(60000)

  let asset = "Issue328.agda"
  let fileContent = ref("")
  let requestedVimMode =
    NodeJs.Process.process->NodeJs.Process.env->Dict.get("AGDA_TEST_VIM")

  let largeModule = () => {
    let declarations = Array.fromInitializer(~length=800, i => {
      let name = "f" ++ Int.toString(i)
      name ++ " : D → D\n" ++ name ++ " x = x\n"
    })
    "module Issue328 where\n\ndata D : Set where\n  d : D\n\n" ++ declarations->Array.join("\n")
  }

  Async.beforeEach(async () => {
    fileContent := await File.read(Path.asset(asset))
    await File.write(Path.asset(asset), largeModule())
  })

  Async.afterEach(async () => {
    await File.write(Path.asset(asset), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  Async.it(
    "keeps an intercepted type command responsive after highlighting a large file",
    async () => {
      let expectedVim = switch requestedVimMode {
      | Some("on") => true
      | Some("off") => false
      | _ =>
        This.skip()
        false
      }

      let hasVim = switch VSCode.Extensions.getExtension("vscodevim.vim") {
      | Some(vim) =>
        let config = VSCode.Workspace.getConfiguration(Some("vim"), None)
        await config->VSCode.WorkspaceConfiguration.updateGlobalSettings(
          "startInInsertMode",
          true,
          None,
        )
        let _ = await vim->VSCode.Extension.activate
        true
      | None => false
      }
      Assert.equal(hasVim, expectedVim, ~message="VSCodeVim test-profile mismatch")

      let ctx = await AgdaMode.makeAndLoad(asset)
      let semanticTokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get

      if hasVim {
        let _ = await VSCode.Commands.executeCommand0("extension.vim_insert")
        await wait(100)
      }

      // Keep the fixture large enough to exercise the path reported in the
      // issue. If it stops producing substantial highlighting, this test is
      // no longer a meaningful reproducer.
      Assert.equal(semanticTokens->Array.length > 1000, true)

      Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(1, 0))

      let originalLength = ctx.state.document->VSCode.TextDocument.getText(None)->String.length
      let (typed, resolveTyped, _) = Util.Promise_.pending()
      let stopWatching = VSCode.Workspace.onDidChangeTextDocument(event => {
        let document = event->VSCode.TextDocumentChangeEvent.document
        if document == ctx.state.document &&
          document->VSCode.TextDocument.getText(None)->String.length >= originalLength + 1 {
          resolveTyped()
        }
      })

      let startedAt = Js.Date.now()
      let _ = await Promise.all(
        ["x"]->Array.map(text =>
          VSCode.Commands.executeCommand1("type", {"text": text})
        ),
      )
      await typed
      let elapsedMilliseconds = Js.Date.now() -. startedAt
      stopWatching->VSCode.Disposable.dispose->ignore

      // A single character must not block Vim's command queue for a
      // human-perceptible interval. Include the observation in the failure
      // so this test is also a reproducible measurement of #328.
      Assert.equal(
        elapsedMilliseconds < 100.0,
        true,
        ~message=
          (hasVim ? "Vim-present" : "Vim-absent") ++
          " one-character typing took " ++
          Float.toString(elapsedMilliseconds) ++
          " ms after Agda highlighting",
      )
    },
  )
})
