open Mocha
open Test__Util

type typeArgs = {"text": string}

// Black-box regression test for #328: after loading and highlighting a large
// Agda file with VSCodeVim installed, typing a character should stay responsive.
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
        let _ = await vim->VSCode.Extension.activate
        true
      | None => false
      }
      Assert.equal(hasVim, expectedVim, ~message="VSCodeVim test-profile mismatch")
      if !hasVim {
        This.skip()
      }

      let ctx = await AgdaMode.makeAndLoad(asset)
      let semanticTokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get

      // Keep the fixture large enough to exercise the path reported in the
      // issue. If it stops producing substantial highlighting, this test is
      // no longer a meaningful reproducer.
      Assert.equal(semanticTokens->Array.length > 1000, true)

      Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(1, 0))
      // Enter insert mode through the same key a user would press, then allow
      // Vim to finish handling that command before measuring the next key.
      let _ = await VSCode.Commands.executeCommand1("type", {"text": "i"})
      await wait(500)

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
      let command = VSCode.Commands.executeCommand1("type", {"text": "x"})
      await typed
      let elapsedMilliseconds = Js.Date.now() -. startedAt
      let _ = await command
      stopWatching->VSCode.Disposable.dispose->ignore

      // Include the observation in the failure so the red test is also a
      // reproducible measurement of #328.
      Assert.equal(
        elapsedMilliseconds < 100.0,
        true,
        ~message=
          "Vim-intercepted typing took " ++
          Float.toString(elapsedMilliseconds) ++
          " ms after Agda highlighting",
      )
    },
  )
})
