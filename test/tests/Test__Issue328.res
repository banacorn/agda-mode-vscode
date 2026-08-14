open Mocha
open Test__Util

type typeArgs = {"text": string}

@module("vscode") @scope("commands")
external registerCommandWithArgs: (string, typeArgs => promise<unit>) => VSCode.Disposable.t =
  "registerCommand"

@send external disposeCommand: VSCode.Disposable.t => unit = "dispose"

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

  let largeModule = () => {
    let declarations = Array.fromInitializer(~length=2000, i => {
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
      let ctx = await AgdaMode.makeAndLoad(asset)
      let semanticTokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get

      // Keep the fixture large enough to exercise the path reported in the
      // issue. If it stops producing substantial highlighting, this test is
      // no longer a meaningful reproducer.
      Assert.equal(semanticTokens->Array.length > 1000, true)

      Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(1, 0))

      // This is the part of Vim's installed behavior that exposes the
      // latency: `type` is extension-host mediated and delegates to the
      // editor's built-in implementation.
      let typeOverride = registerCommandWithArgs("type", args =>
        VSCode.Commands.executeCommand1("default:type", args)
      )

      let elapsedMilliseconds = try {
        let startedAt = Js.Date.now()
        let _ = await VSCode.Commands.executeCommand1("type", {"text": "x"})
        Js.Date.now() -. startedAt
      } catch {
      | exn =>
        typeOverride->disposeCommand
        raise(exn)
      }
      typeOverride->disposeCommand

      // A single character must not block Vim's command queue for a
      // human-perceptible interval. Include the observation in the failure
      // so this test is also a reproducible measurement of #328.
      Assert.equal(
        elapsedMilliseconds < 100.0,
        true,
        ~message=
          "intercepted typing took " ++
          Float.toString(elapsedMilliseconds) ++
          " ms after Agda highlighting",
      )
    },
  )
})
