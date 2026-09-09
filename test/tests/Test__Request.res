open Mocha

describe("Request.encode", () => {
  describe("Compile", () => {
    // any real document works here -- Request.encode's Compile branch never
    // reads the document's content, only the `filepath` argument passed in
    // separately (see `buildRange`, which Compile never calls)
    Async.it("should send the selected backend into the Cmd_compile request", async () => {
      let editor = await Test__Util.File.open_(Test__Util.Path.asset("GotoDefinition.agda"))
      let document = editor->VSCode.TextEditor.document
      let filepath = "/tmp/Test.agda"

      open Config__Backend
      [GHC, GHCNoMain, LaTeX, QuickLaTeX, JS, HTML, Dot]->Array.forEach(backend => {
        let actual = Request.encode(document, "2.7.0", filepath, backend, [], true, Request.Compile)
        let expected =
          `IOTCM "${filepath}" NonInteractive Direct( Cmd_compile ${Config__Backend.encode(
              backend,
            )} "${filepath}" [] )`
        Assert.strictEqual(actual, expected)
      })
    })
  })
})
