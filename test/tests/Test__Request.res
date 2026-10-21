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

  // Like the Emacs mode (agda2-goal-Range), the range of a goal spans its content between "{!" and "!}",
  // in Agda positions: code points from 1, with CRLF as one character.
  // The content is sent with LF line breaks, so that Agda counts it the same way.
  describe("Goal range and content", () => {
    let filepath = "/tmp/Test.agda"
    let openDocument = content =>
      VSCode.Workspace.openTextDocumentWithOptions(
        Some({"content": content, "language": "plaintext"}),
      )
    let encode = (document, request) =>
      Request.encode(document, "2.9.0", filepath, GHC, [], true, request)

    // "𝕄" is one code point but two UTF-16 code units
    let content = "-- 𝕄\r\nx = {! f\r\n  𝕄 !}\r\n"
    let goal: Goal.t = {index: 0, indexString: "0", start: 11, end: 24}
    let range = "(intervalsToRange (Just (mkAbsolute \"/tmp/Test.agda\")) [Interval () (Pn () 12 2 7) (Pn () 19 3 5)])"

    Async.it("should send the range of Give in Agda positions and its content with LF line breaks", async () => {
      let document = await openDocument(content)
      Assert.strictEqual(
        encode(document, Request.Give(goal)),
        "IOTCM \"/tmp/Test.agda\" NonInteractive Direct( Cmd_give WithoutForce 0 " ++ range ++ " \"f\\n  𝕄\" )",
      )
    })

    Async.it("should send the range of Refine in Agda positions and its content with LF line breaks", async () => {
      let document = await openDocument(content)
      Assert.strictEqual(
        encode(document, Request.Refine(goal)),
        "IOTCM \"/tmp/Test.agda\" NonInteractive Direct( Cmd_refine_or_intro False 0 " ++ range ++ " \"f\\n  𝕄\" )",
      )
    })

    Async.it("should send the range of Auto in Agda positions and its content with LF line breaks", async () => {
      let document = await openDocument(content)
      Assert.strictEqual(
        encode(document, Request.Auto(Simplified, goal)),
        "IOTCM \"/tmp/Test.agda\" NonInteractive Direct( Cmd_autoOne Simplified 0 " ++ range ++ " \"f\\n  𝕄\" )",
      )
    })

    Async.it("should send the range of Case in Agda positions and its content with LF line breaks", async () => {
      let document = await openDocument(content)
      Assert.strictEqual(
        encode(document, Request.Case(goal)),
        "IOTCM \"/tmp/Test.agda\" NonInteractive Direct( Cmd_make_case 0 " ++ range ++ " \"f\\n  𝕄\" )",
      )
    })
  })
})
