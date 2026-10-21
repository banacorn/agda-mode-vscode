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

  describe("Highlight", () => {
    let filepath = "/tmp/Test.agda"
    let openDocument = content =>
      VSCode.Workspace.openTextDocumentWithOptions(
        Some({"content": content, "language": "plaintext"}),
      )
    let encode = (document, version, highlightingMethod, request) =>
      Request.encode(document, version, filepath, GHC, [], highlightingMethod, request)

    Async.it("should send Cmd_highlight with the Agda 2.8+ range and Direct highlighting", async () => {
      let document = await openDocument("f = suc zero")
      let actual = encode(document, "2.9.0", true, Request.Highlight(3, 4, 12))
      let expected =
        "IOTCM \"/tmp/Test.agda\" Interactive Direct( Cmd_highlight 3 " ++
        "(intervalsToRange (Just (mkAbsolute \"/tmp/Test.agda\")) [Interval () (Pn () 5 1 5) (Pn () 13 1 13)]) " ++ "\"suc zero\" )"
      Assert.strictEqual(actual, expected)
    })

    Async.it("should send Cmd_highlight with the Agda 2.5.1-2.7 range and Direct highlighting, even when Indirect is configured", async () => {
      let document = await openDocument("f = suc zero")
      let actual = encode(document, "2.7.0", false, Request.Highlight(3, 4, 12))
      let expected =
        "IOTCM \"/tmp/Test.agda\" Interactive Direct( Cmd_highlight 3 " ++
        "(intervalsToRange (Just (mkAbsolute \"/tmp/Test.agda\")) [Interval (Pn () 5 1 5) (Pn () 13 1 13)]) " ++ "\"suc zero\" )"
      Assert.strictEqual(actual, expected)
    })

    Async.it("should send Cmd_highlight with the pre-2.5.1 range", async () => {
      let document = await openDocument("f = suc zero")
      let actual = encode(document, "2.5.0", true, Request.Highlight(3, 4, 12))
      let expected =
        "IOTCM \"/tmp/Test.agda\" Interactive Direct( Cmd_highlight 3 " ++
        "(Range [Interval (Pn (Just (mkAbsolute \"/tmp/Test.agda\")) 5 1 5) (Pn (Just (mkAbsolute \"/tmp/Test.agda\")) 13 1 13)]) " ++ "\"suc zero\" )"
      Assert.strictEqual(actual, expected)
    })

    Async.it("should count code points and treat CRLF as one character in the range", async () => {
      // "𝕄" is one code point but two UTF-16 code units
      let document = await openDocument("-- 𝕄\r\nx = 𝕄 y\r\n")
      let actual = encode(document, "2.9.0", true, Request.Highlight(0, 11, 15))
      let expected =
        "IOTCM \"/tmp/Test.agda\" Interactive Direct( Cmd_highlight 0 " ++
        "(intervalsToRange (Just (mkAbsolute \"/tmp/Test.agda\")) [Interval () (Pn () 10 2 5) (Pn () 13 2 8)]) " ++ "\"𝕄 y\" )"
      Assert.strictEqual(actual, expected)
    })

    Async.it("should escape the highlighted text across lines", async () => {
      let document = await openDocument("f = g \"a\\b\"\n  h")
      let actual = encode(document, "2.9.0", true, Request.Highlight(1, 4, 15))
      let expected =
        "IOTCM \"/tmp/Test.agda\" Interactive Direct( Cmd_highlight 1 " ++
        "(intervalsToRange (Just (mkAbsolute \"/tmp/Test.agda\")) [Interval () (Pn () 5 1 5) (Pn () 16 2 4)]) " ++ "\"g \\\"a\\\\b\\\"\\n  h\" )"
      Assert.strictEqual(actual, expected)
    })

    Async.it("should send the highlighted text with LF line breaks from a CRLF document", async () => {
      let document = await openDocument("-- 𝕄\r\nf = g\r\n  h\r\n")
      let actual = encode(document, "2.9.0", true, Request.Highlight(1, 11, 17))
      let expected =
        "IOTCM \"/tmp/Test.agda\" Interactive Direct( Cmd_highlight 1 " ++
        "(intervalsToRange (Just (mkAbsolute \"/tmp/Test.agda\")) [Interval () (Pn () 10 2 5) (Pn () 15 3 4)]) " ++ "\"g\\n  h\" )"
      Assert.strictEqual(actual, expected)
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
