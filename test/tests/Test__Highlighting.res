open Mocha
open Test__Util

describe("Highlighting", () => {
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Issue180.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Issue180.agda"), fileContent.contents))

  Async.it("should work after inserting a newline", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue180.agda")
    let _ = await Editor.Text.insert(ctx.state.editor, VSCode.Position.make(6, 0), "\n")

    let expected = [
      "(7:0-3) function",
      "(7:6-7) type",
      "(7:10-11) type",
      "(7:14-15) type",
      "(8:0-1) variable",
      "(8:2-3) function",
      "(8:4-5) variable",
    ]

    let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
    let actual =
      tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

    Assert.deepStrictEqual(actual, expected)
  })

  Async.it("should work after deleting an empty line", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue180.agda")

    let _ = await Editor.Text.delete(
      ctx.state.editor,
      VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(6, 0)),
    )

    let expected = [
      "(5:0-3) function",
      "(5:6-7) type",
      "(5:10-11) type",
      "(5:14-15) type",
      "(6:0-1) variable",
      "(6:2-3) function",
      "(6:4-5) variable",
    ]
    let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
    let actual =
      tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

    Assert.deepStrictEqual(actual, expected)
  })

  Async.it("should work after deleting an existing line", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue180.agda")

    let _ = await Editor.Text.delete(
      ctx.state.editor,
      VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(7, 0)),
    )

    let expected = ["(5:0-1) variable", "(5:2-3) function", "(5:4-5) variable"]
    let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
    let actual =
      tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

    Assert.deepStrictEqual(actual, expected)
  })
})
