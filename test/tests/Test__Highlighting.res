open Mocha
open Test__Util

describe("Highlighting", () => {
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Issue180.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Issue180.agda"), fileContent.contents))

  Async.it("should work after inserting a newline", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue180.agda")
    let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(6, 0), "\n")

    open Highlighting__SemanticToken
    let expected = [
      make(7, (0, 3), Function, Some([])),
      make(7, (6, 7), Type, Some([])),
      make(7, (10, 11), Type, Some([])),
      make(7, (14, 15), Type, Some([])),
      make(8, (0, 1), Variable, Some([])),
      make(8, (2, 3), Function, Some([])),
      make(8, (4, 5), Variable, Some([])),
    ]

    let tokens = await ctx.state.highlighting->Highlighting.getSemanticTokens->Resource.get
    let actual = tokens->Array.sliceToEnd(~start=12)

    Assert.deepStrictEqual(actual, expected)
  })

  Async.it("should work after deleting an empty line", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue180.agda")

    let _ = await Editor.Text.delete(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(6, 0)),
    )

    open Highlighting__SemanticToken
    let expected = [
      make(5, (0, 3), Function, Some([])),
      make(5, (6, 7), Type, Some([])),
      make(5, (10, 11), Type, Some([])),
      make(5, (14, 15), Type, Some([])),
      make(6, (0, 1), Variable, Some([])),
      make(6, (2, 3), Function, Some([])),
      make(6, (4, 5), Variable, Some([])),
    ]
    let tokens = await ctx.state.highlighting->Highlighting.getSemanticTokens->Resource.get
    let actual = tokens->Array.sliceToEnd(~start=12)

    Assert.deepStrictEqual(actual, expected)
  })

  Async.it("should work after deleting an existing line", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue180.agda")

    let _ = await Editor.Text.delete(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(7, 0)),
    )

    open Highlighting__SemanticToken
    let expected = [
      make(5, (0, 1), Variable, Some([])),
      make(5, (2, 3), Function, Some([])),
      make(5, (4, 5), Variable, Some([])),
    ]
    let tokens = await ctx.state.highlighting->Highlighting.getSemanticTokens->Resource.get
    let actual = tokens->Array.sliceToEnd(~start=12)

    Assert.deepStrictEqual(actual, expected)
  })
})
