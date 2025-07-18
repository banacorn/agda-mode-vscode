open Mocha
open Test__Util

// describe("State__Goal.caseSplitAux dry run", () => {
//   Async.it("should calculate the information needed for case splitting correctly", async () => {
//     let editor = await VSCode.Window.showTextDocumentWithUri(
//       VSCode.Uri.file(Path.asset("CaseSplit.agda")),
//       None,
//     )
//     let document = VSCode.TextEditor.document(editor)
//     let goals = await Goal.makeMany(editor, [(0, VSCode.Range.make(VSCode.Position.make(7, 14), VSCode.Position.make(7, 21))), 1, 2, 3, 4, 5, 6, 7, 8])
//     let results = goals->Array.map(
//       goal => {
//         // convert `rewriteRange` to text in that range because range offsets are different on different OSs
//         let (inWhereClause, indentWidth, rewriteRange) = State__Goal.caseSplitAux(document, goal)
//         let rewriteRange = VSCode.Range.make(
//           VSCode.TextDocument.positionAt(document, fst(rewriteRange)),
//           VSCode.TextDocument.positionAt(document, snd(rewriteRange)),
//         )
//         (inWhereClause, indentWidth, Editor.Text.get(document, rewriteRange))
//       },
//     )

//     Assert.deepStrictEqual(
//       results,
//       [
//         (false, 9, `x → {!   !}`),
//         (false, 23, `y → {!   !}`),
//         (false, 4, `x → {!   !}`),
//         (false, 4, `y → {!   !}`),
//         (true, 13, `x → {!   !}`),
//         (true, 13, `y → {!   !}`),
//         (true, 2, `x → {!   !}`),
//         (true, 2, `y → {!   !}`),
//         (false, 13, `x → {!   !}`),
//       ],
//     )
//   })
// })

// describe("agda-mode:case", () => {
//   This.timeout(10000)
//   let fileContent = ref("")

//   Async.before(async () => fileContent := (await File.read(Path.asset("CaseSplit.agda"))))
//   Async.after(async () => await File.write(Path.asset("CaseSplit.agda"), fileContent.contents))

//   Async.it("should have more goals after splitting", async () => {
//     let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")
//     await ctx->AgdaMode.case(Some(VSCode.Position.make(7, 16), "x"))
//     Assert.deepStrictEqual(Array.length(ctx.state.goals), 10)
//   })
// })

describe("agda-mode.case", () => {
  This.timeout(4000)
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("CaseSplit.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("CaseSplit.agda"), fileContent.contents))

  // Async.it("should be responded with correct responses", async () => {
  //   let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")

  //   let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(8, 11), "x")
  //   let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
  //     Request.Case({
  //       index: 0,
  //       indexString: "0",
  //       start: 181,
  //       end: 188,
  //     }),
  //   )

  //   let filteredResponses = responses->Array.filter(filteredResponse)
  //   Assert.deepStrictEqual(
  //     filteredResponses,
  //     [
  //       // InteractionPoints([0, 1, 2, 3, 4, 5, 6, 7, 8]),
  //       // MakeCase(ExtendedLambda, ["Z → ?", "(S x) → ?"]),
  //     ],
  //   )
  // })

  Async.it("should handle both MakeCase::ExtendedLambda and MakeCase::Function", async () => {
    let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")
    // 6 goals before case splitting
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 6)

    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(8, 11), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 7)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(13, 16), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 8)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(18, 11), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 9)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(23, 20), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 10)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(28, 9), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 11)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(32, 21), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 12)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(9, 13), ~payload="y")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 13)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(15, 20), ~payload="x")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 14)

    // compare file content before and after
    let actual = await File.read(Path.asset("CaseSplit.agda"))
    let expected = await File.read(Path.asset("CaseSplit.agda.out"))
    Assert.deepStrictEqual(actual, expected)
  })
})
