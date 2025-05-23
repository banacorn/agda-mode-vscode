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

//     Assert.deepEqual(
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

describe("agda-mode:case", () => {
  This.timeout(10000)
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset("CaseSplit.agda"))))
  Async.after(async () => await File.write(Path.asset("CaseSplit.agda"), fileContent.contents))

  Async.it("should have more goals after splitting", async () => {
    let ctx = await AgdaMode.makeAndLoad("CaseSplit.agda")
    await ctx->AgdaMode.case(Some(VSCode.Position.make(7, 16), "x"))
    Assert.deepEqual(Array.length(ctx.state.goals), 10)
  })
})
