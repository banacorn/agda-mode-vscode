open Mocha
open! Test__Util

describe("State__Goal.caseSplitAux dry run", () => {
  Q.it("should calculate the infomation needed for case splitting correctly", async () => {
    let editor = await VSCode.Window.showTextDocumentWithUri(
      VSCode.Uri.file(Path.asset("CaseSplit1.agda")),
      None,
    )
    let document = VSCode.TextEditor.document(editor)
    let goals = await Goal.makeMany(editor, [0, 1, 2, 3, 4, 5, 6, 7, 8])
    let results = goals->Array.map(
      goal => {
        // convert `rewriteRange` to text in that range because range offsets are different on different OSs
        let (inWhereClause, indentWidth, rewriteRange) = State__Goal.caseSplitAux(document, goal)
        let rewriteRange = VSCode.Range.make(
          VSCode.TextDocument.positionAt(document, fst(rewriteRange)),
          VSCode.TextDocument.positionAt(document, snd(rewriteRange)),
        )
        (inWhereClause, indentWidth, Editor.Text.get(document, rewriteRange))
      },
    )
    Ok(
      Assert.deepEqual(
        results,
        [
          (false, 9, `x → {!   !}`),
          (false, 23, `y → {!   !}`),
          (false, 4, `x → {!   !}`),
          (false, 4, `y → {!   !}`),
          (true, 13, `x → {!   !}`),
          (true, 13, `y → {!   !}`),
          (true, 2, `x → {!   !}`),
          (true, 2, `y → {!   !}`),
          (false, 13, `x → {!   !}`),
        ],
      ),
    )
  })
})

describe_skip("Integration test", () => {
  let fileContent = ref("")

  Q.before(() => readFile(Path.asset("CaseSplit2.agda"), fileContent))
  Q.after(() => restoreFile(Path.asset("CaseSplit2.agda"), fileContent))

  Async.it("should have more goals after splitting", async () => {
    let context = await Agda.make("CaseSplit2.agda")
    switch await Agda.load(context) {
    | Ok(result) =>
      switch await Agda.case(Some(VSCode.Position.make(7, 16), "x"), result) {
      | Ok((_, state)) => Assert.deepEqual(Array.length(state.goals), 10)
      | Error(error) => raise(error)
      }
    | Error(error) => raise(error)
    }
  })
})
