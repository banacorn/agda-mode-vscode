open VSCode
module VSRange = Range
open Belt

open! BsMocha.Mocha
module Assert = BsMocha.Assert
module P = BsMocha.Promise

open Test__Util

let makeTextEditor = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )->Promise.flatMap(textDocument => Window.showTextDocumentWithShowOptions(textDocument, None))

let forGoal = (goals, index, callback) =>
  switch goals[index] {
  | None =>
    Assert.fail("failed to instantiate goal #" ++ string_of_int(index))
    Promise.resolved()
  | Some(goal) => callback(goal)
  }

describe("Handle__Goal.caseSplitAux", () =>
  // let lines = [|{j|Z + y = ?|j}, {j|S x + y = ?|j}|];
  P.it("should calculate the infomation needed for case splitting correctly", () =>
    Window.showTextDocumentWithUri(Uri.file(Path.asset("CaseSplit.agda")), None)
    ->Promise.map(editor => {
      let document = TextEditor.document(editor)
      Goal.makeMany(editor, [0, 1, 2, 3, 4, 5, 6, 7, 8])
      ->Promise.map(goals => goals->Array.map(goal => {
          // convert `rewriteRange` to text in that range because range offsets are different on different OSs
          let (inWhereClause, indentWidth, rewriteRange) = Handle__Goal.caseSplitAux(document, goal)
          let rewriteRange = VSRange.make(
            TextDocument.positionAt(document, fst(rewriteRange)),
            TextDocument.positionAt(document, snd(rewriteRange)),
          )
          (inWhereClause, indentWidth, Editor.Text.get(document, rewriteRange))
        }))
      ->Promise.map(results =>
        Assert.deep_equal(
          results,
          [
            (false, 9, j`x → {!   !}`),
            (false, 23, j`y → {!   !}`),
            (false, 4, j`x → {!   !}`),
            (false, 4, j`y → {!   !}`),
            (true, 13, j`x → {!   !}`),
            (true, 13, j`y → {!   !}`),
            (true, 2, j`x → {!   !}`),
            (true, 2, j`y → {!   !}`),
            (false, 13, j`x → {!   !}`),
          ],
        )
      )
    })
    ->Promise.Js.toBsPromise
  )
)
