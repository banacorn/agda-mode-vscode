open Belt
open! BsMocha.Mocha
open Test__Util


module Js' = Js
open Promise
module Js = Js'

module Assert = BsMocha.Assert
module P = BsMocha.Promise

// let makeTextEditor = content =>
//   VSCode.Workspace.openTextDocumentWithOptions(
//     Some({"content": content, "language": "agda"}),
//   )->Promise.flatMap(textDocument => VSCode.Window.showTextDocumentWithShowOptions(textDocument, None))

// let forGoal = (goals, index, callback) =>
//   switch goals[index] {
//   | None =>
//     Assert.fail("failed to instantiate goal #" ++ string_of_int(index))
//     Promise.resolved()
//   | Some(goal) => callback(goal)
//   }

describe("State__Goal.caseSplitAux", () =>
  // let lines = [|{j|Z + y = ?|j}, {j|S x + y = ?|j}|];
  Q.it("should calculate the infomation needed for case splitting correctly", () =>
    VSCode.Window.showTextDocumentWithUri(VSCode.Uri.file(Path.asset("CaseSplit.agda")), None)
    ->Promise.flatMap(editor => {
      let document = VSCode.TextEditor.document(editor)
      Goal.makeMany(editor, [0, 1, 2, 3, 4, 5, 6, 7, 8])
      ->Promise.map(goals =>
        goals->Array.map(goal => {
          // convert `rewriteRange` to text in that range because range offsets are different on different OSs
          let (inWhereClause, indentWidth, rewriteRange) = State__Goal.caseSplitAux(document, goal)
          let rewriteRange = VSCode.Range.make(
            VSCode.TextDocument.positionAt(document, fst(rewriteRange)),
            VSCode.TextDocument.positionAt(document, snd(rewriteRange)),
          )
          (inWhereClause, indentWidth, Editor.Text.get(document, rewriteRange))
        })
      )
      ->Promise.map(results =>
        Ok(Assert.deep_equal(
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
        ))
      )
    })
  )
)
