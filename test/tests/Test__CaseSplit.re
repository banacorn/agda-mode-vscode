open VSCode;
module VSRange = Range;
open Belt;

open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;

open Test__Util;

let makeTextEditor = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );

let forGoal = (goals, index, callback) => {
  switch (goals[index]) {
  | None =>
    Assert.fail("failed to instantiate goal #" ++ string_of_int(index));
    Promise.resolved();
  | Some(goal) => callback(goal)
  };
};

describe("Handle__Goal.caseSplitAux", () => {
  // let lines = [|{j|Z + y = ?|j}, {j|S x + y = ?|j}|];
  P.it(
    "should calculate the infomation needed for case splitting correctly", ()
    => {
      Window.showTextDocumentWithUri(
        Uri.file(Path.asset("CaseSplit.agda")),
        None,
      )
      ->Promise.map(editor => {
          let document = TextEditor.document(editor);
          Goal.makeMany(editor, [|0, 1, 2, 3, 4, 5, 6, 7, 8|])
          ->Promise.map(goals => {
              goals->Array.map(goal => {
                // convert `rewriteRange` to text in that range because range offsets are different on different OSs
                let (inWhereClause, indentWidth, rewriteRange) =
                  Handle__Goal.caseSplitAux(document, goal);
                let rewriteRange =
                  VSRange.make(
                    TextDocument.positionAt(document, fst(rewriteRange)),
                    TextDocument.positionAt(document, snd(rewriteRange)),
                  );
                (
                  inWhereClause,
                  indentWidth,
                  Editor.Text.get(document, rewriteRange),
                );
              })
            })
          ->Promise.map(results => {
              Assert.deep_equal(
                results,
                [|
                  (false, 9, {j|x → {!   !}|j}),
                  (false, 23, {j|y → {!   !}|j}),
                  (false, 4, {j|x → {!   !}|j}),
                  (false, 4, {j|y → {!   !}|j}),
                  (true, 13, {j|x → {!   !}|j}),
                  (true, 13, {j|y → {!   !}|j}),
                  (true, 2, {j|x → {!   !}|j}),
                  (true, 2, {j|y → {!   !}|j}),
                  (false, 13, {j|x → {!   !}|j}),
                |],
              )
            });
        })
      ->Promise.Js.toBsPromise
    })
    //   describe("Function", () => {
    //     let lines = [|{j|Z + y = ?|j}, {j|S x + y = ?|j}|];
    //     P.it("without indentation", () => {
    //       let source = {j|
    // _+_ : ℕ → ℕ → ℕ
    // x + y = {!   !}
    // |j};
    //       makeTextEditor(source)
    //       ->Promise.flatMap(editor => {
    //           let dispatcher =
    //             Dispatcher.make(Path.extensionPath(), editor, () => ());
    //           Goal.makeMany(editor, [|0|])
    //           ->Promise.flatMap(goals => {
    //               switch (goals[0]) {
    //               | None =>
    //                 Assert.fail("failed to instantiate any goals");
    //                 Promise.resolved();
    //               | Some(goal) =>
    //                 let tasks =
    //                   Handle__Goal.handle(ReplaceWithLines(goal, lines));
    //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
    //                 ->Promise.map(() => {
    //                     let expected = {j|
    // _+_ : ℕ → ℕ → ℕ
    // Z + y = ?
    // S x + y = ?
    // |j};
    //                     Assert.equal(editor->Editor.getText, expected);
    //                   });
    //               }
    //             });
    //         })
    //       ->Promise.Js.toBsPromise;
    //     });
    //     P.it("with indentation", () => {
    //       let source = {j|
    //   _+_ : ℕ → ℕ → ℕ
    //   x + y = {!   !}
    // |j};
    //       makeTextEditor(source)
    //       ->Promise.flatMap(editor => {
    //           let dispatcher =
    //             Dispatcher.make(Path.extensionPath(), editor, () => ());
    //           Goal.makeMany(editor, [|0|])
    //           ->Promise.flatMap(goals => {
    //               switch (goals[0]) {
    //               | None =>
    //                 Assert.fail("failed to instantiate any goals");
    //                 Promise.resolved();
    //               | Some(goal) =>
    //                 let tasks =
    //                   Handle__Goal.handle(ReplaceWithLines(goal, lines));
    //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
    //                 ->Promise.map(() => {
    //                     let expected = {j|
    //   _+_ : ℕ → ℕ → ℕ
    //   Z + y = ?
    //   S x + y = ?
    // |j};
    //                     Assert.equal(editor->Editor.getText, expected);
    //                   });
    //               }
    //             });
    //         })
    //       ->Promise.Js.toBsPromise;
    //     });
    //   });
    //   describe_only("Extended lambda", () => {
    //     let lines = [|{j|true → ?|j}, {j|false → ?|j}|];
    //     P.it({j|"in curly brackets, newline after λ"|j}, () => {
    //       let source = {j|
    // not : Bool → Bool
    // not = λ
    //     { x → {!   !}}
    // |j};
    //       makeTextEditor(source)
    //       ->Promise.flatMap(editor => {
    //           let dispatcher =
    //             Dispatcher.make(Path.extensionPath(), editor, () => ());
    //           Goal.makeMany(editor, [|0|])
    //           ->Promise.flatMap(goals => {
    //               switch (goals[0]) {
    //               | None =>
    //                 Assert.fail("failed to instantiate any goals");
    //                 Promise.resolved();
    //               | Some(goal) =>
    //                 let (inWhereClause, rewriteRange) =
    //                   Handle__Goal.inWhereClause(editor, goal);
    //                 Assert.equal(
    //                   inWhereClause,
    //                   false,
    //                   ~message="shouldn't be in a where clause",
    //                 );
    //                 Assert.deep_equal(
    //                   rewriteRange,
    //                   VSRange.make(
    //                     Editor.Point.make(3, 5),
    //                     Editor.Point.make(3, 17),
    //                   ),
    //                 );
    //                 let tasks =
    //                   Handle__Goal.handle(ReplaceWithLambda(goal, lines));
    //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
    //                 ->Promise.map(() => {
    //                     let expected = {j|
    // not : Bool → Bool
    // not = λ
    //     { true → ?
    //     ; false → ?}
    // |j};
    //                     Assert.equal(editor->Editor.getText, expected);
    //                   });
    //               }
    //             });
    //         })
    //       ->Promise.Js.toBsPromise;
    //     });
    //     P.it({j|"in curly brackets, no newline after λ"|j}, () => {
    //       let source = {j|
    // not : Bool → Bool
    // not = λ { x → ? }
    // |j};
    //       makeTextEditor(source)
    //       ->Promise.flatMap(editor => {
    //           let dispatcher =
    //             Dispatcher.make(Path.extensionPath(), editor, () => ());
    //           Goal.makeMany(editor, [|0|])
    //           ->Promise.flatMap(goals => {
    //               switch (goals[0]) {
    //               | None =>
    //                 Assert.fail("failed to instantiate any goals");
    //                 Promise.resolved();
    //               | Some(goal) =>
    //                 let (inWhereClause, rewriteRange) =
    //                   Handle__Goal.inWhereClause(editor, goal);
    //                 Assert.equal(
    //                   inWhereClause,
    //                   false,
    //                   ~message="shouldn't be in a where clause",
    //                 );
    //                 Assert.deep_equal(
    //                   rewriteRange,
    //                   VSRange.make(
    //                     Editor.Point.make(2, 9),
    //                     Editor.Point.make(2, 21),
    //                   ),
    //                 );
    //                 let tasks =
    //                   Handle__Goal.handle(ReplaceWithLambda(goal, lines));
    //                 Dispatcher.addToTheBackCritical(dispatcher, tasks)
    //                 ->Promise.map(() => {
    //                     let expected = {j|
    // not : Bool → Bool
    // not = λ { true → ?
    //         ; false → ?}
    // |j};
    //                     ();
    //                     // Assert.equal(editor->Editor.getText, expected);
    //                   });
    //               }
    //             });
    //         })
    //       ->Promise.Js.toBsPromise;
    //     });
    //   });
});
