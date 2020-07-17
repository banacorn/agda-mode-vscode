open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;
open! Belt;

open Test__Util;

let openTextEditor = path => {
  VSCode.Workspace.openTextDocumentWithFileName(path)
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );
};

let makeTextEditor = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );

let wait = ms => {
  let (promise, resolve) = Promise.pending();
  Js.Global.setTimeout(resolve, ms)->ignore;
  promise;
};

let source = {j|
_+_ : ℕ → ℕ → ℕ
x + y = {!   !}
|j};

let lines = [|{j|Z + y = ?|j}, {j|S x + y = ?|j}|];

// let replaceWithLines = lines => ();

describe_only("Case split", () => {
  P.it("0", () => {
    module Goal = Goal.Impl(Editor);
    module Task = Task.Impl(Editor);
    module Dispatcher = Dispatcher.Impl(Editor);
    module GoalHandler = Handle__Goal.Impl(Editor);
    Js.log(Path.extensionPath());

    // replaceWithLines
    makeTextEditor(source)
    ->Promise.flatMap(editor => {
        Js.log(editor->Editor.getText);

        let dispatcher =
          Dispatcher.make(Path.extensionPath(), editor, () => ());
        Goal.makeMany(editor, [|0|])
        ->Promise.flatMap(goals => {
            switch (goals[0]) {
            | None =>
              Assert.fail("failed to instantiate any goals");
              Promise.resolved();
            | Some(goal) =>
              let tasks = GoalHandler.handle(ReplaceWithLines(goal, lines));
              Dispatcher.addToTheBackCritical(dispatcher, tasks);
            }
          })
        ->Promise.map(() => Js.log(editor->Editor.getText));
      })
    ->Promise.Js.toBsPromise;
  })
});
