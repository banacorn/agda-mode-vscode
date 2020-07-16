open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;
open! Belt;

// open Test__Util;

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

let replaceWithLines = lines => ();

describe_only("Case split", () => {
  P.it("0", () => {
    module Goal = Goal.Impl(Editor);
    module Task = Task.Impl(Editor);
    module GoalHandler = Handle__Goal.Impl(Editor);
    module StateDispatcherPair = States.StateDispatcherPair.Impl(Editor);

    let a = StateDispatcherPair.make;

    // replaceWithLines
    makeTextEditor(source)
    ->Promise.flatMap(editor => {
        Goal.makeMany(editor, [|0|])
        ->Promise.map(goals => {
            switch (goals[0]) {
            | None => Assert.fail("failed to instantiate any goals")
            | Some(goal) =>
              let tasks = GoalHandler.handle(ReplaceWithLines(goal, lines));
              Js.log(tasks->List.map(Task.toString)->Util.Pretty.list);
            }
          })
      })
    ->Promise.Js.toBsPromise;
  })
});
