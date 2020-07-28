open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
// open VSCode;
open! Belt;

open Test__Util;
module Goal = Goal.Impl(Editor);
module Task = Task.Impl(Editor);
module InputMethod = InputMethod.Impl(Editor);
module Dispatcher = Dispatcher.Impl(Editor);
module GoalHandler = Handle__Goal.Impl(Editor);
open Promise;

type setup = {
  editor: Editor.editor,
  emitter: Event.t(InputMethod.event),
};

let activateExtension = (fileName): Promise.t(setup) => {
  module Main = Main.Impl(Editor);

  let disposables = [||];
  let extensionPath = Path.extensionPath();
  let emitter = Main.activateWithoutContext(disposables, extensionPath);
  openTextEditor(fileName)->map(editor => {editor, emitter});
};

let acquire = setup => {
  switch (setup^) {
  | None => resolved(Error("Cannot acquire the setup"))
  | Some(setup) => resolved(Ok(setup))
  };
};

let cleanup = setup => {
  let range =
    Editor.Range.make(Editor.Point.make(0, 0), Editor.Point.make(100, 0));
  setup.editor->Editor.replaceText(range, "")->map(_ => ());
};

let insertChar = (setup, ~positions=?, char) => {
  let promise = setup.emitter.once();
  switch (positions) {
  | None =>
    let pos = Editor.getCursorPosition(setup.editor);
    setup.editor->Editor.insertText(pos, char)->flatMap(_ => promise);
  | Some(positions) =>
    setup.editor->Editor.insertTexts(positions, char)->flatMap(_ => promise)
  };
};

let backspace = setup => {
  let promise = setup.emitter.once();
  let end_ = Editor.getCursorPosition(setup.editor);
  let start = end_->Editor.Point.translate(0, -1);
  let range = Editor.Range.make(start, end_);
  setup.editor->Editor.deleteText(range)->flatMap(_ => promise);
};

let activateInputhMethod = () => {
  VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
  ->flatMap(result => result);
};

let activateInputhMethod2 = setup => {
  VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
  ->flatMap(result => result);
};

let deactivateInputhMethod = () =>
  VSCode.Commands.executeCommand0("agda-mode.escape")
  ->flatMap(result => result);

describe("InputMethod", () => {
  let setup = ref(None);
  Q.before(() => {
    activateExtension(Path.asset("InputMethod.agda"))
    ->map(value => {setup := Some(value)})
  });

  Q.after_each(() => {(setup^)->Option.mapWithDefault(resolved(), cleanup)});

  describe("Insertion", () => {
    Q.it({j|should translate "lambda" to "λ"|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          activateInputhMethod()
          ->flatMap(() => insertChar(setup, "l"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() => Assert.equal(Editor.getText(setup.editor), {j|←|j}))
          ->flatMap(_ => insertChar(setup, "a"))
          ->map(A.equal(InputMethod.Change))
          ->map(() => Assert.equal(Editor.getText(setup.editor), {j|←a|j}))
          ->flatMap(() => insertChar(setup, "m"))
          ->map(A.equal(InputMethod.Change))
          ->map(() =>
              Assert.equal(Editor.getText(setup.editor), {j|←am|j})
            )
          ->flatMap(() => insertChar(setup, "b"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() =>
              Assert.equal(Editor.getText(setup.editor), {j|←amb|j})
            )
          ->flatMap(() => insertChar(setup, "d"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() =>
              Assert.equal(Editor.getText(setup.editor), {j|←ambd|j})
            )
          ->flatMap(() => insertChar(setup, "a"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() => Assert.equal(Editor.getText(setup.editor), {j|λ|j}))
          ->flatMap(deactivateInputhMethod)
          ->map(x => Ok(x))
        })
    });
    Q.it({j|should translate "bn" to "𝕟"|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          activateInputhMethod()
          ->flatMap(() => insertChar(setup, "b"))
          ->map(A.equal(InputMethod.Change))
          ->flatMap(() => insertChar(setup, "n"))
          ->map(A.equal(InputMethod.Change))
          ->map(() => {
              Assert.equal(Editor.getText(setup.editor), {j|𝕟|j})
            })
          ->flatMap(deactivateInputhMethod)
          ->map(x => Ok(x))
        })
    });
  });
  describe("Backspace", () => {
    Q.it({j|should work just fine|j}, () => {
      acquire(setup)
      ->flatMapOk(setup => {
          activateInputhMethod()
          ->flatMap(() => insertChar(setup, "l"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() => Assert.equal(Editor.getText(setup.editor), {j|←|j}))
          ->flatMap(() => insertChar(setup, "a"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() => Assert.equal(Editor.getText(setup.editor), {j|←a|j}))
          ->flatMap(() => insertChar(setup, "m"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() =>
              Assert.equal(Editor.getText(setup.editor), {j|←am|j})
            )
          ->flatMap(() => insertChar(setup, "b"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() =>
              Assert.equal(Editor.getText(setup.editor), {j|←amb|j})
            )
          ->flatMap(() => insertChar(setup, "d"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() =>
              Assert.equal(Editor.getText(setup.editor), {j|←ambd|j})
            )
          ->flatMap(() => insertChar(setup, "a"))
          ->map(A.equal(InputMethod.Change))
          ->tap(() => Assert.equal(Editor.getText(setup.editor), {j|λ|j}))
          ->flatMap(() => backspace(setup))
          ->map(A.equal(InputMethod.Change))
          ->tap(() =>
              Assert.equal(Editor.getText(setup.editor), {j|lambd|j})
            )
          ->flatMap(deactivateInputhMethod)
          ->map(x => Ok(x))
        })
    })
  });
});

// describe("Multiple cursors at once", () => {
//   let positions = [|
//     Editor.Point.make(0, 0),
//     Editor.Point.make(1, 0),
//     Editor.Point.make(2, 0),
//     Editor.Point.make(3, 0),
//   |];
//   Q.it({j|should work just fine|j}, () => {
//     (env^)
//     ->Option.mapWithDefault(resolved(), ((editor, emitter)) =>
//         activateInputhMethod()
//         ->flatMap(() => insertChar(setup, "\n\n\n"))
//         ->tap(() =>
//             Assert.equal(Editor.getText(setup.editor), {j|←|j})
//           )
//         ->flatMap(() =>
//             insertChar(setup, ~positions, "l")
//           )
//         ->tap(() =>
//             Assert.equal(Editor.getText(setup.editor), {j|←|j})
//           )
//         ->flatMap(deactivateInputhMethod)
//       )
//   });
// });
