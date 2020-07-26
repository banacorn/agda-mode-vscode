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

let activateExtension =
    (fileName)
    : Promise.t(option((VSCode.TextEditor.t, Event.t(InputMethod.event)))) =>
  switch (VSCode.Extensions.getExtension("banacorn.agda-mode")) {
  | None => Promise.resolved(None)
  | Some(extension) =>
    openTextEditor(fileName)
    ->Promise.flatMap(editor => {
        extension
        ->VSCode.Extension.activate
        ->Promise.map(emitter => Some((editor, emitter)))
      })
  };

let cleanup = editor => {
  let range =
    Editor.Range.make(Editor.Point.make(0, 0), Editor.Point.make(100, 0));
  editor->Editor.replaceText(range, "")->Promise.map(_ => ());
};

let insertChar =
    (emitter: Event.t(InputMethod.event), editor, ~positions=?, char) => {
  let promise = emitter.once();
  switch (positions) {
  | None =>
    let pos = Editor.getCursorPosition(editor);
    editor->Editor.insertText(pos, char)->Promise.flatMap(_ => promise);
  | Some(positions) =>
    editor->Editor.insertTexts(positions, char)->Promise.flatMap(_ => promise)
  };
};

let backspace = (emitter: Event.t(InputMethod.event), editor) => {
  let promise = emitter.once();
  let end_ = Editor.getCursorPosition(editor);
  let start = end_->Editor.Point.translate(0, -1);
  let range = Editor.Range.make(start, end_);
  editor->Editor.deleteText(range)->Promise.flatMap(_ => promise);
};

let activateInputhMethod = () => {
  VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
  ->Promise.flatMap(result => result);
};

let deactivateInputhMethod = () =>
  VSCode.Commands.executeCommand0("agda-mode.escape")
  ->Promise.flatMap(result => result);

describe_only("InputMethod", () => {
  let env = ref(None);
  Q.before(() => {
    activateExtension(Path.asset("InputMethod.agda"))
    ->Promise.map(
        fun
        | None => BsMocha.Assert.fail("cannot acquire the extension")
        | Some((editor, emitter)) => {
            env := Some((editor, emitter));
          },
      )
  });

  Q.after_each(() => {
    (env^)
    ->Option.mapWithDefault(Promise.resolved(), ((editor, _)) =>
        cleanup(editor)
      )
  });

  describe("Insertion", () => {
    Q.it({j|should translate "lambda" to "λ"|j}, () => {
      (env^)
      ->Option.mapWithDefault(Promise.resolved(), ((editor, emitter)) =>
          activateInputhMethod()
          ->Promise.flatMap(() => insertChar(emitter, editor, "l"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←|j})
            )
          ->Promise.flatMap(_ => insertChar(emitter, editor, "a"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.map(() =>
              Assert.equal(Editor.getText(editor), {j|←a|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "m"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.map(() =>
              Assert.equal(Editor.getText(editor), {j|←am|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "b"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←amb|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "d"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←ambd|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() => Assert.equal(Editor.getText(editor), {j|λ|j}))
          ->Promise.flatMap(deactivateInputhMethod)
        )
    });
    Q.it({j|should translate "bn" to "𝕟"|j}, () => {
      (env^)
      ->Option.mapWithDefault(Promise.resolved(), ((editor, emitter)) => {
          activateInputhMethod()
          ->Promise.flatMap(() => insertChar(emitter, editor, "b"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.flatMap(() => insertChar(emitter, editor, "n"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.map(() => {
              Assert.equal(Editor.getText(editor), {j|𝕟|j})
            })
          ->Promise.flatMap(deactivateInputhMethod)
        })
    });
  });

  describe("Backspace", () => {
    Q.it({j|should work just fine|j}, () => {
      (env^)
      ->Option.mapWithDefault(Promise.resolved(), ((editor, emitter)) =>
          activateInputhMethod()
          ->Promise.flatMap(() => insertChar(emitter, editor, "l"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←a|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "m"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←am|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "b"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←amb|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "d"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←ambd|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() => Assert.equal(Editor.getText(editor), {j|λ|j}))
          ->Promise.flatMap(() => backspace(emitter, editor))
          ->Promise.map(A.equal(InputMethod.Change))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|lambd|j})
            )
          ->Promise.flatMap(deactivateInputhMethod)
        )
    })
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
  //     ->Option.mapWithDefault(Promise.resolved(), ((editor, emitter)) =>
  //         activateInputhMethod()
  //         ->Promise.flatMap(() => insertChar(emitter, editor, "\n\n\n"))
  //         ->Promise.tap(() =>
  //             Assert.equal(Editor.getText(editor), {j|←|j})
  //           )
  //         ->Promise.flatMap(() =>
  //             insertChar(emitter, editor, ~positions, "l")
  //           )
  //         ->Promise.tap(() =>
  //             Assert.equal(Editor.getText(editor), {j|←|j})
  //           )
  //         ->Promise.flatMap(deactivateInputhMethod)
  //       )
  //   });
  // });
});
