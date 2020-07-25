open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
// open VSCode;
open! Belt;

open Test__Util;
module Goal = Goal.Impl(Editor);
module Task = Task.Impl(Editor);
module Dispatcher = Dispatcher.Impl(Editor);
module GoalHandler = Handle__Goal.Impl(Editor);

let activateExtension =
    (fileName): Promise.t(option((VSCode.TextEditor.t, Event.t(unit)))) =>
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
  editor->Editor.setText(range, "")->Promise.map(_ => ());
};

let insertChar = (emitter: Event.t(unit), editor, char) => {
  let promise = emitter.once();
  let pos = Editor.getCursorPosition(editor);
  editor->Editor.insertText(pos, char)->Promise.flatMap(_ => promise);
};

let backspace = (emitter: Event.t(unit), editor) => {
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
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←a|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "m"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←am|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "b"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←amb|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "d"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←ambd|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
          ->Promise.tap(() => Assert.equal(Editor.getText(editor), {j|λ|j}))
          ->Promise.flatMap(deactivateInputhMethod)
        )
    });
    Q.it({j|should translate "bn" to "𝕟"|j}, () => {
      (env^)
      ->Option.mapWithDefault(Promise.resolved(), ((editor, emitter)) => {
          activateInputhMethod()
          ->Promise.flatMap(() => insertChar(emitter, editor, "b"))
          ->Promise.flatMap(() => insertChar(emitter, editor, "n"))
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
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←a|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "m"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←am|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "b"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←amb|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "d"))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|←ambd|j})
            )
          ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
          ->Promise.tap(() => Assert.equal(Editor.getText(editor), {j|λ|j}))
          ->Promise.flatMap(() => backspace(emitter, editor))
          ->Promise.tap(() =>
              Assert.equal(Editor.getText(editor), {j|lambd|j})
            )
          ->Promise.flatMap(deactivateInputhMethod)
        )
    })
  });
});
