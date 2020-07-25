open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
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

let insertChar = (emitter: Event.t(unit), editor, char) => {
  let promise = emitter.once();
  let pos = Editor.getCursorPosition(editor);
  editor->Editor.insertText(pos, char)->Promise.flatMap(_ => promise);
};

let activateInputhMethod = () => {
  VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
  ->Promise.flatMap(result => result);
};

let cleanup = editor => {
  let range =
    Editor.Range.make(Editor.Point.make(0, 0), Editor.Point.make(100, 0));
  editor->Editor.setText(range, "")->Promise.map(_ => ());
  // ->Promise.flatMap(_ => {VSCode.Commands.executeCommand0("agda-mode.quit")})
  // ->Promise.flatMap(result => result);
};

let deactivateInputhMethod = (emitter, editor) =>
  insertChar(emitter, editor, " ");

describe_only("InputMethod", () => {
  let prep = ref(None);
  P.before(() => {
    activateExtension(Path.asset("InputMethod.agda"))
    ->Promise.map(
        fun
        | None => BsMocha.Assert.fail("cannot acquire the extension")
        | Some((editor, emitter)) => {
            prep := Some((editor, emitter));
          },
      )
    ->Promise.Js.toBsPromise
  });

  P.after_each(() => {
    (prep^)
    ->Option.mapWithDefault(
        Promise.resolved()->Promise.Js.toBsPromise, ((editor, _)) =>
        cleanup(editor)->Promise.Js.toBsPromise
      )
  });

  P.it({j|should translate "lambda" to "λ"|j}, () => {
    (prep^)
    ->Option.mapWithDefault(
        Promise.resolved()->Promise.Js.toBsPromise, ((editor, emitter)) =>
        activateInputhMethod()
        ->Promise.flatMap(() => insertChar(emitter, editor, "l"))
        ->Promise.tap(() => Assert.equal(Editor.getText(editor), {j|←|j}))
        ->Promise.flatMap(() => insertChar(emitter, editor, "a"))
        ->Promise.tap(() => Assert.equal(Editor.getText(editor), {j|←a|j}))
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
        ->Promise.flatMap(() => deactivateInputhMethod(emitter, editor))
        ->Promise.Js.toBsPromise
      )
  });
  // P.it({j|should translate "bn" to "𝕟"|j}, () => {
  //   (prep^)
  //   ->Option.mapWithDefault(
  //       Promise.resolved()->Promise.Js.toBsPromise, ((editor, emitter)) => {
  //       activateInputhMethod()
  //       ->Promise.flatMap(() => insertChar(emitter, editor, "b"))
  //       ->Promise.flatMap(() => insertChar(emitter, editor, "n"))
  //       ->Promise.map(_ => {
  //           Assert.equal(Editor.getText(editor), {j|𝕟|j})
  //         })
  //       ->Promise.tap(() => Js.log("before deactivateInputhMethod"))
  //       // ->Promise.flatMap(() => deactivateInputhMethod(emitter, editor))
  //       ->Promise.tap(() => Js.log("deactivateInputhMethod"))
  //       ->Promise.Js.toBsPromise
  //     })
  // });
});
