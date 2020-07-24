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

module Goal = Goal.Impl(Editor);
module Task = Task.Impl(Editor);
module Dispatcher = Dispatcher.Impl(Editor);
module GoalHandler = Handle__Goal.Impl(Editor);

describe_only("InputMethod", () => {
  P.it("should should fail", () => {
    (
      switch (getExtension()) {
      | None =>
        Assert.fail("cannot acquire the extension");
        Promise.resolved();
      | Some(extension) =>
        openTextEditor(Path.asset("InputMethod.agda"))
        ->Promise.flatMap(editor => {
            extension
            ->VSCode.Extension.activate
            ->Promise.flatMap(_ =>
                VSCode.Commands.executeCommand0(
                  "agda-mode.input-symbol[Activate]",
                )
              )
            ->Promise.flatMap(result => result)
            ->Promise.flatMap(() => {
                let pos = Editor.getCursorPosition(editor);
                editor->Editor.insertText(pos, "l");
              })
            ->Promise.flatMap(_ => wait(1000))
            ->Promise.map(_ => {Js.log(Editor.getText(editor))})
          })
      }
    )
    ->Promise.Js.toBsPromise
  })
});
