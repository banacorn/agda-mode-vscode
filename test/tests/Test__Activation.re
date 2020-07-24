open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;
open! Belt;

open Test__Util;
let source = {j|
data ℕ : Set where
  Z : ℕ
  S : ℕ → ?
|j};

let dirname: option(string) = [%bs.node __dirname];
let dirname = dirname->Option.getWithDefault("./");

let openTextEditor = path => {
  VSCode.Workspace.openTextDocumentWithFileName(path)
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );
};

describe_skip("Extention Activation", () => {
  P.it("should be able to acquire \"banacorn.agda-mode\"", () => {
    let extension = VSCode.Extensions.getExtension("banacorn.agda-mode");
    (
      switch (extension) {
      | None =>
        Assert.fail("cannot acquire the extension");
        Promise.resolved();
      | Some(extension) =>
        openTextEditor(Path.asset("A.agda"))
        ->Promise.flatMap(editor => {
            extension
            ->VSCode.Extension.activate
            ->Promise.flatMap(_ => {
                VSCode.Commands.executeCommand0("agda-mode.load")
              })
            ->Promise.flatMap(_ => {wait(1000)})
            ->Promise.tap(() => {Js.log(Editor.getText(editor))})
          })
      }
    )
    ->Promise.Js.toBsPromise;
  })
});
