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

// let openTextEditor = content =>
//   Workspace.openTextDocumentWithOptions(
//     Some({"content": content, "language": "agda"}),
//   )
//   ->Promise.flatMap(textDocument =>
//       Window.showTextDocumentWithShowOptions(textDocument, None)
//     );
let dirname: option(string) = [%bs.node __dirname];
let dirname = dirname->Option.getWithDefault("./");

// let openAsset = path => {
//   let fileUri = VSCode.Uri.file(Path.asset(path));
//   VSCode.Commands.executeCommand1("vscode.open", fileUri)
//   ->Promise.map(() => VSCode.Window.activeTextEditor);
// };

let openTextEditor = path => {
  VSCode.Workspace.openTextDocumentWithFileName(path)
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );
};

let wait = ms => {
  let (promise, resolve) = Promise.pending();
  Js.Global.setTimeout(resolve, ms)->ignore;
  promise;
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
