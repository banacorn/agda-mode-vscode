open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;
open! Belt;

let source = {j|
data ℕ : Set where
  Z : ℕ
  S : ℕ → ?
|j};

let openTextEditor = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );
let dirname: option(string) = [%bs.node __dirname];
let dirname = dirname->Option.getWithDefault("./");

describe_only("Extention Activation", () => {
  P.it("should be able to acquire \"banacorn.agda-mode\"", () => {
    let extension = VSCode.Extensions.getExtension("banacorn.agda-mode");
    (
      switch (extension) {
      | None =>
        Assert.fail("cannot acquire the extension");
        Promise.resolved();
      | Some(extension) =>
        extension
        ->VSCode.Extension.activate
        ->Promise.flatMap(() => {
            let fileName = Node.Path.join2(dirname, "assets/A.agda");
            let fileUri = VSCode.Uri.file(fileName);
            VSCode.Commands.executeCommand1("vscode.open", fileUri);
          })
        ->Promise.tap(Js.log)
        ->Promise.flatMap(() => {
            VSCode.Commands.executeCommand0("agda-mode.load")
          })
        ->Promise.tap(Js.log)
      // ->Promise.flatMap(() => {openTextEditor(source)})
      // ->Promise.flatMap(textEditor => {
      //     VSCode.Commands.executeCommandRaw("agda-mode.next-goal")
      //     ->Promise.map(() => {Js.log(textEditor->Editor.getText)})
      //   })
      }
    )
    ->Promise.Js.toBsPromise;
  })
});
