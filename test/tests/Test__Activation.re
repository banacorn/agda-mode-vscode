open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;
open! Belt;

let source = {j|
data ℕ : Set where
  Z : ℕ
  S : ℕ → ℕ
|j};

// shady!
external makeContext:
  {
    .
    // "extensionPath": string,
    "subscriptions": array(Disposable.t),
  } =>
  Editor.context =
  "%identity";

// let context = makeContext({"subscriptions": [||]});
// AgdaMode.activate(context);

module AgdaMode = States.Impl(Editor);

let openTextEditor = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );

();

describe("Extention Activation", () => {
  it("should be able to acquire \"banacorn.agda-mode\"", () => {
    let extension = VSCode.Extensions.getExtension("banacorn.agda-mode");
    Assert.equal(
      extension->Option.isSome,
      true,
      ~message="extension shouldn't be undefined",
    );
  })
});
