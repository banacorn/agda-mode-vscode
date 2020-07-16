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

module Impl = (Editor: Sig.Editor) => {
  // shady!
  external makeContext:
    {
      .
      // "extensionPath": string,
      "subscriptions": array(Disposable.t),
    } =>
    Editor.context =
    "%identity";

  module AgdaMode = States.Impl(Editor);

  let openTextEditor = content =>
    Workspace.openTextDocumentWithOptions(
      Some({"content": content, "language": "agda"}),
    )
    ->Promise.flatMap(textDocument =>
        Window.showTextDocumentWithShowOptions(textDocument, None)
      );

  ();

  describe_only("Case split", () => {
    P.it("should load the source", () => {
      openTextEditor(source)
      ->Promise.map(textEditor => {
          let context = makeContext({"subscriptions": [||]});
          AgdaMode.activate(context);
          VSCode.Commands.executeCommandRaw("agda-mode:load")
          ->Promise.get(_ => {Js.log("agda-mode loaded")});
        })
      ->Promise.Js.toBsPromise
    })
  });
};

include Impl(Editor);
