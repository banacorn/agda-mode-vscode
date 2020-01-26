open Vscode;

Js.log("Hello, BuckleScript and Reason!");

let activate = (context: ExtensionContext.t) => {
  let disposable =
    Commands.registerCommand("extension.helloWorld", () => {
      Window.showInformationMessage("Hello World!!!!!")
    });
  Js.Array.push(disposable, context.subscriptions) |> ignore;
};

let deactive = () => ();