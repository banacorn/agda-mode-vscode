open Vscode;

Js.log("Hello, BuckleScript and Reason!");

let activate = (context: ExtensionContext.t) => {
    let disposable = Commands.registerCommand("extension.helloWorld", () => {
		Window.showInformationMessage("Hello World!!!!!");
	});
};

let deactive = () => ();