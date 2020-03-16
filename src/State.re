open Vscode;
open Belt;

type t = {
  context: Vscode.ExtensionContext.t,
  editor: TextEditor.t,
  mutable connection: option(AgdaMode.Process.t),
  mutable panel: option(WebviewPanel.t),
};

let make = (context, editor) => {
  context,
  editor,
  connection: None,
  panel: None,
};

let dispose = state =>
  state.panel->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);