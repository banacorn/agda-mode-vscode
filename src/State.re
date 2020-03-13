open Vscode;
open Belt;

type t = {
  editor: TextEditor.t,
  mutable connection: option(AgdaMode.Process.t),
  mutable panel: option(WebviewPanel.t),
};

let make = editor => {editor, connection: None, panel: None};

let dispose = state =>
  state.panel->Option.forEach(panel => panel->WebviewPanel.dispose->ignore);