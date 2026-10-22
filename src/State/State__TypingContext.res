// agdaModeTyping binds the keys of the input method, and VS Code has only one of it for all editors,
// so it is set from the input method of the active editor, read at the moment it is set:
// a late change in another editor then sets the value of the active editor, not its own (#78)
let sync = async (log: Chan.t<Log.t>) => {
  let activated = switch VSCode.Window.activeTextEditor {
  | None => false
  | Some(editor) =>
    switch Registry.get(editor->VSCode.TextEditor.document)->Option.flatMap(entry => entry.state) {
    | None => false
    | Some(state) => IM.isActivated(state.editorIM)
    }
  }
  await State.Context.setIM(log, activated)
}
