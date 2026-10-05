// Opens the Agda panel in a group of its own, right after the editor's group.
// VS Code decides where the new group goes and how big it is, the same as
// when the user splits the editor. Only for `bottom`, the editor's group and
// the panel's group then share their combined space 70:30. Nothing else is
// resized, and focus goes back to the editor.

// the panel is being placed: concurrent commands wait for it instead of
// making a second group
let pending: ref<option<promise<unit>>> = ref(None)

let columnOf = (editor: VSCode.TextEditor.t): option<int> =>
  editor->VSCode.TextEditor.viewColumn->Option.map(column => Obj.magic(column))

let resizeForBottom = async (~editorColumn) => {
  let raw: EditorLayout.rawLayout = await VSCode.Commands.executeCommand0("vscode.getEditorLayout")
  let layout = EditorLayout.fromVSCode(raw)
  // the panel's group comes right after the editor's group
  let resized = layout->EditorLayout.resizePair(~active=editorColumn, ~panel=editorColumn + 1)
  let _ = await VSCode.Commands.executeCommand1("vscode.setEditorLayout", EditorLayout.toVSCode(resized))
}

let place = async (editor: VSCode.TextEditor.t, extensionUri) => {
  let position = Config.View.getPanelMountingPosition()
  // the new group becomes the active one
  let _ = await VSCode.Commands.executeCommand0(
    switch position {
    | Bottom => "workbench.action.newGroupBelow"
    | Right => "workbench.action.newGroupRight"
    },
  )
  let _ = Singleton.Panel.makeInActiveGroup(extensionUri)

  switch (columnOf(editor), position) {
  | (Some(editorColumn), Bottom) => await resizeForBottom(~editorColumn)
  | _ => ()
  }

  // back to the editor
  let _ = await VSCode.Window.showTextDocument(
    editor->VSCode.TextEditor.document,
    ~column=?(editor->VSCode.TextEditor.viewColumn),
    ~preserveFocus=false,
    (),
  )
}

let ensure = async (editor, extensionUri) =>
  switch (Singleton.Panel.get(), pending.contents) {
  | (Some(_), _) => ()
  | (None, Some(promise)) => await promise
  | (None, None) =>
    let promise = place(editor, extensionUri)
    pending := Some(promise)
    try {
      await promise
      pending := None
    } catch {
    | exn =>
      pending := None
      raise(exn)
    }
  }
