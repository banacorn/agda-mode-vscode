open Belt

// https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
type face =
  | Background(string)
  | Foreground(string)

type t = {
  light: face,
  dark: face,
}

let toVSCodeDecorations = (input: array<(t, VSCode.Range.t)>, editor: VSCode.TextEditor.t): array<(
  Editor.Decoration.t,
  array<VSCode.Range.t>,
)> => {
  // dictionaries of color-ranges mapping
  // speed things up by aggregating decorations of the same kind
  let backgroundColorDict: Js.Dict.t<array<VSCode.Range.t>> = Js.Dict.empty()
  let foregroundColorDict: Js.Dict.t<array<VSCode.Range.t>> = Js.Dict.empty()

  let addFaceToDict = (face: face, range) =>
    switch face {
    | Background(color) =>
      switch Js.Dict.get(backgroundColorDict, color) {
      | None => Js.Dict.set(backgroundColorDict, color, [range])
      | Some(ranges) => Js.Array.push(range, ranges)->ignore
      }
    | Foreground(color) =>
      switch Js.Dict.get(foregroundColorDict, color) {
      | None => Js.Dict.set(foregroundColorDict, color, [range])
      | Some(ranges) => Js.Array.push(range, ranges)->ignore
      }
    }

  // convert Aspects to colors and collect them in the dict
  input->Array.forEach((({light, dark}, range)) => {
    let theme = VSCode.Window.activeColorTheme->VSCode.ColorTheme.kind
    if theme == VSCode.ColorThemeKind.Dark {
      addFaceToDict(dark, range)
    } else {
      addFaceToDict(light, range)
    }
  })

  // decorate with colors stored in the dicts
  let backgroundDecorations =
    Js.Dict.entries(backgroundColorDict)->Array.map(((color, ranges)) => (
      Editor.Decoration.highlightBackgroundWithColor(editor, color, ranges),
      ranges,
    ))
  let foregroundDecorations =
    Js.Dict.entries(foregroundColorDict)->Array.map(((color, ranges)) => (
      Editor.Decoration.decorateTextWithColor(editor, color, ranges),
      ranges,
    ))
  // return decorations
  Js.Array.concat(backgroundDecorations, foregroundDecorations)
}
