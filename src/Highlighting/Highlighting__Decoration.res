// https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
type face =
  | Background(string)
  | Foreground(string)

type t = {
  light: face,
  dark: face,
}

// Returns instances for later destruction
let apply = (input: array<(t, VSCode.Range.t)>, editor: VSCode.TextEditor.t): array<(
  Editor.Decoration.t,
  array<VSCode.Range.t>,
)> => {
  // dictionaries of color-ranges mapping
  // speed things up by aggregating decorations of the same kind
  let backgroundColorDict: Dict.t<array<VSCode.Range.t>> = Dict.make()
  let foregroundColorDict: Dict.t<array<VSCode.Range.t>> = Dict.make()

  let addFaceToDict = (face: face, range) =>
    switch face {
    | Background(color) =>
      switch Dict.get(backgroundColorDict, color) {
      | None => Dict.set(backgroundColorDict, color, [range])
      | Some(ranges) => ranges->Array.push(range)
      }
    | Foreground(color) =>
      switch Dict.get(foregroundColorDict, color) {
      | None => Dict.set(foregroundColorDict, color, [range])
      | Some(ranges) => ranges->Array.push(range)
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
    Dict.toArray(backgroundColorDict)->Array.map(((color, ranges)) => (
      Editor.Decoration.highlightBackgroundWithColor(editor, color, ranges),
      ranges,
    ))
  let foregroundDecorations =
    Dict.toArray(foregroundColorDict)->Array.map(((color, ranges)) => (
      Editor.Decoration.decorateTextWithColor(editor, color, ranges),
      ranges,
    ))
  // return decorations
  Array.concat(foregroundDecorations, backgroundDecorations)
}
