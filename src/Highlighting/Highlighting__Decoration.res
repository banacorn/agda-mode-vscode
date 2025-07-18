// https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
type face =
  | Background(string)
  | Foreground(string)

type t = {
  light: face,
  dark: face,
}

// Returns instances for later destruction
let toDecorations = (input: array<(t, VSCode.Range.t)>): Map.t<
  Editor.Decoration.t,
  array<VSCode.Range.t>,
> => {
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

  let resultDict = Map.make()

  // create decorations with colors stored in the dicts
  backgroundColorDict->Dict.forEachWithKey((ranges, color) => {
    let decoration = Editor.Decoration.createBackgroundWithColor(color)
    switch Map.get(resultDict, decoration) {
    | None => Map.set(resultDict, decoration, ranges)
    | Some(existingRanges) => Map.set(resultDict, decoration, [...existingRanges, ...ranges])
    }
  })
  foregroundColorDict->Dict.forEachWithKey((ranges, color) => {
    let decoration = Editor.Decoration.createTextWithColor(color)
    switch Map.get(resultDict, decoration) {
    | None => Map.set(resultDict, decoration, ranges)
    | Some(existingRanges) => Map.set(resultDict, decoration, [...existingRanges, ...ranges])
    }
  })

  resultDict
}

// Convert Emacs faces to VSCode decorations based on the current color theme (dark or light)
let toDecoration = (self: t): Editor.Decoration.t => {
  let convert = color =>
    switch color {
    | Background(color) => Editor.Decoration.createBackgroundWithColor(color)
    | Foreground(color) => Editor.Decoration.createTextWithColor(color)
    }

  let theme = VSCode.Window.activeColorTheme->VSCode.ColorTheme.kind
  if theme == VSCode.ColorThemeKind.Dark {
    convert(self.dark)
  } else {
    convert(self.light)
  }
}
