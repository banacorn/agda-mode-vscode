open Belt

// https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
type face =
  | Background(string)
  | Foreground(string)

type t = {
  light: face,
  dark: face,
}

let fromAspect = (x: Tokens.Aspect.t): option<t> =>
  switch x {
  | Comment =>
    Some({
      light: Foreground("#B0B0B0"),
      dark: Foreground("#505050"),
    })
  | Keyword =>
    Some({
      light: Foreground("#CD6600"),
      dark: Foreground("#FF9932"),
    })
  | String =>
    Some({
      light: Foreground("#B22222"),
      dark: Foreground("#DD4D4D"),
    })
  | Number =>
    Some({
      light: Foreground("#800080"),
      dark: Foreground("#9010E0"),
    })
  | Symbol =>
    Some({
      light: Foreground("#404040"),
      dark: Foreground("#BFBFBF"),
    })
  | PrimitiveType =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Pragma => None
  | Background => None
  | Markup => None
  | Error =>
    Some({
      light: Foreground("#FF0000"),
      dark: Foreground("#FF0000"),
    })
  | DottedPattern => None
  | UnsolvedMeta =>
    Some({
      light: Background("#FFFF00"),
      dark: Background("#806B00"),
    })
  | UnsolvedConstraint =>
    Some({
      light: Background("#FFFF00"),
      dark: Background("#806B00"),
    })
  | TerminationProblem =>
    Some({
      light: Background("#FFA07A"),
      dark: Background("#802400"),
    })
  | PositivityProblem =>
    Some({
      light: Background("#CD853F"),
      dark: Background("#803F00"),
    })
  | Deadcode =>
    Some({
      light: Background("#A9A9A9"),
      dark: Background("#808080"),
    })
  | CoverageProblem =>
    Some({
      light: Background("#F5DEB3"),
      dark: Background("#805300"),
    })
  | IncompletePattern =>
    Some({
      light: Background("#800080"),
      dark: Background("#800080"),
    })
  | TypeChecks => None
  | CatchallClause =>
    Some({
      light: Background("#F5F5F5"),
      dark: Background("#404040"),
    })
  | ConfluenceProblem =>
    Some({
      light: Background("#FFC0CB"),
      dark: Background("#800080"),
    })
  | Bound => None
  | Generalizable => None
  | ConstructorInductive =>
    Some({
      light: Foreground("#008B00"),
      dark: Foreground("#29CC29"),
    })
  | ConstructorCoInductive =>
    Some({
      light: Foreground("#996600"),
      dark: Foreground("#FFEA75"),
    })
  | Datatype =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Field =>
    Some({
      light: Foreground("#EE1289"),
      dark: Foreground("#F570B7"),
    })
  | Function =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Module =>
    Some({
      light: Foreground("#800080"),
      dark: Foreground("#CD80FF"),
    })
  | Postulate =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Primitive =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Record =>
    Some({
      light: Foreground("#0000CD"),
      dark: Foreground("#8080FF"),
    })
  | Argument => None
  | Macro =>
    Some({
      light: Foreground("#458B74"),
      dark: Foreground("#73BAA2"),
    })
  | Operator => None
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
