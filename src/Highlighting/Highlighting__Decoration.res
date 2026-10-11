// https://www.gnu.org/software/emacs/manual/html_node/emacs/Faces.html
// Each face holds the id of a theme color contributed in package.json ("contributes.colors"),
// so that themes and users can override it with "workbench.colorCustomizations".
type t =
  | Background(string)
  | Foreground(string)
  | BackgroundUnderlined(string)
  | ForegroundUnderlined(string)

// Convert Emacs faces to VSCode decorations
let toDecoration = (self: t): Editor.Decoration.t =>
  switch self {
  | Background(id) => Editor.Decoration.createBackground(id)
  | Foreground(id) => Editor.Decoration.createText(id)
  | BackgroundUnderlined(id) => Editor.Decoration.createBackground(~underline=true, id)
  | ForegroundUnderlined(id) => Editor.Decoration.createText(~underline=true, id)
  }
