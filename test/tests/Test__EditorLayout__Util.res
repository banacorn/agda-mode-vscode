// Helpers for writing and printing `EditorLayout.t<string>` in tests.
// Leaves are named by plain strings.

// A group taking `size` of its parent's slot. Sizes are written as percentages
// (or as pixels, as `getEditorLayout` reports them) and stored as `size / 100`.
let leaf = (size, name): EditorLayout.sized<string> => {size: size /. 100.0, tree: Group(name)}

// a nested split taking `size` of its parent's slot
let nested = (size, tree): EditorLayout.sized<string> => {size: size /. 100.0, tree}

// children side by side (left/right)
let row = (children): EditorLayout.t<string> => Split(LeftRight, children)

// children stacked (top/bottom)
let col = (children): EditorLayout.t<string> => Split(TopBottom, children)

// Compact printing, e.g. `[A(50) | [B(70) / Agda(30)](50)]`:
// `|` separates left/right siblings, `/` separates top/bottom siblings, and
// the number is the sibling's share of its parent in percent.
let rec show = (tree: EditorLayout.t<string>): string =>
  switch tree {
  | Group(name) => name
  | Split(orientation, children) =>
    let separator = switch orientation {
    | LeftRight => " | "
    | TopBottom => " / "
    }
    "[" ++ children->Array.map(showSized)->Array.join(separator) ++ "]"
  }
and showSized = ({size, tree}: EditorLayout.sized<string>): string =>
  show(tree) ++ "(" ++ Float.toString(Math.round(size *. 100.0)) ++ ")"
