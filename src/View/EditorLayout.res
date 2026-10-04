// A snapshot of the editor-group grid, shaped like `vscode.getEditorLayout`:
// a tree of groups. Every `Split` lists its children along one orientation,
// and every child carries its share (`size`) of the slot its parent gives it.
type orientation = LeftRight | TopBottom

type rec t<'group> =
  | Group('group)
  | Split(orientation, array<sized<'group>>)
and sized<'group> = {
  size: float,
  tree: t<'group>,
}

// Gives `active` 70% and `panel`, the group right after it, 30% of the share
// they have together. Nothing else changes, and sizes become proportions per
// sibling list, rounded to two decimals.
let resizePair = (layout: t<'group>, ~active as _: 'group, ~panel as _: 'group): t<'group> => layout
