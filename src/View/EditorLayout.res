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
let resizePair = (layout: t<'group>, ~active: 'group, ~panel: 'group): t<'group> => {
  let isGroup = (child: sized<'group>, group) =>
    switch child.tree {
    | Group(candidate) => candidate == group
    | Split(_, _) => false
    }

  let rec go = (tree: t<'group>): t<'group> =>
    switch tree {
    | Group(_) => tree
    | Split(orientation, children) =>
      let children = children->Array.map(child => {...child, tree: go(child.tree)})

      // the panel sits right after the active group: they share their combined share 70:30
      let children = children->Array.mapWithIndex((child, index) =>
        switch (children[index + 1], children[index - 1]) {
        | (Some(next), _) if isGroup(child, active) && isGroup(next, panel) => {
            ...child,
            size: (child.size +. next.size) *. 0.7,
          }
        | (_, Some(previous)) if isGroup(previous, active) && isGroup(child, panel) => {
            ...child,
            size: (previous.size +. child.size) *. 0.3,
          }
        | _ => child
        }
      )

      // sizes become proportions of the sibling list, rounded to two decimals
      let total = children->Array.reduce(0., (sum, child) => sum +. child.size)
      Split(
        orientation,
        total == 0.
          ? children
          : children->Array.map(child => {
              ...child,
              size: Math.round(child.size /. total *. 100.) /. 100.,
            }),
      )
    }

  go(layout)
}

// The payloads of `vscode.getEditorLayout` and `vscode.setEditorLayout`
type rec rawGroup = {groups?: array<rawGroup>, size?: float}
type rawLayout = {orientation: int, groups: array<rawGroup>}

let orientationOfInt = n => n == 0 ? LeftRight : TopBottom
let intOfOrientation = orientation =>
  switch orientation {
  | LeftRight => 0
  | TopBottom => 1
  }
let flip = orientation =>
  switch orientation {
  | LeftRight => TopBottom
  | TopBottom => LeftRight
  }

// Groups are numbered 1, 2, 3 ... in depth-first order, which is also the
// order of their view columns.
let fromVSCode = (raw: rawLayout): t<int> => {
  let counter = ref(0)
  let rec go = (orientation, groups: array<rawGroup>): t<int> =>
    Split(
      orientation,
      groups->Array.map(group => {
        let tree = switch group.groups {
        | Some(children) if Array.length(children) > 0 => go(flip(orientation), children)
        | _ =>
          counter := counter.contents + 1
          Group(counter.contents)
        }
        {size: group.size->Option.getOr(1.0), tree}
      }),
    )
  go(orientationOfInt(raw.orientation), raw.groups)
}

let toVSCode = (layout: t<'group>): rawLayout => {
  let rec toRawGroup = (sized: sized<'group>): rawGroup =>
    switch sized.tree {
    | Group(_) => {size: sized.size}
    | Split(_, children) => {groups: children->Array.map(toRawGroup), size: sized.size}
    }
  switch layout {
  | Group(_) => {orientation: 0, groups: [{size: 1.0}]}
  | Split(orientation, children) => {
      orientation: intOfOrientation(orientation),
      groups: children->Array.map(toRawGroup),
    }
  }
}
