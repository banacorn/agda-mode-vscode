// Builds the exact wire-format JSON message the extension host would send
// to the panel webview to display a goal type such as:
//
//   insertList k v (toList l ++ (k₁ , v₁) ∷ toList r) ≡ _y_90
//
// as reported in #338. Uses the real `View`/`Item`/`RichText` constructors
// and encoders instead of hand-rolling the wire format, so this can't
// silently drift from the actual variant shapes or encoding.
//
// The expression is represented with `RichText.Inline.Horz`, matching how
// the ALS groups a function application for interactive/clickable display
// (see src/View/Component/RichText.res) -- each element of a `Horz` group
// renders as a sibling `.component-horz-item`, which is what triggers the
// Chromium per-flex-item copy quirk in #338.

// One element per `.component-horz-item`, exactly as pasted in the issue.
let tokens = [
  "insertList",
  "k",
  "v",
  "(",
  "toList",
  "l",
  "++",
  "(",
  "k₁",
  ",",
  "v₁",
  ")",
  "∷",
  "toList",
  "r",
  ")",
  "≡",
  "_y_90",
]

let expression: RichText.t = RichText.RichText(
  [RichText.Inline.Horz(tokens->Array.map(token => [RichText.Inline.Text(token, [])]))],
)

let goalTypeItem: Item.t = Labeled("Goal", "special", expression, None, None)

let displayEvent: View.RequestOrEventToView.t = Event(
  Display(Plain("Goal type"), [goalTypeItem]),
)

let json = JSON.stringify(View.RequestOrEventToView.encode(displayEvent))

// The plain-text form the panel is expected to produce when this item is
// copied -- tokens joined by single spaces, matching what's shown on screen.
// (Doesn't reproduce Agda's actual paren spacing -- out of scope for #338,
// which is about stray newlines, not spacing.)
let expectedCopyText = tokens->Array.join(" ")
