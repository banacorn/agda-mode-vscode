// Test data for source-location links in the panel: the exact wire-format
// JSON message the extension host would send to display an error message
// that mentions a source location, and the message the panel should send
// back when that location is clicked. Uses the real `RichText.parse` (the
// path that turns locations in Emacs-mode messages into links) and the real
// `View`/`Item` encoders instead of hand-rolling the wire format.

let location = "/tmp/Issue34.agda:8,7-9"

let errorItem: Item.t = Labeled("Error", "error", RichText.parse(location), None, None)

let displayEvent: View.RequestOrEventToView.t = Event(Display(Plain("Error"), [errorItem]))

let json = JSON.stringify(View.RequestOrEventToView.encode(displayEvent))

// The message the panel is expected to post back to the extension host when
// the rendered location is clicked.
let jumpMessage = View.ResponseOrEventFromView.encode(
  Event(JumpToTarget(Link.SrcLoc(Common.AgdaRange.parse(location)->Option.getExn))),
)
