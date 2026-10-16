// Builds the exact wire-format JSON message the extension host would send
// to the panel webview to display an error message that mentions a source
// location, as reported in #34. Uses the real `RichText.parse` (the path
// that turns locations in Emacs-mode messages into links) and the real
// `View`/`Item` encoders instead of hand-rolling the wire format.

let location = "/tmp/Issue34.agda:8,7-9"

let errorItem: Item.t = Labeled("Error", "error", RichText.parse(location), None, None)

let displayEvent: View.RequestOrEventToView.t = Event(Display(Plain("Error"), [errorItem]))

let json = JSON.stringify(View.RequestOrEventToView.encode(displayEvent))
