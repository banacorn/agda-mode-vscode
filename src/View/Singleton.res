open Belt 

module Panel: {
  // methods
  let get: string => WebviewPanel.t
  let destroy: unit => unit
} = {
  let handle: ref<option<WebviewPanel.t>> = ref(None)
  let get = extensionPath =>
    switch handle.contents {
    | None =>
      let panel = WebviewPanel.make("Agda", extensionPath)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      WebviewPanel.onceDestroyed(panel)->Promise.get(() => {
        handle := None
      })
      panel
    | Some(panel) => panel
    }

  let destroy = () => {
    handle.contents->Option.forEach(WebviewPanel.destroy)
    handle := None
  }
}

module DebugBuffer: {
  // methods
  let get: string => WebviewPanel.t
  let destroy: unit => unit
} = {
  let handle: ref<option<WebviewPanel.t>> = ref(None)
  let get = extensionPath =>
    switch handle.contents {
    | None =>
      let panel = WebviewPanel.make("Agda Debug Buffer", extensionPath)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      WebviewPanel.onceDestroyed(panel)->Promise.get(() => {
        handle := None
      })
      panel
    | Some(panel) => panel
    }

  let destroy = () => {
    handle.contents->Option.forEach(WebviewPanel.destroy)
    handle := None
  }
}
