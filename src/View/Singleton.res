open Belt

module Panel: {
  // methods
  let make: string => WebviewPanel.t
  let destroy: unit => unit
} = {
  let handle: ref<option<WebviewPanel.t>> = ref(None)
  let make = extensionPath =>
    switch handle.contents {
    | None =>
      let panel = WebviewPanel.make("Agda", extensionPath)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      WebviewPanel.onceDestroyed(panel)->Promise.finally(() => {
        handle := None
      })->Promise.done
      panel
    | Some(panel) => panel
    }

  let destroy = () => {
    handle.contents->Option.forEach(WebviewPanel.destroy)
    handle := None
  }
}

module DebugBuffer: {
  let get: unit => option<WebviewPanel.t>
  let make: string => WebviewPanel.t
  let destroy: unit => unit
} = {
  let handle: ref<option<WebviewPanel.t>> = ref(None)
  let get = () => handle.contents
  let make = extensionPath =>
    switch handle.contents {
    | None =>
      let panel = WebviewPanel.make("Agda Debug Buffer", extensionPath)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      WebviewPanel.onceDestroyed(panel)->Promise.finally(() => {
        handle := None
      })->Promise.done
      panel
    | Some(panel) => panel
    }

  let destroy = () => {
    handle.contents->Option.forEach(WebviewPanel.destroy)
    handle := None
  }
}
