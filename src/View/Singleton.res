module Panel: {
  // methods
  let make: VSCode.Uri.t => WebviewPanel.t
  let destroy: unit => unit
  let get: unit => option<WebviewPanel.t>
} = {
  let handle: ref<option<WebviewPanel.t>> = ref(None)
  let make = extensionUri =>
    switch handle.contents {
    | None =>
      let panel = WebviewPanel.make("Agda", extensionUri)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      WebviewPanel.onceDestroyed(panel)
      ->Promise.finally(() => {
        handle := None
      })
      ->Promise.done
      panel
    | Some(panel) => panel
    }

  let get = () => handle.contents

  let destroy = () => {
    handle.contents->Option.forEach(WebviewPanel.destroy)
    handle := None
  }
}

module DebugBuffer: {
  let get: unit => option<WebviewPanel.t>
  let make: VSCode.Uri.t => WebviewPanel.t
  let destroy: unit => unit
} = {
  let handle: ref<option<WebviewPanel.t>> = ref(None)
  let get = () => handle.contents
  let make = extensionUri =>
    switch handle.contents {
    | None =>
      let panel = WebviewPanel.make("Agda Debug Buffer", extensionUri)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      WebviewPanel.onceDestroyed(panel)
      ->Promise.finally(() => {
        handle := None
      })
      ->Promise.done
      panel
    | Some(panel) => panel
    }

  let destroy = () => {
    handle.contents->Option.forEach(WebviewPanel.destroy)
    handle := None
  }
}
