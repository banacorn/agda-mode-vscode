include View__Type
open Belt 


open ViewController
module type Panel = {
  // methods
  let make: string => PanelController.t
  let destroy: unit => unit
}

module Panel: Panel = {
  let handle: ref<option<PanelController.t>> = ref(None)
  let make = extensionPath =>
    switch handle.contents {
    | None =>
      let panel = PanelController.make(extensionPath)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      PanelController.onceDestroyed(panel)->Promise.get(() => {
        handle := None
      })
      panel
    | Some(panel) => panel
    }

  let destroy = () => {
    handle.contents->Option.forEach(PanelController.destroy)
    handle := None
  }
}
