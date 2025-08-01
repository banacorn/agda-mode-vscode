// Simplified version switching module without connection endpoints
// Just shows a dummy quickpick item

module QP = {
  type t = {
    quickPick: VSCode.QuickPick.t<VSCode.QuickPickItem.t>,
    subscriptions: array<VSCode.Disposable.t>,
  }

  let make = () => {
    quickPick: VSCode.Window.createQuickPick(),
    subscriptions: [],
  }

  let render = self => {
    self.quickPick->VSCode.QuickPick.show
  }

  let destroy = self => {
    self.quickPick->VSCode.QuickPick.dispose
    self.subscriptions->Array.forEach(sub => sub->VSCode.Disposable.dispose)
  }
}

let run = async (_state: State.t, _platformDeps: Platform.t) => {
  let qp = QP.make()
  
  // Set placeholder
  qp.quickPick->VSCode.QuickPick.setPlaceholder("Switch Version (Dummy)")
  
  // Create a dummy item
  let dummyItem: VSCode.QuickPickItem.t = {
    label: "$(star) Dummy Version Switcher",
    description: "This is a placeholder implementation",
    detail: "No actual functionality yet",
  }
  
  // Set items
  qp.quickPick->VSCode.QuickPick.setItems([dummyItem])
  
  // Show the quickpick
  qp->QP.render
  
  // Handle selection - just close on any selection
  qp.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(_selectedItems => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)
  
  // Handle hide event
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)
}