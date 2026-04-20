type t = {
  log: Chan.t<Log.t>,
  quickPick: VSCode.QuickPick.t<Connection__UI__Item.t>,
  subscriptions: array<VSCode.Disposable.t>,
  mutable items: array<Connection__UI__Item.t>,
  mutable pendingHides: int,
}

let make = (log): t => {
  let quickPick: VSCode.QuickPick.t<Connection__UI__Item.t> = VSCode.Window.createQuickPick()
  {
    log,
    quickPick,
    subscriptions: [],
    items: [],
    pendingHides: 0,
  }
}

let setPlaceholder = (self: t, placeholder: string): unit => {
  self.quickPick->VSCode.QuickPick.setPlaceholder(placeholder)
}

let updateItems = (self: t, items: array<Connection__UI__Item.t>): unit => {
  self.items = items
  self.quickPick->VSCode.QuickPick.setItems(items)
}

let show = (self: t): unit => {
  self.quickPick->VSCode.QuickPick.show
}

let onSelection = (self: t, handler: array<Connection__UI__Item.t> => unit): unit => {
  self.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(handler)
  ->Util.Disposable.add(self.subscriptions)
}

let onHide = (self: t, handler: unit => unit): unit => {
  self.quickPick
  ->VSCode.QuickPick.onDidHide(handler)
  ->Util.Disposable.add(self.subscriptions)
}

let destroy = (self: t): unit => {
  self.quickPick->VSCode.QuickPick.dispose
  self.subscriptions->Array.forEach(sub => sub->VSCode.Disposable.dispose)
  self.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.Destroyed))
}
