// React Hook for request-response handling
let recv = (reqEmitter: Chan.t<'req>, resEmitter: Chan.t<'res>, handler: 'req => Promise.t<'res>) =>
  React.useEffect1(
    () => Some(reqEmitter->Chan.on(req => handler(req)->Promise.get(resEmitter->Chan.emit))),
    [],
  )

// React Hook for receiving events
let on = (emitter: Chan.t<'a>, handler: 'a => unit) =>
  React.useEffect1(() => Some(emitter->Chan.on(handler)), [])
