// React Hook for request-response handling
let recv = (
  reqEmitter: Event.t<'req>,
  resEmitter: Event.t<'res>,
  handler: 'req => Promise.t<'res>,
) =>
  React.useEffect1(() => Some(reqEmitter.on(req => handler(req)->Promise.get(resEmitter.emit))), [])

// React Hook for receiving events
let on = (emitter: Event.t<'a>, handler: 'a => unit) =>
  React.useEffect1(() => Some(emitter.on(handler)), [])
