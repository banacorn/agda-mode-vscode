// React Hook for request-response handling
let recv = (reqChan: Chan.t<'req>, resChan: Chan.t<'res>, handler: 'req => Promise.t<'res>) =>
  React.useEffect1(
    () => Some(reqChan->Chan.on(req => handler(req)->Promise.get(resChan->Chan.emit))),
    [],
  )

// React Hook for receiving events
let on = (chan: Chan.t<'a>, handler: 'a => unit) =>
  React.useEffect1(() => Some(chan->Chan.on(handler)), [])
