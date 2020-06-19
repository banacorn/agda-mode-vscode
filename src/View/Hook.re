// React Hook for request-response handling
let on =
    (
      reqEmitter: Event.t('req),
      resEmitter: Event.t('res),
      handler: 'req => Promise.t('res),
    ) =>
  React.useEffect1(
    () => {
      Some(
        reqEmitter.on(req => {handler(req)->Promise.get(resEmitter.emit)}),
      )
    },
    [||],
  );