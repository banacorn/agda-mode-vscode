// React Hook for receiving requests
let on = (emitter: Event.t('req), handler: 'req => unit) =>
  React.useEffect1(() => {Some(emitter.on(handler))}, [||]);