// React Hook for listening to a event emitter on mount
let on = (emitter: Event.t('a), handler: 'a => unit) =>
  React.useEffect1(() => Some(emitter.on(handler)), [||]);