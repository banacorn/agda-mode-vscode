type t<'a> = {
  emitter: EventEmitter3.t,
  emit: 'a => unit,
  once: unit => Promise.t<'a>,
  on: ('a => unit, unit) => unit,
  destroy: unit => unit,
}

let make = () => {
  let emitter = EventEmitter3.make()
  {
    emitter: emitter,
    emit: x => emitter |> EventEmitter3.emit("data", x) |> ignore,
    once: () => {
      let (promise, resolve) = Promise.pending()
      emitter |> EventEmitter3.once("data", resolve) |> ignore
      promise
    },
    on: callback => {
      emitter |> EventEmitter3.on("data", callback) |> ignore
      () => emitter |> EventEmitter3.removeListener("data", callback) |> ignore
    },
    destroy: () => EventEmitter3.removeAllListeners(emitter) |> ignore,
  }
}
