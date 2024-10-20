module EventEmitter3 = {
  // Types

  type t

  type eventName = string
  type listener<'a> = 'a => unit
  type listener2<'a, 'b> = ('a, 'b) => unit
  type listener3<'a, 'b, 'c> = ('a, 'b, 'c) => unit

  // Class: EventEmitter

  @module("eventemitter3") @new
  external make: unit => t = "EventEmitter"

  // Event: 'newListener'

  @send
  external onNewListener: (t, string, listener<'a>) => t = "on"
  let onNewListener = (arg1, obj) => obj->onNewListener("newListener", arg1)

  @send
  external onNewListener2: (t, string, listener2<'a, 'b>) => t = "on"
  let onNewListener2 = (arg1, obj) => obj->onNewListener2("newListener", arg1)

  @send
  external onNewListener3: (t, string, listener3<'a, 'b, 'c>) => t = "on"
  let onNewListener3 = (arg1, obj) => obj->onNewListener3("newListener", arg1)

  @send
  external onceNewListener: (t, string, listener<'a>) => t = "once"
  let onceNewListener = (arg1, obj) => obj->onceNewListener("newListener", arg1)

  @send
  external onceNewListener2: (t, string, listener2<'a, 'b>) => t = "once"
  let onceNewListener2 = (arg1, obj) => obj->onceNewListener2("newListener", arg1)

  @send
  external onceNewListener3: (t, string, listener3<'a, 'b, 'c>) => t = "once"
  let onceNewListener3 = (arg1, obj) => obj->onceNewListener3("newListener", arg1)

  @send
  external onRemoveListener: (t, string, listener<unit>) => t = "on"
  let onRemoveListener = (arg1, obj) => obj->onRemoveListener("removeListener", arg1)

  @send
  external onceRemoveListener: (t, string, listener<unit>) => t = "once"
  let onceRemoveListener = (arg1, obj) => obj->onceRemoveListener("removeListener", arg1)

  @val @scope("EventEmitter")
  external defaultMaxListeners: int = "defaultMaxListeners"

  @send
  external addListener: (t, eventName, listener<'a>) => t = "addListener"
  let addListener = (arg1, arg2, obj) => obj->addListener(arg1, arg2)

  @send
  external addListener2: (t, eventName, listener2<'a, 'b>) => t = "addListener"
  let addListener2 = (arg1, arg2, obj) => obj->addListener2(arg1, arg2)

  @send
  external addListener3: (t, eventName, listener3<'a, 'b, 'c>) => t = "addListener"
  let addListener3 = (arg1, arg2, obj) => obj->addListener3(arg1, arg2)

  @send
  external emit0: (t, eventName) => bool = "emit"
  let emit0 = (arg1, obj) => obj->emit0(arg1)

  @send
  external emit: (t, eventName, 'a) => bool = "emit"
  let emit = (arg1, arg2, obj) => obj->emit(arg1, arg2)

  @send
  external emit2: (t, eventName, 'a, 'b) => bool = "emit"
  let emit2 = (arg1, arg2, arg3, obj) => obj->emit2(arg1, arg2, arg3)

  @send
  external emit3: (t, eventName, 'a, 'b, 'c) => bool = "emit"
  let emit3 = (arg1, arg2, arg3, arg4, obj) => obj->emit3(arg1, arg2, arg3, arg4)

  @send
  external eventNames: (t, eventName) => array<eventName> = "eventNames"
  let eventNames = (arg1, obj) => obj->eventNames(arg1)

  @send
  external listenerCount: (t, eventName) => int = "listenerCount"
  let listenerCount = (arg1, obj) => obj->listenerCount(arg1)

  @send
  external listeners: (t, eventName) => array<listener<'a>> = "listeners"
  let listeners = (arg1, obj) => obj->listeners(arg1)

  @send
  external listeners2: (t, eventName) => array<listener2<'a, 'b>> = "listeners"
  let listeners2 = (arg1, obj) => obj->listeners2(arg1)

  @send
  external listeners3: (t, eventName) => array<listener3<'a, 'b, 'c>> = "listeners"
  let listeners3 = (arg1, obj) => obj->listeners3(arg1)

  @send
  external off: (t, eventName, listener<'a>) => t = "off"
  let off = (arg1, arg2, obj) => obj->off(arg1, arg2)

  @send
  external off2: (t, eventName, listener2<'a, 'b>) => t = "off"
  let off2 = (arg1, arg2, obj) => obj->off2(arg1, arg2)

  @send
  external off3: (t, eventName, listener3<'a, 'b, 'c>) => t = "off"
  let off3 = (arg1, arg2, obj) => obj->off3(arg1, arg2)

  let on = addListener
  let on2 = addListener2
  let on3 = addListener3

  @send
  external once: (t, eventName, listener<'a>) => t = "once"
  let once = (arg1, arg2, obj) => obj->once(arg1, arg2)

  @send
  external once2: (t, eventName, listener2<'a, 'b>) => t = "once"
  let once2 = (arg1, arg2, obj) => obj->once2(arg1, arg2)

  @send
  external once3: (t, eventName, listener3<'a, 'b, 'c>) => t = "once"
  let once3 = (arg1, arg2, obj) => obj->once3(arg1, arg2)

  @send
  external prependListener: (t, array<eventName>, listener<'a>) => t = "prependListener"
  let prependListener = (arg1, arg2, obj) => obj->prependListener(arg1, arg2)

  @send
  external prependListener2: (t, array<eventName>, listener2<'a, 'b>) => t = "prependListener"
  let prependListener2 = (arg1, arg2, obj) => obj->prependListener2(arg1, arg2)

  @send
  external prependListener3: (t, array<eventName>, listener3<'a, 'b, 'c>) => t = "prependListener"
  let prependListener3 = (arg1, arg2, obj) => obj->prependListener3(arg1, arg2)

  @send
  external prependOnceListener: (t, eventName, listener<'a>) => t = "prependOnceListener"
  let prependOnceListener = (arg1, arg2, obj) => obj->prependOnceListener(arg1, arg2)

  @send
  external prependOnceListener2: (t, eventName, listener2<'a, 'b>) => t = "prependOnceListener"
  let prependOnceListener2 = (arg1, arg2, obj) => obj->prependOnceListener2(arg1, arg2)

  @send
  external prependOnceListener3: (t, eventName, listener3<'a, 'b, 'c>) => t = "prependOnceListener"
  let prependOnceListener3 = (arg1, arg2, obj) => obj->prependOnceListener3(arg1, arg2)

  @send
  external removeAllListeners: t => t = "removeAllListeners"

  @send
  external removeAllListeners_: (t, array<eventName>) => t = "removeAllListeners"
  let removeAllListeners_ = (arg1, obj) => obj->removeAllListeners_(arg1)

  let removeListener = off
  let removeListener2 = off2
  let removeListener3 = off3

  @send
  external setMaxListeners: (t, int) => t = "setMaxListeners"
  let setMaxListeners = (arg1, obj) => obj->setMaxListeners(arg1)

  @send
  external rawListeners: (t, eventName) => array<listener<'a>> = "rawListeners"
  let rawListeners = (arg1, obj) => obj->rawListeners(arg1)

  @send
  external rawListeners2: (t, eventName) => array<listener2<'a, 'b>> = "rawListeners"
  let rawListeners2 = (arg1, obj) => obj->rawListeners2(arg1)

  @send
  external rawListeners3: (t, eventName) => array<listener3<'a, 'b, 'c>> = "rawListeners"
  let rawListeners3 = (arg1, obj) => obj->rawListeners3(arg1)

  // events.once(emitter, name)

  @module("eventemitter3")
  external oncePromise: (t, string) => Js.Promise.t<'a> = "once"

  @module("eventemitter3")
  external oncePromise2: (t, string) => Js.Promise.t<('a, 'b)> = "once"

  @module("eventemitter3")
  external oncePromise3: (t, string) => Js.Promise.t<('a, 'b, 'c)> = "once"
}

module Module: {
  type t<'a>
  let make: unit => t<'a>
  let emit: (t<'a>, 'a) => unit
  let on: (t<'a>, 'a => unit, unit) => unit
  let once: t<'a> => Promise.t<'a>
  let destroy: t<'a> => unit
} = {
  type t<'a> = EventEmitter3.t
  let make = EventEmitter3.make
  let emit = (self, x) => self |> EventEmitter3.emit("data", x) |> ignore
  let on = (self, callback) => {
    self |> EventEmitter3.on("data", callback) |> ignore
    () => self |> EventEmitter3.removeListener("data", callback) |> ignore
  }
  let once = self => {
    let (promise, resolve) = Promise.pending()
    self |> EventEmitter3.once("data", resolve) |> ignore
    promise
  }
  let destroy = self => self |> EventEmitter3.removeAllListeners |> ignore
}

include Module
