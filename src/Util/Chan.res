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
  external onNewListener: (t, @as("newListener") _, listener2<eventName, listener<'a>>) => t = "on"

  @send
  external onNewListener2: (t, @as("newListener") _, listener2<eventName, listener2<'a, 'b>>) => t =
    "on"

  @send
  external onNewListener3: (
    t,
    @as("newListener") _,
    listener2<eventName, listener3<'a, 'b, 'c>>,
  ) => t = "on"

  @send
  external onceNewListener: (t, @as("newListener") _, listener2<eventName, listener<'a>>) => t =
    "once"

  @send
  external onceNewListener2: (
    t,
    @as("newListener") _,
    listener2<eventName, listener2<'a, 'b>>,
  ) => t = "once"

  @send
  external onceNewListener3: (
    t,
    @as("newListener") _,
    listener2<eventName, listener3<'a, 'b, 'c>>,
  ) => t = "once"

  @send
  external onRemoveListener: (t, string, listener<unit>) => t = "on"
  let onRemoveListener = (arg1, obj) => obj->onRemoveListener("removeListener", arg1)

  @send
  external onRemoveListener: (t, @as("removeListener") _, listener<unit>) => t = "on"

  @send
  external onceRemoveListener: (t, @as("removeListener") _, listener<unit>) => t = "once"

  // EventEmitter.defaultMaxListeners

  @val @scope("EventEmitter")
  external defaultMaxListeners: int = "defaultMaxListeners"

  // emitter.addListener(eventName, listener)

  @send
  external addListener: (t, eventName, listener<'a>) => t = "addListener"

  @send
  external addListener2: (t, eventName, listener2<'a, 'b>) => t = "addListener"

  @send
  external addListener3: (t, eventName, listener3<'a, 'b, 'c>) => t = "addListener"

  // emitter.emit(eventName[, ...args])

  @send external emit0: (t, eventName) => bool = "emit"

  @send external emit: (t, eventName, 'a) => bool = "emit"

  @send external emit2: (t, eventName, 'a, 'b) => bool = "emit"

  @send external emit3: (t, eventName, 'a, 'b, 'c) => bool = "emit"

  @send
  external emit0: (t, eventName) => bool = "emit"
  let emit0 = (arg1, obj) => obj->emit0(arg1)

  @send
  external eventNames: (t, eventName) => array<eventName> = "eventNames"

  @send
  external emit2: (t, eventName, 'a, 'b) => bool = "emit"
  let emit2 = (arg1, arg2, arg3, obj) => obj->emit2(arg1, arg2, arg3)

  @send external getMaxListeners: t => int = "getMaxListeners"

  @send
  external eventNames: (t, eventName) => array<eventName> = "eventNames"
  let eventNames = (arg1, obj) => obj->eventNames(arg1)

  @send
  external listenerCount: (t, eventName) => int = "listenerCount"

  @send
  external listeners: (t, eventName) => array<listener<'a>> = "listeners"
  let listeners = (arg1, obj) => obj->listeners(arg1)

  @send
  external listeners: (t, eventName) => array<listener<'a>> = "listeners"

  @send
  external listeners2: (t, eventName) => array<listener2<'a, 'b>> = "listeners"

  @send
  external listeners3: (t, eventName) => array<listener3<'a, 'b, 'c>> = "listeners"

  @send
  external off2: (t, eventName, listener2<'a, 'b>) => t = "off"
  let off2 = (arg1, arg2, obj) => obj->off2(arg1, arg2)

  @send external off: (t, eventName, listener<'a>) => t = "off"
  @send
  external off2: (t, eventName, listener2<'a, 'b>) => t = "off"
  @send
  external off3: (t, eventName, listener3<'a, 'b, 'c>) => t = "off"

  // emitter.on(eventName, listener)

  let on = addListener
  let on2 = addListener2
  let on3 = addListener3

  // emitter.once(eventName, listener)

  @send external once: (t, eventName, listener<'a>) => t = "once"

  @send
  external once2: (t, eventName, listener2<'a, 'b>) => t = "once"

  @send
  external once3: (t, eventName, listener3<'a, 'b, 'c>) => t = "once"

  @send
  external once2: (t, eventName, listener2<'a, 'b>) => t = "once"
  let once2 = (arg1, arg2, obj) => obj->once2(arg1, arg2)

  @send
  external prependListener: (t, eventName, listener<'a>) => t = "prependListener"

  @send
  external prependListener2: (t, eventName, listener2<'a, 'b>) => t = "prependListener"

  @send
  external prependListener3: (t, eventName, listener3<'a, 'b, 'c>) => t = "prependListener"

  @send
  external prependListener3: (t, array<eventName>, listener3<'a, 'b, 'c>) => t = "prependListener"

  @send
  external prependOnceListener: (t, eventName, listener<'a>) => t = "prependOnceListener"

  @send
  external prependOnceListener2: (t, eventName, listener2<'a, 'b>) => t = "prependOnceListener"

  @send
  external prependOnceListener3: (t, eventName, listener3<'a, 'b, 'c>) => t = "prependOnceListener"

  @send
  external removeAllListeners: t => t = "removeAllListeners"

  @send external removeAllListeners: t => unit = "removeAllListeners"

  @send
  external removeAllListeners_: (t, array<eventName>) => t = "removeAllListeners"

  // emitter.removeListener(eventName, listener)

  let removeListener = off
  let removeListener2 = off2
  let removeListener3 = off3

  // emitter.setMaxListeners(n)

  @send external setMaxListeners: (t, int) => t = "setMaxListeners"

  // emitter.rawListeners(eventName)

  @send
  external rawListeners: (t, eventName) => array<listener<'a>> = "rawListeners"

  @send
  external rawListeners2: (t, eventName) => array<listener2<'a, 'b>> = "rawListeners"

  @send
  external rawListeners3: (t, eventName) => array<listener3<'a, 'b, 'c>> = "rawListeners"

  // events.once(emitter, name)

  @module("eventemitter3")
  external oncePromise: (t, string) => promise<'a> = "once"

  @module("eventemitter3")
  external oncePromise2: (t, string) => promise<('a, 'b)> = "once"

  @module("eventemitter3")
  external oncePromise3: (t, string) => promise<('a, 'b, 'c)> = "once"
}

module Module: {
  type t<'a>
  let make: unit => t<'a>
  let emit: (t<'a>, 'a) => unit
  let on: (t<'a>, 'a => unit) => unit => unit
  let once: t<'a> => promise<'a>
  let pipe: (t<'a>, t<'a>) => unit => unit
  let destroy: t<'a> => unit
} = {
  type t<'a> = EventEmitter3.t
  let make = EventEmitter3.make
  let emit = (self, x) => EventEmitter3.emit(self, "data", x)->ignore
  let on = (self, callback) => {
    self->EventEmitter3.on("data", callback)->ignore
    () => self->EventEmitter3.removeListener("data", callback)->ignore
  }
  let once = self =>
    Promise.make((resolve, _) => {
      self->EventEmitter3.once("data", resolve)->ignore
    })
  let pipe = (self, other) => {
    self->on(val => other->emit(val))
  }
  let destroy = self => self->EventEmitter3.removeAllListeners->ignore
}

include Module
