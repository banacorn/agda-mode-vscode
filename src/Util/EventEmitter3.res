// Types

type t

type eventName = string
type listener<'a> = 'a => unit
type listener2<'a, 'b> = ('a, 'b) => unit
type listener3<'a, 'b, 'c> = ('a, 'b, 'c) => unit

// Class: EventEmitter

@bs.module("eventemitter3") @bs.new
external make: unit => t = "EventEmitter"

// Event: 'newListener'

@bs.send.pipe(: t)
external onNewListener: (@bs.as("newListener") _, listener2<eventName, listener<'a>>) => t = "on"

@bs.send.pipe(: t)
external onNewListener2: (@bs.as("newListener") _, listener2<eventName, listener2<'a, 'b>>) => t =
  "on"

@bs.send.pipe(: t)
external onNewListener3: (
  @bs.as("newListener") _,
  listener2<eventName, listener3<'a, 'b, 'c>>,
) => t = "on"

@bs.send.pipe(: t)
external onceNewListener: (@bs.as("newListener") _, listener2<eventName, listener<'a>>) => t =
  "once"

@bs.send.pipe(: t)
external onceNewListener2: (@bs.as("newListener") _, listener2<eventName, listener2<'a, 'b>>) => t =
  "once"

@bs.send.pipe(: t)
external onceNewListener3: (
  @bs.as("newListener") _,
  listener2<eventName, listener3<'a, 'b, 'c>>,
) => t = "once"

// Event: 'removeListener'

@bs.send.pipe(: t)
external onRemoveListener: (@bs.as("removeListener") _, listener<unit>) => t = "on"

@bs.send.pipe(: t)
external onceRemoveListener: (@bs.as("removeListener") _, listener<unit>) => t = "once"

// EventEmitter.defaultMaxListeners

@bs.val @bs.scope("EventEmitter")
external defaultMaxListeners: int = "defaultMaxListeners"

// emitter.addListener(eventName, listener)

@bs.send.pipe(: t)
external addListener: (eventName, listener<'a>) => t = "addListener"

@bs.send.pipe(: t)
external addListener2: (eventName, listener2<'a, 'b>) => t = "addListener"

@bs.send.pipe(: t)
external addListener3: (eventName, listener3<'a, 'b, 'c>) => t = "addListener"

// emitter.emit(eventName[, ...args])

@bs.send.pipe(: t) external emit0: eventName => bool = "emit"

@bs.send.pipe(: t) external emit: (eventName, 'a) => bool = "emit"

@bs.send.pipe(: t) external emit2: (eventName, 'a, 'b) => bool = "emit"

@bs.send.pipe(: t) external emit3: (eventName, 'a, 'b, 'c) => bool = "emit"

// emitter.eventNames()

@bs.send.pipe(: t)
external eventNames: eventName => array<eventName> = "eventNames"

// emitter.getMaxListeners()

@bs.send.pipe(: t) external getMaxListeners: int = "getMaxListeners"

// emitter.listenerCount(eventName)

@bs.send.pipe(: t) external listenerCount: eventName => int = "listenerCount"

// emitter.listeners(eventName)

@bs.send.pipe(: t)
external listeners: eventName => array<listener<'a>> = "listeners"

@bs.send.pipe(: t)
external listeners2: eventName => array<listener2<'a, 'b>> = "listeners"

@bs.send.pipe(: t)
external listeners3: eventName => array<listener3<'a, 'b, 'c>> = "listeners"

// emitter.off(eventName, listener)

@bs.send.pipe(: t) external off: (eventName, listener<'a>) => t = "off"
@bs.send.pipe(: t) external off2: (eventName, listener2<'a, 'b>) => t = "off"
@bs.send.pipe(: t)
external off3: (eventName, listener3<'a, 'b, 'c>) => t = "off"

// emitter.on(eventName, listener)

let on = addListener
let on2 = addListener2
let on3 = addListener3

// emitter.once(eventName, listener)

@bs.send.pipe(: t) external once: (eventName, listener<'a>) => t = "once"

@bs.send.pipe(: t)
external once2: (eventName, listener2<'a, 'b>) => t = "once"

@bs.send.pipe(: t)
external once3: (eventName, listener3<'a, 'b, 'c>) => t = "once"

// emitter.prependListener(eventName, listener)

@bs.send.pipe(: t)
external prependListener: (eventName, listener<'a>) => t = "prependListener"

@bs.send.pipe(: t)
external prependListener2: (eventName, listener2<'a, 'b>) => t = "prependListener"

@bs.send.pipe(: t)
external prependListener3: (eventName, listener3<'a, 'b, 'c>) => t = "prependListener"

// emitter.prependOnceListener(eventName, listener)

@bs.send.pipe(: t)
external prependOnceListener: (eventName, listener<'a>) => t = "prependOnceListener"

@bs.send.pipe(: t)
external prependOnceListener2: (eventName, listener2<'a, 'b>) => t = "prependOnceListener"

@bs.send.pipe(: t)
external prependOnceListener3: (eventName, listener3<'a, 'b, 'c>) => t = "prependOnceListener"

// emitter.removeAllListeners([eventName])

@bs.send.pipe(: t) external removeAllListeners: t = "removeAllListeners"

@bs.send.pipe(: t)
external removeAllListeners_: array<eventName> => t = "removeAllListeners"

// emitter.removeListener(eventName, listener)

let removeListener = off
let removeListener2 = off2
let removeListener3 = off3

// emitter.setMaxListeners(n)

@bs.send.pipe(: t) external setMaxListeners: int => t = "setMaxListeners"

// emitter.rawListeners(eventName)

@bs.send.pipe(: t)
external rawListeners: eventName => array<listener<'a>> = "rawListeners"

@bs.send.pipe(: t)
external rawListeners2: eventName => array<listener2<'a, 'b>> = "rawListeners"

@bs.send.pipe(: t)
external rawListeners3: eventName => array<listener3<'a, 'b, 'c>> = "rawListeners"

// events.once(emitter, name)

@bs.module("eventemitter3")
external oncePromise: (t, string) => Js.Promise.t<'a> = "once"

@bs.module("eventemitter3")
external oncePromise2: (t, string) => Js.Promise.t<('a, 'b)> = "once"

@bs.module("eventemitter3")
external oncePromise3: (t, string) => Js.Promise.t<('a, 'b, 'c)> = "once"
