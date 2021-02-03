module Socket = {
  type t
  // methods
  @bs.send external destroy: t => t = "destroy"
  // events
  @bs.send
  external on: (t, @bs.string [#error(Js.Exn.t => unit) | #timeout(unit => unit)]) => t = "on"
}

@bs.module("net")
external connect: (int, unit => unit) => Socket.t = "connect"
