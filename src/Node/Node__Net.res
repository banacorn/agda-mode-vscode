module Socket = {
  type t
  // methods
  @send external destroy: t => t = "destroy"
  // events
  @send
  external on: (t, @string [#error(Js.Exn.t => unit) | #timeout(unit => unit)]) => t = "on"
}

@module("net")
external connect: (int, unit => unit) => Socket.t = "connect"
