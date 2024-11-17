@module("util")
external promisify: (('a, (Js.nullable<Js.Exn.t>, 'b) => unit) => unit, 'a) => promise<'b> =
  "promisify"
