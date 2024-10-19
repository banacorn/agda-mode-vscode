@bs.module("fs")
external readdir: (string, (Js.nullable<Js.Exn.t>, array<string>) => unit) => unit = "readdir"

@bs.module("fs")
external access: (string, Js.null<Js.Exn.t> => unit) => unit = "access"
@bs.module("fs")
external readFile: (string, (Js.nullable<Js.Exn.t>, NodeJs.Buffer.t) => unit) => unit = "readFile"
@bs.module("fs")
external unlink: (string, Js.nullable<Js.Exn.t> => unit) => unit = "unlink"