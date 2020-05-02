[@bs.module "fs"]
external readdir: (string, (option(Js.Exn.t), array(string)) => unit) => unit =
  "readdir";

[@bs.module "fs"]
external access: (string, Js.null(Js.Exn.t) => unit) => unit = "access";
[@bs.module "fs"]
external readFile: (string, (option(Js.Exn.t), Node.Buffer.t) => unit) => unit =
  "readFile";
[@bs.module "fs"]
external unlink: (string, option(Js.Exn.t) => unit) => unit = "unlink";
