// Means of Inter-process communication
type t =
  | ViaPipe(string, array<string>) // command (filepath), args
  | ViaTCP(NodeJs.Url.t)
