// Means of Inter-process communication
type t =
  | ViaPipe(string, array<string>) // command (filepath), args
  | ViaTCP(string, NodeJs.Url.t) // raw URL, parsed URL
