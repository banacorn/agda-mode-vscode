// Means of Inter-process communication
type t =
  | ViaPipe(
      string,
      array<string>,
      option<Connection__Target__ALS__LSP__Binding.executableOptions>,
    ) // command, args, options
  | ViaTCP(NodeJs.Url.t)
