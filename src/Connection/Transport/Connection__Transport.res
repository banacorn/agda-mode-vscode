// Means of Inter-process communication
type t =
  | ViaPipe(string, array<string>) // command (filepath), args
  | ViaTCP(string, URL.t) // raw URL, parsed URL
  | ViaWASM(WASMLoader.t) // WASM setup
