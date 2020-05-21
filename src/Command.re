type t =
  | Load
  | Quit;

let names: array((t, string)) = [|(Load, "load"), (Quit, "quit")|];

let toString =
  fun
  | Load => "Load"
  | Quit => "Quit";