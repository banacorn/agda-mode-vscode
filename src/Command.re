type t =
  | Load
  | Quit
  | ViewResponse(View.Response.t);

let names: array((t, string)) = [|(Load, "load"), (Quit, "quit")|];

let toString =
  fun
  | Load => "Load"
  | Quit => "Quit"
  | ViewResponse(_) => "ViewResponse";