type t =
  | Load
  | Quit
  | NextGoal
  | PreviousGoal
  | ViewResponse(View.Response.t);

let names: array((t, string)) = [|
  (Load, "load"),
  (Quit, "quit"),
  (NextGoal, "nextGoal"),
  (PreviousGoal, "previousGoal"),
|];

let toString =
  fun
  | Load => "Load"
  | Quit => "Quit"
  | NextGoal => "NextGoal"
  | PreviousGoal => "PreviousGoal"
  | ViewResponse(_) => "ViewResponse";